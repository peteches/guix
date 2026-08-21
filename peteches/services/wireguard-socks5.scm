;; peteches/services/wireguard-socks5.scm
;;
;; Split-tunnel WireGuard, exposed only through a local SOCKS5 proxy -- both
;; confined to their OWN network namespace with their own independent DNS
;; resolver.
;;
;; Unlike Guix's own `wireguard-service-type' (which takes peers/keys as
;; literal Scheme values baked into the derivation), this service treats the
;; ENTIRE wg-quick config file as opaque: private key, peer public key,
;; endpoint, tunnel address all live in a single SOPS secret decrypted to a
;; path on disk (normally /run/secrets/wg0.conf via #:sops-secrets). This
;; module never sees any of that content -- only the path.
;;
;; Isolation design: `wg0' is created inside a dedicated network namespace
;; (default "wg0ns"), connected to the host only by a veth pair. Because
;; nothing else lives in that namespace, an ordinary wg-quick default route
;; (from `AllowedIPs = 0.0.0.0/0' in the secret's [Peer] section) already
;; sends ALL of the namespace's traffic over the tunnel -- no `Table = off',
;; fwmark, or policy-routing PostUp/PreDown commands are needed the way the
;; previous (single-netns, nftables-skuid-mark) design required. `microsocks'
;; also runs inside that namespace, bound to the netns side of the veth, so
;; only processes that explicitly dial it (from the host, over the veth) get
;; routed over the tunnel; the host's own default route and DNS resolver
;; (e.g. Tailscale's MagicDNS at 100.100.100.100) are completely untouched.
;;
;; The namespace's ONLY connection to anywhere is that veth pair, but the
;; WireGuard kernel module's own encrypted UDP packets (the handshake and
;; every subsequent packet to the peer's real endpoint) still need a real
;; path to the actual internet -- wg0 can't bootstrap its own tunnel through
;; itself. So wsc-netns-shepherd-service also: (1) turns the host into a NAT
;; gateway for the netns (net.ipv4.ip_forward=1, enabled directly by the
;; start procedure, plus a masquerade rule and forward-chain accepts added
;; via firewall-service-type -- see wsc-firewall-rules), and (2) seeds a
;; plain default route inside the namespace via the veth's host-side address
;; BEFORE wg-quick ever runs, so wg-quick's own peer traffic has somewhere
;; to go. Once wg0 comes up, its own default route (from AllowedIPs) takes
;; over for everything else in the namespace; the veth-gateway route is only
;; ever used by the tunnel's own handshake/keepalive traffic.
;;
;; Independent DNS: Linux bind-mounts /etc/netns/<name>/resolv.conf over
;; /etc/resolv.conf for anything run via `ip netns exec <name> ...' (that's
;; what makes it "independent" -- it's a kernel/iproute2 mechanism, not
;; something this module has to fake). This service seeds that file with
;; ONLY openresolv's own signature line (see %openresolv-signature-line
;; below) before wg-quick ever runs, which is enough to make an ordinary
;; `DNS = ...' line in the wg0-conf secret work exactly as wg-quick
;; documents it -- `resolvconf -a' regenerates the namespace's private
;; resolv.conf from that DNS server, reached over the tunnel by the
;; namespace's own default route, never touching the host's resolver.
;; DNS configuration itself lives entirely in the secret, same as every
;; other tunnel parameter. See docs/secrets-management.org for the current
;; wg0.conf template.
;;
;; Everything else on the VM keeps using its normal default route and DNS.
;;
;; None of the three shepherd services auto-start (auto-start? defaults to
;; #f). Bring the tunnel up on demand, innermost-last -- note the netns
;; service is named after NETNS, i.e. "netns-wg0ns" with the defaults, not
;; "netns-wg0":
;;   herd start netns-wg0ns
;;   herd start wireguard-wg0
;;   herd start socks5-proxy
;; and tear it down outermost-first:
;;   herd stop socks5-proxy
;;   herd stop wireguard-wg0
;;   herd stop netns-wg0ns
;; If `netns-wg0ns' fails partway through (e.g. veth creation fails after the
;; namespace itself was created), it can leave the namespace or a host-side
;; veth behind; clean up by hand with `ip netns delete wg0ns' and/or
;; `ip link delete veth-wg0h' before retrying.

(define-module (peteches services wireguard-socks5)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)   ;shadow (nologin)
  #:use-module (gnu packages linux)   ;iproute
  #:use-module (gnu packages skarnet) ;s6 (s6-setuidgid)
  #:use-module (gnu packages vpn)     ;wireguard-tools
  #:use-module (gnu packages web)     ;microsocks
  #:use-module (peteches services firewall)
  #:export (wireguard-socks5-configuration
            make-wireguard-socks5-configuration
            wireguard-socks5-configuration?
            wireguard-socks5-service-type))

(define-record-type* <wireguard-socks5-configuration>
  wireguard-socks5-configuration make-wireguard-socks5-configuration
  wireguard-socks5-configuration?
  ;; WireGuard interface name; must match the basename wg-quick derives from
  ;; CONFIG-FILE's decrypted content being named accordingly (wg-quick names
  ;; the interface after the config file's basename -- see #:path on the
  ;; sops-secret in claude-workstation.scm, which is why it MUST be
  ;; "/run/secrets/wg0.conf" when interface is "wg0").
  (interface        wsc-interface (default "wg0"))
  ;; Path to the decrypted wg-quick config file (a sops-secret's #:path).
  (config-file       wsc-config-file)
  ;; The wg-quick secret's sops-secret KEY (a single string, e.g. "wg0-conf"
  ;; -- must match the (key '("...")) used for it in claude-workstation.scm).
  ;; Used ONLY to compute the exact per-secret shepherd service
  ;; ("sops-secret-<key>") this tunnel waits on, instead of the generic
  ;; `sops-secrets' aggregate -- which only reports started once EVERY
  ;; secret on the machine (tailscale auth-key, criticalgrind's Plane/
  ;; Outline keys, ...) has decrypted. Depending on the aggregate meant an
  ;; unrelated secret failing (e.g. tailscale's) blocked the tunnel even
  ;; though its own wg0-conf secret had already decrypted fine.
  (sops-key          wsc-sops-key (default "wg0-conf"))
  (wireguard         wsc-wireguard-package (default wireguard-tools))
  (socks-package     wsc-socks-package (default microsocks))
  ;; Dedicated system user microsocks runs as inside the namespace (dropped
  ;; to via s6-setuidgid, since entering the namespace itself requires root
  ;; -- see wsc-socks-shepherd-service).
  (socks-user        wsc-socks-user (default "socks5"))
  (socks-port        wsc-socks-port (default 1080))
  ;; Name of the dedicated network namespace housing wg0 and microsocks.
  (netns             wsc-netns (default "wg0ns"))
  ;; veth pair connecting the host to the namespace. Host-side name/address
  ;; live in the host's (default) namespace; netns-side live inside NETNS.
  ;; Interface names must stay <=15 chars (kernel IFNAMSIZ limit).
  (veth-host         wsc-veth-host (default "veth-wg0h"))
  (veth-netns        wsc-veth-netns (default "veth-wg0n"))
  (veth-host-address  wsc-veth-host-address (default "10.200.0.1/30"))
  (veth-netns-address wsc-veth-netns-address (default "10.200.0.2/30"))
  (socks-log-file    wsc-socks-log-file (default "/var/log/microsocks.log"))
  ;; wg-quick's start/stop and the netns/veth setup/teardown all run as
  ;; inline lambdas (not make-forkexec-constructor), since each is a
  ;; one-shot sequence of commands that exits immediately -- there's no
  ;; long-running process for shepherd to track), so there's no #:log-file
  ;; keyword to hand them. Their output is instead captured by redirecting
  ;; Guile's current output/error ports (which system*'s children inherit)
  ;; for the duration of the call -- see wsc-logged. `herd status -n' only
  ;; shows a transient in-memory buffer that can miss messages; these files
  ;; persist across restarts.
  (wg-log-file       wsc-wg-log-file (default "/var/log/wireguard-wg0.log"))
  (netns-log-file    wsc-netns-log-file (default "/var/log/wireguard-netns.log"))
  ;; Off by default: the namespace, tunnel and proxy do NOT come up on boot
  ;; or on `herd reload' -- start them explicitly (see the module
  ;; docstring for the three-step order) when the tunnel is actually
  ;; wanted, and `herd stop' them in reverse when done. Set to #t only if
  ;; this VM should always route via SOCKS5 automatically.
  (auto-start?       wsc-auto-start? (default #f)))

(define %path-env
  "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")

;; openresolv (bundled with wireguard-tools, and unconditionally on
;; wg-quick's own hardcoded PATH regardless of namespace) refuses to touch
;; /etc/resolv.conf unless its FIRST LINE already reads exactly this --
;; otherwise it assumes the file is externally managed (e.g. Tailscale's
;; MagicDNS stamp on the host) and aborts wg-quick under `set -e' with
;; "signature mismatch: ...". Seeding the namespace's private resolv.conf
;; with just this line before wg-quick ever runs makes an ordinary
;; `DNS = ...' line in the wg0-conf secret work as documented: `resolvconf
;; -a' (called by wg-quick's PostUp) sees a file it already "owns" and
;; regenerates it with the real nameserver(s) -- see
;; lib/resolvconf/libc in the openresolv package for the exact check.
;; NOTE: this is openresolv's own internal marker, not a documented public
;; contract -- if a future openresolv release changes it, this silently
;; stops working and DNS= fails again exactly like the original Tailscale
;; collision, just against this file instead.
(define %openresolv-signature-line "# Generated by resolvconf\n")

;; wg-quick's Guix wrapper hardcodes absolute paths for ip/iptables/
;; procps/openresolv/coreutils, but internally still calls bare `wg' (for
;; `wg set'/`addconf'/`showconf') relying on inherited PATH. The start/stop
;; procedures below run as an inline lambda directly under shepherd's own
;; (minimal) process environment rather than via make-forkexec-constructor,
;; so unlike every other service in this repo PATH is never otherwise set
;; here -- without this, `wg-quick up' fails because `wg' can't be found.
;; Returns a gexp evaluating to the PATH string (not a plain string: it
;; references a package's store output, which only resolves inside a gexp).
(define (wsc-wg-quick-path-env config)
  #~(string-append #$(file-append (wsc-wireguard-package config) "/bin")
                    ":/run/setuid-programs:/run/current-system/profile/bin:"
                    "/run/current-system/profile/sbin"))

(define (wsc-wireguard-service-name config)
  (symbol-append 'wireguard- (string->symbol (wsc-interface config))))

(define (wsc-netns-service-name config)
  (symbol-append 'netns- (string->symbol (wsc-netns config))))

;; BODY is a gexp for a thunk (a `(lambda () ...)' expression, unevaluated).
;; Returns a gexp that opens LOG-FILE (creating/appending), redirects
;; current-output-port/current-error-port to it for BODY's dynamic extent
;; -- so system*'s forked children, which inherit Guile's current ports,
;; land their stdout/stderr there too -- then closes it again.
(define (wsc-logged log-file body)
  #~(let ((log (open-file #$log-file "a")))
      (dynamic-wind
       (lambda () #t)
       (lambda ()
         (with-output-to-port log
           (lambda () (with-error-to-port log #$body))))
       (lambda () (close-port log)))))

;; Matches sops-guix's own (sops-secret->shepherd-service-name), which is
;; "sops-secret-" ++ (key->file-name key) -- for a single-string key list
;; (our case) that's just "sops-secret-" ++ the key string verbatim.
(define (wsc-sops-secret-service-name config)
  (symbol-append 'sops-secret- (string->symbol (wsc-sops-key config))))

;; Strips the /NN prefix length off an "ip addr"-style CIDR string, e.g.
;; "10.200.0.2/30" -> "10.200.0.2" -- used as the bind address for
;; microsocks inside the namespace (the netns side of the veth), so the
;; proxy is reachable from the host but NOT from the WireGuard peer (which
;; can otherwise reach anything the namespace owns, since AllowedIPs is
;; 0.0.0.0/0).
(define (wsc-veth-netns-ip config)
  (car (string-split (wsc-veth-netns-address config) #\/)))

;; Same idea, for the host side -- this is the gateway address the
;; namespace routes its own (non-tunnel) traffic through, i.e. the
;; WireGuard kernel module's own handshake/keepalive packets to the peer's
;; real endpoint. See wsc-netns-shepherd-service and wsc-firewall-rules.
(define (wsc-veth-host-ip config)
  (car (string-split (wsc-veth-host-address config) #\/)))

;; NAT-gateway rules for the host: masquerade traffic sourced from the
;; namespace's veth address as it leaves via the host's real interface, and
;; explicitly allow it through the forward chain (the base firewall's
;; forward chain has policy drop with no rules of its own). Scoped tightly
;; to the veth's own fixed address, not a wider subnet -- see
;; wsc-veth-netns-ip. This complements, but doesn't replace,
;; net.ipv4.ip_forward=1, which wsc-netns-shepherd-service's start
;; procedure sets directly (a single global on/off switch, not something
;; nftables controls).
(define (wsc-firewall-rules config)
  (let ((netns-ip (wsc-veth-netns-ip config))
        (veth-host (wsc-veth-host config)))
    (nftables-rules
     (nat-postrouting
      (list (string-append "ip saddr " netns-ip " masquerade comment \"wg0ns egress\"")))
     (forward
      (list (string-append "iifname \"" veth-host "\" ip saddr " netns-ip
                            " accept comment \"wg0ns egress\"")
            (string-append "oifname \"" veth-host
                            "\" ct state { established, related } accept"
                            " comment \"wg0ns return traffic\""))))))

(define (wsc-accounts config)
  (let ((user (wsc-socks-user config)))
    (list (user-group (name user) (system? #t))
          (user-account
            (name user)
            (group user)
            (system? #t)
            (comment "SOCKS5 proxy egressing over WireGuard (netns-isolated)")
            (home-directory "/var/empty")
            (shell (file-append shadow "/sbin/nologin"))))))

;; Creates the dedicated network namespace, its veth link to the host, and
;; its independent /etc/netns/<name>/resolv.conf (seeded with just
;; openresolv's signature line -- see %openresolv-signature-line -- so
;; `DNS = ...' in the secret can populate the rest). Nothing else lives in
;; this namespace, so once wg0 goes up inside it (wsc-wireguard-shepherd-
;; service) a plain wg-quick default route already sends everything out
;; the tunnel -- no fwmark or policy routing required.
(define (wsc-netns-shepherd-service config)
  (let* ((netns (wsc-netns config))
         (veth-host (wsc-veth-host config))
         (veth-netns (wsc-veth-netns config))
         (host-address (wsc-veth-host-address config))
         (netns-address (wsc-veth-netns-address config))
         (host-ip (wsc-veth-host-ip config))
         (ip (file-append iproute "/sbin/ip")))
    (shepherd-service
     (provision (list (wsc-netns-service-name config)))
     (requirement '(user-processes networking))
     (documentation
      (string-append "Network namespace \"" netns
                     "\" housing the WireGuard interface and SOCKS5 proxy, "
                     "connected to the host only by a veth pair, with its "
                     "own independent DNS resolver."))
     (start
      #~(lambda _
          #$(wsc-logged
             (wsc-netns-log-file config)
             #~(lambda ()
                 (let ((dir (string-append "/etc/netns/" #$netns)))
                   (catch 'system-error
                     (lambda () (mkdir "/etc/netns"))
                     (lambda args (unless (file-exists? "/etc/netns") (apply throw args))))
                   (catch 'system-error
                     (lambda () (mkdir dir))
                     (lambda args (unless (file-exists? dir) (apply throw args))))
                   (call-with-output-file (string-append dir "/resolv.conf")
                     (lambda (port) (display #$%openresolv-signature-line port))))
                 ;; A single global switch (not per-namespace, not nftables) --
                 ;; without it the kernel never forwards packets between the
                 ;; veth and the host's real interface at all, no matter what
                 ;; NAT/forward rules exist.
                 (call-with-output-file "/proc/sys/net/ipv4/ip_forward"
                   (lambda (port) (display "1" port)))
                 (and (zero? (system* #$ip "netns" "add" #$netns))
                      (zero? (system* #$ip "link" "add" #$veth-host
                                       "type" "veth" "peer" "name" #$veth-netns))
                      (zero? (system* #$ip "link" "set" #$veth-netns "netns" #$netns))
                      (zero? (system* #$ip "addr" "add" #$host-address "dev" #$veth-host))
                      (zero? (system* #$ip "link" "set" #$veth-host "up"))
                      (zero? (system* #$ip "netns" "exec" #$netns
                                       #$ip "addr" "add" #$netns-address "dev" #$veth-netns))
                      (zero? (system* #$ip "netns" "exec" #$netns
                                       #$ip "link" "set" #$veth-netns "up"))
                      (zero? (system* #$ip "netns" "exec" #$netns
                                       #$ip "link" "set" "lo" "up"))
                      ;; wg-quick's own default route (installed once wg0
                      ;; comes up, via AllowedIPs) covers everything else in
                      ;; the namespace -- this one exists solely so the
                      ;; WireGuard kernel module's OWN traffic to the peer's
                      ;; real endpoint has a way out before that happens.
                      (zero? (system* #$ip "netns" "exec" #$netns
                                       #$ip "route" "add" "default"
                                       "via" #$host-ip "dev" #$veth-netns)))))))
     ;; Deleting the host-side veth also removes its (still-attached) peer
     ;; if it never made it into the namespace; deleting the namespace tears
     ;; down wg0 and anything else still running inside it. Reflect the
     ;; real exit status of the namespace deletion, same reasoning as
     ;; wg-quick's stop below: report success only if it actually happened.
     (stop
      #~(lambda _
          #$(wsc-logged
             (wsc-netns-log-file config)
             #~(lambda ()
                 (system* #$ip "link" "delete" #$veth-host)
                 (not (zero? (system* #$ip "netns" "delete" #$netns)))))))
     (auto-start? (wsc-auto-start? config))
     (respawn? #f))))

(define (wsc-wireguard-shepherd-service config)
  (let* ((wg-quick (file-append (wsc-wireguard-package config) "/bin/wg-quick"))
         (config-file (wsc-config-file config))
         (netns (wsc-netns config))
         (ip (file-append iproute "/sbin/ip")))
    (shepherd-service
     (provision (list (wsc-wireguard-service-name config)))
     (requirement (list 'user-processes
                        (wsc-netns-service-name config)
                        (wsc-sops-secret-service-name config)))
     (documentation
      (string-append "WireGuard tunnel " (wsc-interface config)
                     " inside network namespace \"" netns "\""
                     " (entire config decrypted from SOPS)"))
     (start #~(lambda _
                #$(wsc-logged
                   (wsc-wg-log-file config)
                   #~(lambda ()
                       (setenv "PATH" #$(wsc-wg-quick-path-env config))
                       (zero? (system* #$ip "netns" "exec" #$netns
                                        #$wg-quick "up" #$config-file))))))
     ;; wg-quick runs under `set -e -o pipefail' and executes PreDown
     ;; hooks BEFORE actually deleting the interface (cmd_down): if a
     ;; PreDown command fails, the script aborts right there and the
     ;; interface is never deleted. Reporting success unconditionally here
     ;; would let the interface leak while shepherd believes it's stopped
     ;; -- the next `start' then fails outright, since wg-quick refuses to
     ;; create an interface that already exists. Reflect the real exit
     ;; status instead: #f only if `wg-quick down' actually succeeded.
     (stop #~(lambda _
               #$(wsc-logged
                  (wsc-wg-log-file config)
                  #~(lambda ()
                      (setenv "PATH" #$(wsc-wg-quick-path-env config))
                      (not (zero? (system* #$ip "netns" "exec" #$netns
                                             #$wg-quick "down" #$config-file)))))))
     (auto-start? (wsc-auto-start? config))
     (respawn? #f))))

(define (wsc-socks-shepherd-service config)
  (let* ((microsocks (file-append (wsc-socks-package config) "/bin/microsocks"))
         (netns (wsc-netns config))
         (bind (wsc-veth-netns-ip config))
         (port (number->string (wsc-socks-port config)))
         (ip (file-append iproute "/sbin/ip"))
         (setuidgid (file-append s6 "/bin/s6-setuidgid"))
         (user (wsc-socks-user config))
         (resolv-conf (string-append "/etc/netns/" netns "/resolv.conf")))
    (shepherd-service
     (provision '(socks5-proxy))
     (requirement (list 'user-processes (wsc-wireguard-service-name config)))
     (documentation
      (string-append "microsocks SOCKS5 proxy, running inside network "
                     "namespace \"" netns "\" -- all its traffic, including "
                     "its own DNS lookups, is confined to the tunnel."))
     ;; `ip netns exec' must run as root (entering a namespace needs
     ;; CAP_SYS_ADMIN), so privilege can only be dropped to socks-user AFTER
     ;; that -- s6-setuidgid does exactly that hand-off, execing microsocks
     ;; in place once it has switched uid/gid/groups. No -w: it whitelists
     ;; IPs to BYPASS -u/-P auth, so on its own (without -u/-P at all)
     ;; microsocks rejects it as invalid usage and exits 1. Access here is
     ;; already restricted to whatever can reach BIND over the veth alone,
     ;; and with no -u/-P microsocks runs unauthenticated for anything that
     ;; can, which is exactly what's wanted.
     (start
      #~(let ((forkexec
               (make-forkexec-constructor
                (list #$ip "netns" "exec" #$netns
                      #$setuidgid #$user
                      #$microsocks "-i" #$bind "-p" #$port)
                #:log-file #$(wsc-socks-log-file config)
                #:environment-variables (list #$%path-env))))
          (lambda args
            #$(wsc-logged
               (wsc-socks-log-file config)
               #~(lambda ()
                   ;; wireguard-wg0's PostUp (`resolvconf -a', driven by the
                   ;; secret's `DNS = ...' line) is what actually populates
                   ;; this file with a real nameserver; wsc-netns-shepherd-
                   ;; service's start only ever seeds it with openresolv's
                   ;; bare signature line (see %openresolv-signature-line).
                   ;; requirement above only proves wireguard-wg0's shepherd
                   ;; service reported started, not that its PostUp hook
                   ;; has finished -- so there's a real window, on a fast
                   ;; full-stack restart, where microsocks would otherwise
                   ;; exec into a namespace whose resolv.conf is still just
                   ;; that signature line. With proxy_dns on, that makes
                   ;; every proxychains-routed lookup resolve through a
                   ;; resolver that doesn't exist -- which used to fail
                   ;; silently (a ~20s connect timeout per lookup, no log
                   ;; line anywhere pointing at the cause). Poll for up to
                   ;; 15s (PostUp normally finishes in well under 1s) and
                   ;; only refuse to launch, loudly, if it never shows up --
                   ;; that's a real problem (netns-wg0ns/wireguard-wg0 not
                   ;; actually up), not just a slow PostUp.
                   (define (nameserver-ready?)
                     (and (file-exists? #$resolv-conf)
                          (call-with-input-file #$resolv-conf
                            (lambda (port)
                              (let loop ()
                                (let ((line (read-line port)))
                                  (cond ((eof-object? line) #f)
                                        ((string-prefix? "nameserver" line) #t)
                                        (else (loop)))))))))
                   (let retry ((attempts-left 15))
                     (cond
                      ((nameserver-ready?) (apply forkexec args))
                      ((> attempts-left 0)
                       (sleep 1)
                       (retry (- attempts-left 1)))
                      (else
                       (format (current-error-port)
                               "socks5-proxy: refusing to start -- ~a still \
has no nameserver line after 15s (wireguard-wg0's PostUp hasn't finished \
repopulating DNS); check netns-wg0ns/wireguard-wg0 status~%"
                               #$resolv-conf)
                       #f))))))))
     (stop #~(make-kill-destructor))
     (auto-start? (wsc-auto-start? config)))))

(define (wsc-shepherd-services config)
  (list (wsc-netns-shepherd-service config)
        (wsc-wireguard-shepherd-service config)
        (wsc-socks-shepherd-service config)))

(define (wsc-profile config)
  (list (wsc-wireguard-package config) (wsc-socks-package config) iproute s6))

(define-public wireguard-socks5-service-type
  (service-type (name 'wireguard-socks5)
                (description
                 "Split-tunnel WireGuard whose entire config comes from a
SOPS secret, isolated in its own network namespace along with a local
SOCKS5 proxy (microsocks) -- the system's default route and DNS resolver
are left untouched.")
                (extensions
                 (list (service-extension account-service-type
                                          wsc-accounts)
                       (service-extension shepherd-root-service-type
                                          wsc-shepherd-services)
                       (service-extension profile-service-type
                                          wsc-profile)
                       (service-extension firewall-service-type
                                          wsc-firewall-rules)))))
