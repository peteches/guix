;; peteches/services/wireguard-socks5.scm
;;
;; Split-tunnel WireGuard, exposed only through a local SOCKS5 proxy.
;;
;; Unlike Guix's own `wireguard-service-type' (which takes peers/keys as
;; literal Scheme values baked into the derivation), this service treats the
;; ENTIRE wg-quick config file as opaque: private key, peer public key,
;; endpoint, tunnel address and the policy-routing PostUp/PreDown commands
;; all live in a single SOPS secret decrypted to a path on disk (normally
;; /run/secrets/wg0.conf via #:sops-secrets). This module never sees any of
;; that content -- only the path.
;;
;; Split-tunnel design: the secret's wg-quick config MUST set `Table = off',
;; so bringing the interface up does NOT touch the system's default route
;; ("does not capture all traffic"). Only processes that explicitly dial the
;; local SOCKS5 proxy (microsocks, run as a dedicated system user) get routed
;; over the tunnel: an nftables `route' hook marks packets owned by that user
;; with FWMARK, and a policy-routing rule -- set up by the secret's own
;; PostUp/PreDown, using the SAME fwmark/table number as this service's
;; default (51820) -- sends FWMARK'd traffic out via the WireGuard interface.
;; See docs/secrets-management.org for the wg0.conf template and the exact
;; PostUp/PreDown lines expected to match FWMARK.
;;
;; Everything else on the VM keeps using its normal default route.
;;
;; Neither shepherd service auto-starts (auto-start? defaults to #f): bring
;; the tunnel up on demand with `herd start wireguard-wg0' then
;; `herd start socks5-proxy', and tear it down with `herd stop socks5-proxy'
;; then `herd stop wireguard-wg0' when done.

(define-module (peteches services wireguard-socks5)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)   ;shadow (nologin)
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
  ;; Dedicated system user microsocks runs as; also the nftables `meta skuid'
  ;; match, so packets it originates get FWMARK'd for policy routing.
  (socks-user        wsc-socks-user (default "socks5"))
  (socks-bind        wsc-socks-bind (default "127.0.0.1"))
  (socks-port        wsc-socks-port (default 1080))
  ;; fwmark AND routing-table id used by the mark rule below. The secret's
  ;; wg-quick PostUp/PreDown must use this exact same number for its
  ;; `ip rule add fwmark ... table ...' / `ip route add default dev %i
  ;; table ...' lines.
  (fwmark            wsc-fwmark (default 51820))
  (log-file          wsc-log-file (default "/var/log/microsocks.log"))
  ;; Off by default: the tunnel and proxy do NOT come up on boot or on
  ;; `herd reload' -- start them explicitly with `herd start wireguard-wg0'
  ;; and `herd start socks5-proxy' when you actually want the tunnel, and
  ;; `herd stop' the two (proxy first) when done. Set to #t only if this VM
  ;; should always route via SOCKS5 automatically.
  (auto-start?       wsc-auto-start? (default #f)))

(define %path-env
  "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")

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

;; Matches sops-guix's own (sops-secret->shepherd-service-name), which is
;; "sops-secret-" ++ (key->file-name key) -- for a single-string key list
;; (our case) that's just "sops-secret-" ++ the key string verbatim.
(define (wsc-sops-secret-service-name config)
  (symbol-append 'sops-secret- (string->symbol (wsc-sops-key config))))

(define (wsc-accounts config)
  (let ((user (wsc-socks-user config)))
    (list (user-group (name user) (system? #t))
          (user-account
            (name user)
            (group user)
            (system? #t)
            (comment "SOCKS5 proxy egressing over WireGuard")
            (home-directory "/var/empty")
            (shell (file-append shadow "/sbin/nologin"))))))

(define (wsc-wireguard-shepherd-service config)
  (let* ((wg-quick (file-append (wsc-wireguard-package config) "/bin/wg-quick"))
         (config-file (wsc-config-file config)))
    (shepherd-service
     (provision (list (wsc-wireguard-service-name config)))
     (requirement (list 'user-processes 'networking
                        (wsc-sops-secret-service-name config)))
     (documentation
      (string-append "WireGuard tunnel " (wsc-interface config)
                     " (split-tunnel; entire config decrypted from SOPS)"))
     (start #~(lambda _
                (setenv "PATH" #$(wsc-wg-quick-path-env config))
                (zero? (system* #$wg-quick "up" #$config-file))))
     ;; wg-quick runs under `set -e -o pipefail' and executes PreDown
     ;; hooks BEFORE actually deleting the interface (cmd_down): if a
     ;; PreDown command fails (e.g. removing a policy-routing rule/route
     ;; that, for whatever reason, doesn't exist), the script aborts right
     ;; there and the interface is never deleted. Reporting success
     ;; unconditionally here would let the interface leak while shepherd
     ;; believes it's stopped -- the next `start' then fails outright,
     ;; since wg-quick refuses to create an interface that already exists.
     ;; Reflect the real exit status instead: #f only if `wg-quick down'
     ;; actually succeeded.
     (stop #~(lambda _
               (setenv "PATH" #$(wsc-wg-quick-path-env config))
               (not (zero? (system* #$wg-quick "down" #$config-file)))))
     (auto-start? (wsc-auto-start? config))
     (respawn? #f))))

(define (wsc-socks-shepherd-service config)
  (let* ((microsocks (file-append (wsc-socks-package config) "/bin/microsocks"))
         (bind (wsc-socks-bind config))
         (port (number->string (wsc-socks-port config))))
    (shepherd-service
     (provision '(socks5-proxy))
     (requirement (list 'user-processes (wsc-wireguard-service-name config)))
     (documentation
      "microsocks SOCKS5 proxy; traffic it originates is fwmark-routed over WireGuard.")
     ;; No -w: it whitelists IPs to BYPASS -u/-P auth, so on its own
     ;; (without -u/-P at all) microsocks rejects it as invalid usage
     ;; ("-1/-w options must be used together with user/pass") and exits 1.
     ;; Access here is already restricted by BIND (127.0.0.1) alone, and
     ;; with no -u/-P microsocks runs unauthenticated for anything that can
     ;; reach that address, which is exactly what's wanted.
     (start #~(make-forkexec-constructor
               (list #$microsocks "-i" #$bind "-p" #$port)
               #:user #$(wsc-socks-user config)
               #:group #$(wsc-socks-user config)
               #:log-file #$(wsc-log-file config)
               #:environment-variables (list #$%path-env)))
     (stop #~(make-kill-destructor))
     (auto-start? (wsc-auto-start? config)))))

(define (wsc-shepherd-services config)
  (list (wsc-wireguard-shepherd-service config)
        (wsc-socks-shepherd-service config)))

(define (wsc-firewall-rules config)
  (let ((user (wsc-socks-user config))
        (mark (number->string (wsc-fwmark config))))
    (nftables-rules
     (raw (list (string-append
                 "table inet wireguard-socks5-mark {\n"
                 "  chain output {\n"
                 "    type route hook output priority mangle; policy accept;\n"
                 "    meta skuid \"" user "\" meta mark set " mark "\n"
                 "  }\n"
                 "}\n"))))))

(define (wsc-profile config)
  (list (wsc-wireguard-package config) (wsc-socks-package config)))

(define-public wireguard-socks5-service-type
  (service-type (name 'wireguard-socks5)
                (description
                 "Split-tunnel WireGuard whose entire config comes from a
SOPS secret, reachable only via a local SOCKS5 proxy (microsocks) -- the
system's default route is left untouched.")
                (extensions
                 (list (service-extension account-service-type
                                          wsc-accounts)
                       (service-extension shepherd-root-service-type
                                          wsc-shepherd-services)
                       (service-extension firewall-service-type
                                          wsc-firewall-rules)
                       (service-extension profile-service-type
                                          wsc-profile)))))
