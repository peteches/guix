;; (peteches system-services tailscale)
;; Multi-instance Tailscale for Guix System.
;;
;; Features:
;; - Each instance runs in its own network namespace (netns)
;; - Each netns gets outbound connectivity via veth + NAT
;; - NAT/forwarding via nftables (not iptables)
;; - Adds helper scripts ts-<name> to your system profile
;; - Arbitrary number of instances supported (compose/extend append)
;;
;; Bring up each tailnet:
;;   sudo tailscale --socket=/run/tailscale/work/tailscaled.sock up --accept-dns --accept-routes
;;   sudo tailscale --socket=/run/tailscale/home/tailscaled.sock up --accept-dns --accept-routes
;;
;; Use helpers:
;;   ts-work ssh host.work.ts.net
;;   ts-home ssh host.home.ts.net
;;
(define-module (peteches system-services tailscale)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)   ; etc-service-type
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)     ; iproute + nftables
  #:use-module (gnu packages bash)      ; bash for wrapper scripts
  #:use-module (gnu packages web)
  #:use-module (gnu packages base)     ; coreutils
  #:use-module (peteches packages tailscale)
  #:use-module (peteches system-services firewall)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (guix licenses)
  #:export (tailscale-instance-configuration
            tailscale-instance-configuration?
            tailscale-service-type))

;; Linux interface names must be <= 15 chars.
;; We keep the original behavior for short names, but if truncation is needed,
;; we add a short stable hash suffix to reduce collision risk.
(define (ifname prefix name)
  (let* ((raw (string-append prefix name))
         (maxlen 15))
    (if (<= (string-length raw) maxlen)
        raw
        (let* ((h (abs (string-hash raw)))
               (sfx (format #f "~4,'0x" (modulo h #x10000))) ; 4 hex chars
               ;; Keep room for "-" + 4 chars
               (keep (- maxlen 5))
               (base (string-take raw (max 0 keep))))
          (string-append base "-" sfx)))))

;; Stable per-name /24: 10.250.X.0/24
(define (name->octet name)
  (let* ((h (abs (string-hash name)))
         (x (+ 10 (modulo h 200)))) ; 10..209
    x))

(define (subnet-for name)
  (let ((x (name->octet name)))
    (values (format #f "10.250.~a.0/24" x)
            (format #f "10.250.~a.1/24" x)  ; host veth addr
            (format #f "10.250.~a.2/24" x)  ; netns veth addr
            (format #f "10.250.~a.1" x))))  ; gw inside netns

(define-record-type* <tailscale-instance-configuration>
  tailscale-instance-configuration make-tailscale-instance-configuration
  tailscale-instance-configuration?
  (name       tailscale-instance-configuration-name
              (default "default"))
  (package    tailscale-instance-configuration-package
              (default tailscale))

  ;; netns name (defaults to "ts-" + name)
  (netns      tailscale-instance-configuration-netns
              (default #f))

  ;; if added will be used as the search option in resolv.conf
  (magic-dns-suffix tailscale-instance-configuration-magic-dns-suffix
		    (default #f))

  ;; optional overrides
  (state-file  tailscale-instance-configuration-state-file
               (default #f))
  (socket-file tailscale-instance-configuration-socket-file
               (default #f))
  (tun         tailscale-instance-configuration-tun
               (default #f))
  (port        tailscale-instance-configuration-port
               (default #f))
  (log-file    tailscale-instance-configuration-log-file
               (default #f))
  (extra-args  tailscale-instance-configuration-extra-args
               (default '()))

  ;; Forward ports from inside the netns back to the host.
  ;; Each element is either (SRC . DST) or (SRC DST), ports are integers.
  (forward-ports tailscale-instance-configuration-forward-ports
                 (default '()))

  ;; Forward ports from the host into the netns (reverse of forward-ports).
  ;; Each element is (SRC . DST) or (SRC DST), ports are integers.
  (host-forward-ports tailscale-instance-configuration-host-forward-ports
                      (default '()))

  ;; Optional SOCKS5 proxy port.  When set (integer), microsocks is started
  ;; inside the netns bound to 0.0.0.0:<port>, whitelisting the host veth IP.
  ;; Host applications connect via SOCKS5 at 10.250.X.2:<port>.
  (socks-proxy-port tailscale-instance-configuration-socks-proxy-port
                    (default #f))

  ;; Taildrop receive: set taildrop-dir to enable the file-get polling service.
  ;; Files sent via `tailscale file cp` are retrieved into taildrop-dir.
  ;; taildrop-user (if set) is used to chown all files after each retrieval.
  (taildrop-dir      tailscale-instance-configuration-taildrop-dir
                     (default #f))
  (taildrop-user     tailscale-instance-configuration-taildrop-user
                     (default #f))
  (taildrop-schedule tailscale-instance-configuration-taildrop-schedule
                     (default "*/1 * * * *")))

(define (instance-default-paths name)
  (let ((base (string-append "/var/lib/tailscale/" name))
        (run  (string-append "/run/tailscale/" name)))
    (values (string-append base "/tailscaled.state")
            (string-append run  "/tailscaled.sock")
            (string-append "/var/log/tailscaled-" name ".log")
            ;; default tun name, respecting IFNAMSIZ
            (ifname "ts-" name))))

(define (instance->resolved cfg)
  ;; Fill in defaults derived from name if fields are #f.
  (match cfg
    (($ <tailscale-instance-configuration>
        name package netns magic-dns state-file socket-file tun port log-file extra-args forward-ports
        host-forward-ports socks-proxy-port taildrop-dir taildrop-user taildrop-schedule)
     (let-values (((dstate dsock dlog dtun) (instance-default-paths name)))
       (tailscale-instance-configuration
        (name name)
        (package package)
        (netns (or netns (string-append "ts-" name)))
        (magic-dns-suffix magic-dns)
        (state-file (or state-file dstate))
        (socket-file (or socket-file dsock))
        (tun (or tun dtun))
        (port port)
        (log-file (or log-file dlog))
        (extra-args extra-args)
        (forward-ports forward-ports)
        (host-forward-ports host-forward-ports)
        (socks-proxy-port socks-proxy-port)
        (taildrop-dir taildrop-dir)
        (taildrop-user taildrop-user)
        (taildrop-schedule taildrop-schedule))))))

(define (tailscale-instance->shepherd-service cfg)
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package netns magic-dns state-file socket-file tun port log-file extra-args forward-ports _ _ _ _ _)
       (let* ((service-name (string->symbol (string-append "tailscaled-" name)))
              (setup-name   (string->symbol (string-append "tailscale-netns-setup-" name)))
              (ip (file-append iproute "/sbin/ip"))
              (run-args
               (append (list (file-append package "/bin/tailscaled")
                             (string-append "--statedir=/var/lib/tailscale/" name)
                             (string-append "--socket=" socket-file)
                             (string-append "--tun=" tun))
                       (if port
                           (list (string-append "--port=" (number->string port)))
                           '())
                       extra-args)))
         (shepherd-service
          (provision (list service-name))
	  (auto-start? #t)
	  (one-shot? #f)
          (documentation (string-append "Tailscale (" name ") in netns " netns))
          ;; Ensure netns + veth + address exist before starting tailscaled.
          (requirement (list 'user-processes 'networking 'peteches-firewall setup-name))
          (start #~(make-forkexec-constructor
                    (list #$ip "netns" "exec" #$netns #$@run-args)
                    #:log-file #$log-file
                    #:environment-variables
                    (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
          (stop #~(make-kill-destructor))))))))

(define (tailscale-instance->prefs-service cfg)
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package netns magic-dns state-file socket-file tun port log-file extra-args forward-ports _ _ _ _ _)
       (let* ((svc-name     (string->symbol (string-append "tailscale-prefs-" name)))
              (tsd-svc      (string->symbol (string-append "tailscaled-" name)))
              (tailscale    (file-append package "/bin/tailscale"))
              (args         (append (list tailscale
                                          (string-append "--socket=" socket-file)
                                          "set"
                                          (string-append "--accept-dns=false")
                                          (string-append "--accept-routes=true"))
                                    ;; Extra safety: keep this instance from trying to be an exit node by accident
                                    ;; (uncomment if you want)
                                    ;; (list "--advertise-exit-node=false")
                                    )))
         (shepherd-service
          (provision (list svc-name))
          (documentation (string-append "Apply persisted Tailscale prefs for instance " name))
          (requirement (list 'user-processes 'networking tsd-svc))
          (one-shot? #t)
          (auto-start? #t)
          (start #~(make-forkexec-constructor
                    (list #$@args)
                    #:log-file #$(string-append "/var/log/tailscale-prefs-" name ".log")
                    #:environment-variables
                    (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
          (stop #~(lambda _ #t))))))))


(define (ts-wrapper-package cfg)
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          inst-name inst-package ns-name magic-dns state-file socket-file tun port log-file extra-args forward-ports _ _ _ _ _)
       (let* ((bash-path    (file-append bash "/bin/bash"))
              (ip-path      (file-append iproute "/sbin/ip"))
              (ts-bin       (file-append inst-package "/bin/tailscale"))
              (pkg-name     (string-append "tailscale-wrappers-" inst-name)))
         (package
          (name pkg-name)
          (version "0")
          (source #f)
          (build-system trivial-build-system)
          (arguments
           (list
            #:modules '((guix build utils))
            #:builder
            #~(begin
                (use-modules (guix build utils))
                (let* ((out        (assoc-ref %outputs "out"))
                       (bin        (string-append out "/bin"))
                       (ts-path    (string-append bin "/ts-"     #$inst-name))
                       (netns-path (string-append bin "/netns-"  #$inst-name)))
                  (mkdir-p bin)

                  ;; ts-<name>: tailscale CLI pinned to per-instance socket, run in the netns
                  (call-with-output-file ts-path
                    (lambda (port)
                      (format port "#!~a\n" #$bash-path)
                      (display "set -euo pipefail\n" port)

                      (display "SOCK=" port) (write #$socket-file port) (newline port)
                      (display "TS=" port)   (write #$ts-bin port) (newline port)
                      (display "IP=" port)   (write #$ip-path port) (newline port)
                      (display "NS=" port)   (write #$ns-name port) (newline port)
                      (display "if [ \"$#\" -lt 1 ]; then\n" port)
                      (display "  echo \"Usage: ts-<name> <tailscale-subcommand> [args...]\" >&2\n" port)
                      (display "  exit 2\n" port)
                      (display "fi\n" port)
                      (display "if [ \"$(id -u)\" -ne 0 ]; then\n" port)
                      (display "  exec sudo \"$IP\" netns exec \"$NS\" \"$TS\" --socket=\"$SOCK\" \"$@\"\n" port)
                      (display "else\n" port)
                      (display "  exec \"$IP\" netns exec \"$NS\" \"$TS\" --socket=\"$SOCK\" \"$@\"\n" port)
                      (display "fi\n" port)))

                  ;; netns-<name>: generic netns exec wrapper
                  (call-with-output-file netns-path
                    (lambda (port)
                      (format port "#!~a\n" #$bash-path)
                      (display "set -euo pipefail\n" port)
                      (display "IP=" port) (write #$ip-path port) (newline port)
                      (display "NS=" port) (write #$ns-name port) (newline port)
                      (display "if [ \"$#\" -lt 1 ]; then\n" port)
                      (display "  echo \"Usage: netns-<name> <command> [args...]\" >&2\n" port)
                      (display "  exit 2\n" port)
                      (display "fi\n" port)
                      (display "if [ \"$(id -u)\" -ne 0 ]; then\n" port)
                      (display "  exec sudo \"$IP\" netns exec \"$NS\" \"$@\"\n" port)
                      (display "else\n" port)
                      (display "  exec \"$IP\" netns exec \"$NS\" \"$@\"\n" port)
                      (display "fi\n" port)))

                  (chmod ts-path    #o555)
                  (chmod netns-path #o555)))))
          (synopsis (string-append "Wrappers for Tailscale instance " inst-name))
          (description
           "Installs ts-<name> (tailscale CLI pinned to the instance socket, run in the instance netns) and netns-<name> (generic ip netns exec wrapper).")
          (home-page "https://tailscale.com/")
          (license public-domain)))))))

(define (tailscale-profile-entries instances)
  ;; Install tailscale + wrappers + netcat (for SSH ProxyCommand nc).
  ;; Include microsocks when any instance has socks-proxy-port set.
  (let* ((resolved    (map instance->resolved instances))
         (pkgs        (map tailscale-instance-configuration-package resolved))
         (wrappers    (map ts-wrapper-package instances))
         (need-socks? (any tailscale-instance-configuration-socks-proxy-port resolved)))
    (delete-duplicates
     (append pkgs wrappers (list netcat)
             (if need-socks? (list microsocks) '()))
     eq?)))

;; Sudo is picky: sudoers.d files must be owned by root and typically 0440.
;; etc-service-type doesn't let us directly set file modes, so we wrap the
;; generated sudoers text in a computed-file that chmods the output.
(define (file-like->0440 name file)
  (computed-file name
		 (with-imported-modules '((guix build utils))
					#~(begin
					    (use-modules (guix build utils))
					    (copy-file #$file #$output)
					    (chmod #$output #o440)))
		 ;; Crucial: avoid guile-minimal, which lacks (ice-9 posix) etc.
		 #:guile guile-3.0))

;; Ensure this is available in the file:
;; (use-modules (gnu packages netcat))



(define (tailscale-instance->netns-setup-service cfg)
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package netns magic-dns state-file socket-file tun port log-file extra-args forward-ports _ _ _ _ _)
       (let-values (((subnet hostip nsip gw) (subnet-for name)))
         (let* ((ip (file-append iproute "/sbin/ip"))
                (veth-host (ifname "vts-" name))
                (veth-ns   (ifname "vtn-" name))
                (svc-name  (string->symbol (string-append "tailscale-netns-setup-" name))))
           (shepherd-service
            (provision (list svc-name))
            (documentation (string-append "Set up network namespace + veth for Tailscale instance " name))
            (requirement '(user-processes networking peteches-firewall))
            (one-shot? #t)
            (start
             #~(lambda _
                 (define (run/ok? args)
                   (let ((rc (apply system* args)))
                     (if (zero? rc)
                         #t
                         (begin
                           (format (current-error-port)
                                   "netns-setup(~a): failed: ~s (rc=~a)\n"
                                   #$name args rc)
                           #f))))
                 (define (run/ignore args)
                   (let ((rc (apply system* args)))
                     (when (not (zero? rc))
                       (format (current-error-port)
                               "netns-setup(~a): ignoring failure: ~s (rc=~a)\n"
                               #$name args rc))
                     #t))

                 ;; Ensure netns exists (ignore "File exists" errors)
                 (run/ignore (list #$ip "netns" "add" #$netns))

                 ;; loopback up (must succeed)
                 (unless (run/ok? (list #$ip "netns" "exec" #$netns #$ip "link" "set" "lo" "up"))
                   (exit 1))

                 ;; Drop stale host veth if present (ignore errors)
                 (run/ignore (list #$ip "link" "del" #$veth-host))

                 ;; Create veth pair and move one end into the netns (must succeed)
                 (unless (run/ok? (list #$ip "link" "add" #$veth-host "type" "veth" "peer" "name" #$veth-ns))
                   (exit 1))
                 (unless (run/ok? (list #$ip "link" "set" #$veth-ns "netns" #$netns))
                   (exit 1))

                 ;; Host side: ensure IPv4 address is present (REPLACE is key)
                 (unless (run/ok? (list #$ip "addr" "replace" #$hostip "dev" #$veth-host))
                   (exit 1))
                 (unless (run/ok? (list #$ip "link" "set" #$veth-host "up"))
                   (exit 1))

                 ;; Netns side: address + link up
                 (unless (run/ok? (list #$ip "netns" "exec" #$netns #$ip "addr" "replace" #$nsip "dev" #$veth-ns))
                   (exit 1))
                 (unless (run/ok? (list #$ip "netns" "exec" #$netns #$ip "link" "set" #$veth-ns "up"))
                   (exit 1))

                 ;; Default route inside netns
                 (unless (run/ok? (list #$ip "netns" "exec" #$netns #$ip "route" "replace" "default" "via" #$gw))
                   (exit 1))

                 #t))
            (stop #~(lambda _ #t)))))))))



;; Port forward services: listen inside the netns and forward to the host via the veth.
(define (forward-ports->alist forward-ports)
  "Normalize FORWARD-PORTS into an alist of (SRC . DST) integer pairs.
Accept elements like (SRC . DST) or (SRC DST)."
  (define (->pair x)
    (match x
      ((src . dst)
       (unless (and (integer? src) (integer? dst))
         (error "forward-ports entries must be integer port pairs" x))
       (cons src dst))
      ((src dst)
       (unless (and (integer? src) (integer? dst))
         (error "forward-ports entries must be integer port pairs" x))
       (cons src dst))
      (_ (error "invalid forward-ports entry; expected (SRC . DST) or (SRC DST)" x))))
  (map ->pair forward-ports))

(define (tailscale-instance->forward-services cfg)
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package netns magic-dns state-file socket-file tun port log-file extra-args forward-ports _ _ _ _ _)
       (let* ((setup-name (string->symbol (string-append "tailscale-netns-setup-" name)))
              (ip         (file-append iproute "/sbin/ip"))
              (socat      (file-append socat "/bin/socat")))
         (let-values (((subnet hostip nsip gw) (subnet-for name)))
           (map (lambda (p)
                  (let* ((src (car p))
                         (dst (cdr p))
                         (svc-name (string->symbol
                                    (format #f "tailscale-forward-~a-~a" name src)))
                         (lf (format #f "/var/log/tailscale-forward-~a-~a.log" name src)))
                    (shepherd-service
                     (provision (list svc-name))
                     (documentation
                      (format #f "Forward TCP port ~a in netns ~a to host ~a:~a"
                              src netns gw dst))
                     (requirement (list 'user-processes 'networking setup-name))
                     (auto-start? #t)
                     (one-shot? #f)
                     (start
                      #~(make-forkexec-constructor
                         (list #$ip "netns" "exec" #$netns
                               #$socat
                               (string-append "TCP-LISTEN:"
                                              #$(number->string src)
                                              ",fork,reuseaddr")
                               (string-append "TCP:"
                                              #$gw
                                              ":"
                                              #$(number->string dst)))
                         #:log-file #$lf
                         #:environment-variables
                         (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
                     (stop #~(make-kill-destructor)))))
                (forward-ports->alist forward-ports))))))))

(define (tailscale-instance->host-forward-services cfg)
  "Return shepherd services running socat on the HOST, forwarding TCP ports into
the netns.  This is the reverse of tailscale-instance->forward-services.
Returns an empty list when host-forward-ports is empty."
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package netns magic-dns state-file socket-file tun port log-file extra-args
          forward-ports host-forward-ports _ _ _ _)
       (let-values (((subnet hostip nsip gw) (subnet-for name)))
         (let* ((socat      (file-append socat "/bin/socat"))
                (setup-name (string->symbol (string-append "tailscale-netns-setup-" name)))
                (ns-ip      (car (string-split nsip #\/))))
           (map (lambda (p)
                  (let* ((src      (car p))
                         (dst      (cdr p))
                         (svc-name (string->symbol
                                    (format #f "tailscale-host-forward-~a-~a" name src)))
                         (lf       (format #f "/var/log/tailscale-host-forward-~a-~a.log"
                                           name src)))
                    (shepherd-service
                     (provision (list svc-name))
                     (documentation
                      (format #f "Forward TCP port ~a on host to ~a:~a in netns ~a"
                              src ns-ip dst netns))
                     (requirement (list 'user-processes 'networking setup-name))
                     (auto-start? #t)
                     (one-shot? #f)
                     (start
                      #~(make-forkexec-constructor
                         (list #$socat
                               (string-append "TCP-LISTEN:" #$(number->string src)
                                              ",fork,reuseaddr")
                               (string-append "TCP:" #$ns-ip ":" #$(number->string dst)))
                         #:log-file #$lf
                         #:environment-variables
                         (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
                     (stop #~(make-kill-destructor)))))
                (forward-ports->alist host-forward-ports))))))))


(define (tailscale-instance->socks-service cfg)
  "Return a shepherd service running microsocks inside the netns, or #f when
socks-proxy-port is not configured."
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package netns magic-dns state-file socket-file tun port log-file extra-args
          forward-ports _ socks-proxy-port _ _ _)
       (if (not socks-proxy-port)
           #f
           (let-values (((subnet hostip nsip gw) (subnet-for name)))
             (let* ((ip         (file-append iproute "/sbin/ip"))
                    (ms         (file-append microsocks "/bin/microsocks"))
                    (setup-name (string->symbol (string-append "tailscale-netns-setup-" name)))
                    (svc-name   (string->symbol (string-append "tailscale-socks-" name)))
                    (lf         (format #f "/var/log/tailscale-socks-~a.log" name)))
               (shepherd-service
                (provision (list svc-name))
                (documentation
                 (format #f "SOCKS5 proxy (microsocks) for Tailscale instance ~a on port ~a"
                         name socks-proxy-port))
                (requirement (list 'user-processes 'networking setup-name))
                (auto-start? #t)
                (one-shot? #f)
                (start
                 #~(make-forkexec-constructor
                    (list #$ip "netns" "exec" #$netns
                          #$ms
                          "-i" "0.0.0.0"
                          "-p" #$(number->string socks-proxy-port))
                    #:log-file #$lf
                    #:environment-variables
                    (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
                (stop #~(make-kill-destructor))))))))))

(define (tailscale-instance->socks-expose-service cfg)
  "Return a shepherd service running socat in the DEFAULT namespace that
forwards connections on socks-proxy-port to the microsocks instance inside the
netns.  Returns #f when socks-proxy-port is not configured."
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package netns magic-dns state-file socket-file tun port log-file extra-args
          forward-ports _ socks-proxy-port _ _ _)
       (if (not socks-proxy-port)
           #f
           (let-values (((subnet hostip nsip gw) (subnet-for name)))
             (let* ((socat      (file-append socat "/bin/socat"))
                    (socks-svc  (string->symbol (string-append "tailscale-socks-" name)))
                    (svc-name   (string->symbol (string-append "tailscale-socks-expose-" name)))
                    ;; strip /24 suffix from "10.250.X.2/24" to get bare netns IP
                    (ns-ip      (car (string-split nsip #\/)))
                    (lf         (format #f "/var/log/tailscale-socks-expose-~a.log" name)))
               (shepherd-service
                (provision (list svc-name))
                (documentation
                 (format #f "Expose SOCKS5 proxy for Tailscale instance ~a: default-ns 0.0.0.0:~a -> ~a:~a"
                         name socks-proxy-port ns-ip socks-proxy-port))
                (requirement (list 'user-processes 'networking socks-svc))
                (auto-start? #t)
                (one-shot? #f)
                (start
                 #~(make-forkexec-constructor
                    (list #$socat
                          (string-append "TCP-LISTEN:"
                                         #$(number->string socks-proxy-port)
                                         ",fork,reuseaddr")
                          (string-append "TCP:"
                                         #$ns-ip
                                         ":"
                                         #$(number->string socks-proxy-port)))
                    #:log-file #$lf
                    #:environment-variables
                    (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
                (stop #~(make-kill-destructor))))))))))

(define (tailscale-instance->taildrop-service cfg)
  "Return a scheduled shepherd service that retrieves Taildrop files for this
instance, or #f when taildrop-dir is not configured."
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package netns magic-dns state-file socket-file tun port log-file extra-args
          forward-ports host-forward-ports socks-proxy-port taildrop-dir taildrop-user taildrop-schedule)
       (if (not taildrop-dir)
           #f
           (let* ((svc-name  (string->symbol (string-append "tailscale-taildrop-" name)))
                  (tsd-svc   (string->symbol (string-append "tailscaled-" name)))
                  (tailscale (file-append package "/bin/tailscale"))
                  (chown-bin (file-append coreutils "/bin/chown"))
                  (lf        (string-append "/var/log/tailscale-taildrop-" name ".log"))
                  (get-script
                   (program-file
                    (string-append "tailscale-taildrop-get-" name)
                    #~(begin
                        (let ((rc (system* #$tailscale
                                           (string-append "--socket=" #$socket-file)
                                           "file" "get" #$taildrop-dir)))
                          (when (and (zero? rc) #$(if taildrop-user #t #f))
                            (system* #$chown-bin "-R"
                                     #$(if taildrop-user taildrop-user "")
                                     #$taildrop-dir))
                          (exit (if (zero? rc) 0 1)))))))
             (shepherd-service
              (provision (list svc-name))
              (documentation
               (string-append "Poll Taildrop files for Tailscale instance " name
                              " into " taildrop-dir
                              ". Trigger: herd trigger " (symbol->string svc-name)))
              (requirement (list 'user-processes tsd-svc))
              (modules '((shepherd service timer)))
              (start #~(make-timer-constructor
                        (cron-string->calendar-event #$taildrop-schedule)
                        (command (list #$get-script))))
              (stop #~(make-timer-destructor))
              (actions
               (list (shepherd-action
                      (name 'trigger)
                      (documentation "Retrieve Taildrop files immediately.")
                      (procedure #~(lambda (running . _)
                                     (system* #$get-script)
                                     running))))))))))))

(define (tailscale-activation instances)
  (let ((resolved (map instance->resolved instances)))
    #~(begin
        (use-modules (guix build utils)
                     (srfi srfi-1)
                     (srfi srfi-13)
                     (ice-9 match))

        (mkdir-p "/var/log")
        (mkdir-p "/run/tailscale")
        (mkdir-p "/var/lib/tailscale")
        (mkdir-p "/etc/netns")

        (mkdir-p "/run/sudoers.d")
        (chmod "/run/sudoers.d" #o750)

        (when (file-exists? "/proc/sys/net/ipv4/ip_forward")
          (call-with-output-file "/proc/sys/net/ipv4/ip_forward"
            (lambda (p)
              (display "1\n" p))))

        ;; Normalize magic-dns to either #f or a list of strings.
        ;; Error out on anything else.
        (define (magic-dns->list md)
          (cond
           ((not md) #f)
           ((string? md) (list md))
           ((list? md)
            (if (every string? md)
                md
                (error "magic-dns list must contain only strings" md)))
           (else
            (error "magic-dns must be #f, a string, or a list of strings" md))))

        ;; Write netns resolv.conf with fixed nameservers + optional search line.
        (define (write-netns-resolv! path magic-dns)
          (let ((md-list (magic-dns->list magic-dns)))
            (call-with-output-file path
              (lambda (out)
                (display "nameserver 100.100.100.100\n" out)
                (display "nameserver 1.1.1.1\n" out)
                (display "nameserver 8.8.8.8\n" out)
                (when (and md-list (not (null? md-list)))
                  (display "search " out)
                  (display (string-join md-list " ") out)
                  (newline out))))
            (chmod path #o644)))

        ;; Helper: write sudoers include as a REAL file with strict perms.
	(define (write-sudoers! dest ip netns ts socket nc)
	  (let ((tmp (string-append dest ".tmp")))
	    ;; Clean up any prior leftovers
	    (when (file-exists? tmp)
	      (delete-file tmp))
	    (when (file-exists? dest)
	      (delete-file dest))

	    (call-with-output-file tmp
	      (lambda (out)
		;; tailscale in netns
		(display "%wheel ALL=(root) NOPASSWD: " out)
		(display ip out)
		(display " netns exec " out)
		(display netns out)
		(display " " out)
		(display ts out)
		(display " --socket=" out)
		(display socket out)
		(display " *\n" out)

		;; nc in netns (for ProxyCommand)
		(display "%wheel ALL=(root) NOPASSWD: " out)
		(display ip out)
		(display " netns exec " out)
		(display netns out)
		(display " " out)
		(display nc out)
		(display " *\n" out)))

	    (chmod tmp #o440)
	    (rename-file tmp dest)))


        ;; Per-instance dirs + per-netns resolv.conf + per-instance sudoers.
        #$@(map
            (lambda (cfg)
              (match cfg
                (($ <tailscale-instance-configuration>
                    name package netns magic-dns state-file socket-file tun port log-file extra-args forward-ports
                    _ _ taildrop-dir taildrop-user _)
                 (let ((ip (file-append iproute "/sbin/ip"))
                       (ts (file-append package "/bin/tailscale"))
		       (nc (file-append netcat "/bin/nc"))
                       (chown-bin (file-append coreutils "/bin/chown")))
                   #~(begin
                       (mkdir-p (dirname #$state-file))
                       (mkdir-p (dirname #$socket-file))
                       (let* ((d  (string-append "/etc/netns/" #$netns))
                              (rc (string-append d "/resolv.conf")))
                         (mkdir-p d)
                         ;; Always write it so it can't get stuck broken.
                         (write-netns-resolv! rc #$magic-dns))
                       (write-sudoers!
                        (string-append "/run/sudoers.d/tailscale-" #$name)
                        #$ip
                        #$netns
                        #$ts
                        #$socket-file
			#$nc)
                       (when #$taildrop-dir
                         (mkdir-p #$taildrop-dir)
                         (when #$taildrop-user
                           (system* #$chown-bin #$taildrop-user #$taildrop-dir))))))))
            resolved)

        )))

(define (tailscale->firewall-rules instances)
  (let ((instances (map instance->resolved instances)))
    (define (one cfg)
      (match cfg
        (($ <tailscale-instance-configuration>
            inst-name inst-package netns magic-dns state-file socket-file tun port log-file extra-args forward-ports
            _ socks-proxy-port _ _ _)
         (let-values (((subnet hostip nsip gw) (subnet-for inst-name)))
           (let* ((veth-host (ifname "vts-" inst-name))
                  (fwd-pairs (forward-ports->alist forward-ports)))
             (nftables-rules
              (input (append
                      (map (lambda (p)
                             (string-append "ip saddr " subnet
                                            " tcp dport " (number->string (cdr p))
                                            " accept comment \"ts-fwd " inst-name ":" (number->string (cdr p)) "\""))
                           fwd-pairs)
                      (if socks-proxy-port
                          (list
                           ;; Allow host applications to connect to the SOCKS expose listener.
                           (string-append
                            "tcp dport " (number->string socks-proxy-port)
                            " accept comment \"ts-socks expose " inst-name ":" (number->string socks-proxy-port) "\"")
                           ;; Allow return traffic from microsocks inside the netns back to the host.
                           (string-append
                            "ip saddr " subnet
                            " tcp sport " (number->string socks-proxy-port)
                            " ct state { established, related }"
                            " accept comment \"ts-socks return " inst-name ":" (number->string socks-proxy-port) "\""))
                          '())))
              (forward (list
                        ;; Allow NEW+EST+REL flows from the netns veth into the rest of the host.
                        (string-append "iifname \"" veth-host "\" ip saddr " subnet
                                       " ct state { new, established, related } "
                                       "counter accept comment \"ts-netns out " inst-name "\"")
                        ;; Allow return traffic back into the netns.
                        (string-append "oifname \"" veth-host "\" ct state { established, related } "
                                       "counter accept comment \"ts-netns return " inst-name "\"")))
              (nat-postrouting (list
                                ;; Interface-agnostic NAT: masquerade when leaving anywhere
                                ;; except back out the netns veth itself. This survives Wi-Fi
                                ;; vs Ethernet vs tethering changes.
                                (string-append "ip saddr " subnet
                                               " oifname != \"" veth-host "\" "
                                               "counter masquerade comment \"masquerade ts-netns " inst-name "\"")))))))))
    (fold rules-merge (nftables-rules) (map one instances))))

(define (tailscale-shepherd-services instances)
  (append
   (map tailscale-instance->netns-setup-service instances)
   (append-map tailscale-instance->forward-services instances)
   (append-map tailscale-instance->host-forward-services instances)
   (filter-map tailscale-instance->socks-service instances)
   (filter-map tailscale-instance->socks-expose-service instances)
   (filter-map tailscale-instance->taildrop-service instances)
   (map tailscale-instance->shepherd-service instances)
   (map tailscale-instance->prefs-service instances)))

(define-public tailscale-service-type
  (service-type
   (name 'tailscale)
   (compose append)
   (extend append)
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailscale-shepherd-services)
          (service-extension activation-service-type
                             tailscale-activation)
          (service-extension firewall-service-type
                             tailscale->firewall-rules)
          (service-extension profile-service-type
                             tailscale-profile-entries)))
   (default-value '())
   (description "Run one or more Tailscale instances, each in its own network namespace, with nftables NAT and ts-<name> helpers.")))
