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
  #:use-module (gnu packages networking)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)     ; iproute + nftables
  #:use-module (gnu packages bash)      ; bash for wrapper scripts
  #:use-module (gnu packages web)
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
		    (default ""))

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
               (default '())))

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
        name package netns magic-dns state-file socket-file tun port log-file extra-args)
     (let-values (((dstate dsock dlog dtun) (instance-default-paths name)))
       (tailscale-instance-configuration
        (name name)
        (package package)
        (netns (or netns (string-append "ts-" name)))
        (state-file (or state-file dstate))
        (socket-file (or socket-file dsock))
        (tun (or tun dtun))
        (port port)
        (log-file (or log-file dlog))
        (extra-args extra-args))))))

(define (tailscale-instance->shepherd-service cfg)
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package netns magic-dns state-file socket-file tun port log-file extra-args)
       (let* ((service-name (string->symbol (string-append "tailscaled-" name)))
              (setup-name   (string->symbol (string-append "tailscale-netns-setup-" name)))
              (ip (file-append iproute "/sbin/ip"))
              (run-args
               (append (list (file-append package "/bin/tailscaled")
                             (string-append "--state=" state-file)
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
          name package netns magic-dns state-file socket-file tun port log-file extra-args)
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
                    #:log-file #$log-file
                    #:environment-variables
                    (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
          (stop #~(lambda _ #t))))))))


(define (ts-wrapper-package cfg)
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          inst-name inst-package ns-name magic-dns state-file socket-file tun port log-file extra-args)
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
  ;; Install tailscale + wrappers.
  ;; (Guix will dedupe store paths anyway, but we keep the list tidy.)
  (let* ((resolved (map instance->resolved instances))
         (pkgs (map tailscale-instance-configuration-package resolved))
         (wrappers (map ts-wrapper-package instances)))
    (append (delete-duplicates pkgs eq?) wrappers)))

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

(define (tailscale-sudoers-entries instances)
  ;; etc-service-type expects an alist of (DESTINATION . FILE-LIKE)
  (map (lambda (cfg)
         (let ((cfg (instance->resolved cfg)))
           (match cfg
             (($ <tailscale-instance-configuration>
                 name package netns magic-dns state-file socket-file tun port log-file extra-args)
              (let* ((ip (file-append iproute "/sbin/ip"))
                     (ts (file-append package "/bin/tailscale"))
                     (rule
                      (mixed-text-file (string-append "sudoers-tailscale-" name)
                        "%wheel ALL=(root) NOPASSWD: "
                        ip " netns exec " netns " "
                        ts " --socket=" socket-file " *\n")))
                (list (string-append "sudoers.d/tailscale-" name)
                      (file-like->0440 (string-append "sudoers-0440-" name) rule)))))))
       instances))

(define (tailscale-instance->netns-setup-service cfg)
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package netns magic-dns state-file socket-file tun port log-file extra-args)
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
        (define (write-sudoers! dest ip netns ts socket)
          (let ((tmp (string-append dest ".tmp")))
            (when (file-exists? dest)
              (delete-file dest))
            (call-with-output-file tmp
              (lambda (out)
                (display "%wheel ALL=(root) NOPASSWD: " out)
                (display ip out)
                (display " netns exec " out)
                (display netns out)
                (display " " out)
                (display ts out)
                (display " --socket=" out)
                (display socket out)
                (display " *\n" out)))
            (chmod tmp #o440)
            (rename-file tmp dest)))

        ;; Per-instance dirs + per-netns resolv.conf + per-instance sudoers.
        #$@(map
            (lambda (cfg)
              (match cfg
                (($ <tailscale-instance-configuration>
                    name package netns magic-dns state-file socket-file tun port log-file extra-args)
                 (let ((ip (file-append iproute "/sbin/ip"))
                       (ts (file-append package "/bin/tailscale")))
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
                        #$socket-file))))))
            resolved))))

(define (tailscale->firewall-rules instances)
  (let ((instances (map instance->resolved instances)))
    (define (one cfg)
      (match cfg
        (($ <tailscale-instance-configuration>
            inst-name inst-package netns magic-dns state-file socket-file tun port log-file extra-args)
         (let-values (((subnet hostip nsip gw) (subnet-for inst-name)))
           (let ((veth-host (ifname "vts-" inst-name)))
             (nftables-rules
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
                             tailscale-profile-entries)
          (service-extension etc-service-type
                             tailscale-sudoers-entries)))
   (default-value '())
   (description "Run one or more Tailscale instances, each in its own network namespace, with nftables NAT and ts-<name> helpers.")))
