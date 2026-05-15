;; peteches/system-services/pihole.scm — Guix service type for Pi-hole.
;;
;; Pi-hole FTL (Faster-Than-Light) is a modified dnsmasq that provides DNS
;; ad-blocking, a built-in web server, and a SQLite query database.
;;
;; Key runtime notes:
;;  - FTL must start as root to bind port 53; it manages its own privilege
;;    handling internally.
;;  - Config files installed via etc-service-type are read-only (store-derived).
;;    Activation copies pihole.toml to data-dir for FTL to read/write at runtime.
;;  - Gravity (blocklist database) lives in data-dir; populate it via the web
;;    interface or the API after first boot.

(define-module (peteches system-services pihole)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages dns)
  #:use-module (srfi srfi-1)
  #:use-module (peteches packages pihole)
  #:use-module (peteches system-services firewall)
  #:export (pihole-unbound-configuration
            pihole-unbound-configuration?
            pihole-configuration
            pihole-configuration?
            %default-pihole-adlists
            pihole-service-type))

;;; ── Default values ───────────────────────────────────────────────────────

(define %default-pihole-adlists
  '("https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"))

;;; ── Record types ─────────────────────────────────────────────────────────

(define-record-type* <pihole-unbound-configuration>
  pihole-unbound-configuration make-pihole-unbound-configuration
  pihole-unbound-configuration?
  (listen-port    pihole-unbound-configuration-listen-port    (default 5335))
  (listen-address pihole-unbound-configuration-listen-address (default "127.0.0.1"))
  ;; Raw text appended to the server: block of unbound.conf.
  (extra-server   pihole-unbound-configuration-extra-server   (default "")))

(define-record-type* <pihole-configuration>
  pihole-configuration make-pihole-configuration
  pihole-configuration?
  (package        pihole-configuration-package        (default pihole-ftl))
  ;; Network interface FTL binds to.
  (interface      pihole-configuration-interface      (default "eth0"))
  ;; Upstream DNS servers.  When with-unbound? is #t this is overridden to
  ;; point at the local Unbound instance.
  (dns-upstreams  pihole-configuration-dns-upstreams  (default '("8.8.8.8" "8.8.4.4")))
  ;; Blocking reply mode: "NULL" | "NXDOMAIN" | "NODATA" | "IP" | "IP_NODATA_AAAA"
  (blocking-mode  pihole-configuration-blocking-mode  (default "NULL"))
  ;; Enable DNS query logging.
  (query-logging? pihole-configuration-query-logging? (default #t))
  ;; Writable directory for FTL databases (gravity.db, pihole-FTL.db, etc.)
  (data-dir       pihole-configuration-data-dir       (default "/var/lib/pihole"))
  ;; Directory for FTL, dnsmasq and webserver log files.
  (log-dir        pihole-configuration-log-dir        (default "/var/log/pihole"))
  ;; HTTP port for the built-in web interface.  Set to #f to disable.
  (web-port       pihole-configuration-web-port       (default 80))
  ;; When #t, also start an Unbound recursive resolver and point FTL at it.
  (with-unbound?  pihole-configuration-with-unbound?  (default #f))
  ;; Unbound configuration (used only when with-unbound? is #t).
  (unbound        pihole-configuration-unbound        (default (pihole-unbound-configuration)))
  ;; Raw TOML appended verbatim after the generated sections.
  (extra-toml     pihole-configuration-extra-toml     (default ""))
  ;; Extra CLI arguments passed to pihole-FTL.
  (extra-args     pihole-configuration-extra-args     (default '())))

;;; ── TOML / config-file rendering ─────────────────────────────────────────
;;
;; All rendering is pure Scheme (no gexps); results are passed to plain-file.

(define (toml-bool b)
  (if b "true" "false"))

(define (toml-string s)
  (string-append "\"" s "\""))

(define (toml-string-array lst)
  (string-append
   "["
   (string-join (map toml-string lst) ", ")
   "]"))

(define (render-pihole-toml config)
  "Produce a complete pihole.toml string for pihole-FTL v6."
  (let* ((with-ub?      (pihole-configuration-with-unbound? config))
         (ub            (pihole-configuration-unbound config))
         (upstreams     (if with-ub?
                            (list (string-append
                                   (pihole-unbound-configuration-listen-address ub)
                                   "#"
                                   (number->string
                                    (pihole-unbound-configuration-listen-port ub))))
                            (pihole-configuration-dns-upstreams config)))
         (data-dir      (pihole-configuration-data-dir config))
         (log-dir       (pihole-configuration-log-dir config))
         (web-port      (pihole-configuration-web-port config))
         (extra         (pihole-configuration-extra-toml config)))
    (string-append
     "[dns]\n"
     "  upstreams = " (toml-string-array upstreams) "\n"
     "  interface = " (toml-string (pihole-configuration-interface config)) "\n"
     "  listeningMode = \"SINGLE\"\n"
     "  queryLogging = " (toml-bool (pihole-configuration-query-logging? config)) "\n"
     "  blocking.active = true\n"
     "  blocking.mode = " (toml-string (pihole-configuration-blocking-mode config)) "\n"
     "\n"
     "[files]\n"
     "  database = " (toml-string (string-append data-dir "/pihole-FTL.db")) "\n"
     "  gravity = " (toml-string (string-append data-dir "/gravity.db")) "\n"
     "  gravity_tmp = \"/tmp\"\n"
     "  macvendor = " (toml-string (string-append data-dir "/macvendor.db")) "\n"
     "  log.ftl = " (toml-string (string-append log-dir "/FTL.log")) "\n"
     "  log.dnsmasq = " (toml-string (string-append log-dir "/pihole.log")) "\n"
     "  log.webserver = " (toml-string (string-append log-dir "/webserver.log")) "\n"
     "\n"
     "[webserver]\n"
     "  port = " (toml-string (if web-port
                                  (string-append (number->string web-port) "o")
                                  "")) "\n"
     "\n"
     "[ntp]\n"
     "  sync.active = false\n"
     "\n"
     extra)))

(define (render-unbound-conf config)
  "Produce an unbound.conf for use as a Pi-hole recursive upstream resolver."
  (let* ((ub   (pihole-configuration-unbound config))
         (port (pihole-unbound-configuration-listen-port ub))
         (addr (pihole-unbound-configuration-listen-address ub))
         (xtra (pihole-unbound-configuration-extra-server ub)))
    (string-append
     "server:\n"
     "    verbosity: 0\n"
     "    username: \"unbound\"\n"
     "    interface: " addr "\n"
     "    port: " (number->string port) "\n"
     "    do-ip4: yes\n"
     "    do-udp: yes\n"
     "    do-tcp: yes\n"
     "    do-ip6: no\n"
     "    harden-glue: yes\n"
     "    harden-dnssec-stripped: yes\n"
     "    use-caps-for-id: no\n"
     "    edns-buffer-size: 1232\n"
     "    prefetch: yes\n"
     "    num-threads: 1\n"
     "    so-rcvbuf: 1m\n"
     "    private-address: 192.168.0.0/16\n"
     "    private-address: 169.254.0.0/16\n"
     "    private-address: 172.16.0.0/12\n"
     "    private-address: 10.0.0.0/8\n"
     "    private-address: fd00::/8\n"
     "    private-address: fe80::/10\n"
     xtra "\n")))

;;; ── Service extension helpers ─────────────────────────────────────────────

(define (pihole-accounts config)
  (append
   (list
    (user-group
     (name "pihole")
     (system? #t))
    (user-account
     (name "pihole")
     (group "pihole")
     (system? #t)
     (comment "Pi-hole FTL daemon")
     (home-directory "/var/lib/pihole")
     (shell (file-append shadow "/sbin/nologin"))))
   (if (pihole-configuration-with-unbound? config)
       (list
        (user-group
         (name "unbound")
         (system? #t))
        (user-account
         (name "unbound")
         (group "unbound")
         (system? #t)
         (comment "Unbound DNS resolver")
         (home-directory "/var/lib/unbound")
         (shell (file-append shadow "/sbin/nologin"))))
       '())))

(define (pihole-activation config)
  ;; Bind the generated TOML as a store file-like so the gexp can reference it
  ;; directly — this avoids putting pihole.toml in /etc/ where FTL would try to
  ;; chown it (a read-only store symlink).
  (let* ((data-dir  (pihole-configuration-data-dir config))
         (log-dir   (pihole-configuration-log-dir config))
         (toml-file (plain-file "pihole.toml" (render-pihole-toml config))))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$data-dir)
        (mkdir-p #$log-dir)
        (mkdir-p "/run/pihole")
        ;; Ensure pihole user owns writable directories.
        (let* ((pw  (getpwnam "pihole"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (chown #$data-dir uid gid)
          (chown #$log-dir  uid gid)
          (chown "/run/pihole" uid gid))
        ;; Copy the store-derived pihole.toml to the writable data-dir.
        ;; Overwrites when the store version is newer (i.e. after reconfiguration).
        (let ((src #$toml-file)
              (dst (string-append #$data-dir "/pihole.toml")))
          (when (or (not (file-exists? dst))
                    (> (stat:mtime (stat src))
                       (stat:mtime (stat dst))))
            (copy-file src dst)
            (let* ((pw  (getpwnam "pihole"))
                   (uid (passwd:uid pw))
                   (gid (passwd:gid pw)))
              (chown dst uid gid)))))))

(define (pihole-etc-files config)
  ;; pihole.toml is NOT installed here — FTL tries to chown its config file at
  ;; startup, which fails on a read-only store symlink.  It is instead copied to
  ;; data-dir (writable) by the activation gexp.
  (if (pihole-configuration-with-unbound? config)
      (list `("unbound/unbound.conf"
              ,(plain-file "unbound.conf" (render-unbound-conf config))))
      '()))

(define (make-unbound-shepherd-service config)
  (shepherd-service
   (provision '(unbound))
   (documentation "Unbound recursive DNS resolver (Pi-hole upstream).")
   (requirement '(networking file-systems))
   (start #~(make-forkexec-constructor
              (list #$(file-append unbound "/sbin/unbound")
                    "-d"                      ; don't daemonise (shepherd controls it)
                    "-c" "/etc/unbound/unbound.conf")
              ;; Start as root so unbound can raise fd limits and bind; it
              ;; drops to the "unbound" user internally via username: in conf.
              #:log-file #$(string-append
                            (pihole-configuration-log-dir config)
                            "/unbound.log")))
   (stop #~(make-kill-destructor))))

(define (pihole-shepherd-service config)
  (let* ((pkg      (pihole-configuration-package config))
         (data-dir (pihole-configuration-data-dir config))
         (log-dir  (pihole-configuration-log-dir config))
         (with-ub? (pihole-configuration-with-unbound? config))
         (ftl-svc
          (shepherd-service
           (provision '(pihole))
           (documentation "Pi-hole FTL DNS ad-blocking daemon.")
           (requirement (append '(networking file-systems)
                                (if with-ub? '(unbound) '())))
           (start #~(make-forkexec-constructor
                     (append
                      (list #$(file-append pkg "/bin/pihole-FTL")
                            "--config"
                            (string-append #$data-dir "/pihole.toml"))
                      '#$(pihole-configuration-extra-args config))
                     #:log-file (string-append #$log-dir "/FTL.log")))
           (stop #~(make-kill-destructor)))))
    (if with-ub?
        (list (make-unbound-shepherd-service config) ftl-svc)
        (list ftl-svc))))

(define (pihole-firewall-rules config)
  (let ((web-port (pihole-configuration-web-port config)))
    (nftables-rules
     (input
      (append
       (list "udp dport 53 accept comment \"pihole-dns\""
             "tcp dport 53 accept comment \"pihole-dns\"")
       (if web-port
           (list (string-append "tcp dport "
                                (number->string web-port)
                                " accept comment \"pihole-web\""))
           '()))))))

(define (pihole-profile config)
  (append
   (list (pihole-configuration-package config))
   (if (pihole-configuration-with-unbound? config)
       (list unbound)
       '())))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public pihole-service-type
  (service-type
   (name 'pihole)
   (description
    "Pi-hole FTL DNS ad-blocking daemon.  Optionally starts Unbound as a
recursive upstream resolver.  After first boot, seed the gravity database via
the web interface at http://<host>/ or via the Pi-hole API.")
   (extensions
    (list
     (service-extension account-service-type       pihole-accounts)
     (service-extension activation-service-type    pihole-activation)
     (service-extension etc-service-type           pihole-etc-files)
     (service-extension shepherd-root-service-type  pihole-shepherd-service)
     (service-extension firewall-service-type      pihole-firewall-rules)
     (service-extension profile-service-type       pihole-profile)))
   (default-value (pihole-configuration))))
