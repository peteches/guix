;; peteches/system-services/transmission.scm — Transmission BitTorrent daemon.

(define-module (peteches system-services transmission)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bittorrent)   ; upstream transmission
  #:use-module (peteches system-services firewall)
  #:use-module (peteches system-services media-accounts)
  #:export (transmission-configuration
            transmission-configuration?
            transmission-service-type))

(define-record-type* <transmission-configuration>
  transmission-configuration make-transmission-configuration
  transmission-configuration?
  (package      transmission-configuration-package      (default transmission))
  (rpc-port     transmission-configuration-rpc-port     (default 9091))
  (peer-port    transmission-configuration-peer-port    (default 51413))
  (data-dir     transmission-configuration-data-dir     (default "/var/lib/transmission"))
  (download-dir transmission-configuration-download-dir (default "/media/downloads/torrents"))
  (log-file     transmission-configuration-log-file     (default "/var/log/transmission.log")))

(define (transmission-initial-settings config)
  "Return a JSON string with initial Transmission settings."
  (string-append
   "{\n"
   "    \"download-dir\": \"" (transmission-configuration-download-dir config) "\",\n"
   "    \"incomplete-dir\": \"" (transmission-configuration-download-dir config) "/.incomplete\",\n"
   "    \"incomplete-dir-enabled\": true,\n"
   "    \"rpc-enabled\": true,\n"
   "    \"rpc-port\": " (number->string (transmission-configuration-rpc-port config)) ",\n"
   "    \"rpc-whitelist-enabled\": false,\n"
   "    \"rpc-authentication-required\": false,\n"
   "    \"rpc-bind-address\": \"0.0.0.0\",\n"
   "    \"peer-port\": " (number->string (transmission-configuration-peer-port config)) ",\n"
   "    \"peer-port-random-on-start\": false,\n"
   "    \"utp-enabled\": true,\n"
   "    \"ratio-limit-enabled\": false,\n"
   "    \"trash-original-torrent-files\": false\n"
   "}\n"))

(define (transmission-accounts config)
  (list
   (user-account
    (name "transmission")
    (group "media")
    (system? #t)
    (comment "Transmission BitTorrent daemon")
    (home-directory (transmission-configuration-data-dir config))
    (shell (file-append shadow "/sbin/nologin")))))

(define (transmission-activation config)
  (let ((data-dir     (transmission-configuration-data-dir config))
        (download-dir (transmission-configuration-download-dir config))
        (settings     (transmission-initial-settings config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw           (getpwnam "transmission"))
               (uid          (passwd:uid pw))
               (gid          (passwd:gid pw))
               (settings-file (string-append #$data-dir "/settings.json")))
          (for-each (lambda (d)
                      (mkdir-p d)
                      (chown d uid gid))
                    (list #$data-dir
                          #$download-dir
                          (string-append #$download-dir "/.incomplete")))
          ;; Write settings only on first boot so web-UI changes are preserved.
          (unless (file-exists? settings-file)
            (call-with-output-file settings-file
              (lambda (p) (display #$settings p)))
            (chown settings-file uid gid)
            (chmod settings-file #o640))))))

(define (transmission-shepherd-service config)
  (let* ((pkg      (transmission-configuration-package config))
         (data-dir (transmission-configuration-data-dir config))
         (log-file (transmission-configuration-log-file config)))
    (list
     (shepherd-service
      (provision '(transmission))
      (documentation "Transmission BitTorrent daemon.")
      (requirement '(networking file-systems))
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/transmission-daemon")
                      "--config-dir" #$data-dir
                      "--foreground")
                #:user "transmission"
                #:group "media"
                #:log-file #$log-file))
      (stop #~(make-kill-destructor))))))

(define (transmission-firewall-rules config)
  (nftables-rules
   (input (list
           (string-append
            "tcp dport "
            (number->string (transmission-configuration-rpc-port config))
            " accept comment \"transmission-rpc\"")
           (string-append
            "tcp dport "
            (number->string (transmission-configuration-peer-port config))
            " accept comment \"transmission-peer\"")
           (string-append
            "udp dport "
            (number->string (transmission-configuration-peer-port config))
            " accept comment \"transmission-peer-udp\"")))))

(define (transmission-profile config)
  (list (transmission-configuration-package config)))

(define-public transmission-service-type
  (service-type
   (name 'transmission)
   (description "Transmission BitTorrent daemon service.")
   (extensions
    (list
     (service-extension account-service-type       transmission-accounts)
     (service-extension activation-service-type    transmission-activation)
     (service-extension shepherd-root-service-type transmission-shepherd-service)
     (service-extension firewall-service-type      transmission-firewall-rules)
     (service-extension profile-service-type       transmission-profile)))
   (default-value (transmission-configuration))))
