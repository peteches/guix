;; peteches/system-services/prowlarr.scm — Prowlarr indexer manager service.

(define-module (peteches system-services prowlarr)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (peteches packages prowlarr)
  #:use-module (peteches system-services firewall)
  #:use-module (peteches system-services media-accounts)
  #:export (prowlarr-configuration
            prowlarr-configuration?
            prowlarr-service-type))

(define-record-type* <prowlarr-configuration>
  prowlarr-configuration make-prowlarr-configuration
  prowlarr-configuration?
  (package  prowlarr-configuration-package  (default prowlarr))
  (port     prowlarr-configuration-port     (default 9696))
  (data-dir prowlarr-configuration-data-dir (default "/var/lib/prowlarr"))
  (log-file prowlarr-configuration-log-file (default "/var/log/prowlarr.log")))

(define (prowlarr-accounts config)
  (list
   (user-account
    (name "prowlarr")
    (group "media")
    (system? #t)
    (comment "Prowlarr indexer manager daemon")
    (home-directory (prowlarr-configuration-data-dir config))
    (shell (file-append shadow "/sbin/nologin")))))

(define (prowlarr-activation config)
  (let ((data-dir (prowlarr-configuration-data-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw  (getpwnam "prowlarr"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (mkdir-p #$data-dir)
          (chown #$data-dir uid gid)))))

(define (prowlarr-shepherd-service config)
  (let* ((pkg      (prowlarr-configuration-package config))
         (data-dir (prowlarr-configuration-data-dir config))
         (log-file (prowlarr-configuration-log-file config)))
    (list
     (shepherd-service
      (provision '(prowlarr))
      (documentation "Prowlarr indexer manager.")
      (requirement '(networking file-systems))
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/prowlarr")
                      (string-append "--data=" #$data-dir)
                      "--nobrowser")
                #:user "prowlarr"
                #:group "media"
                #:log-file #$log-file))
      (stop #~(make-kill-destructor))))))

(define (prowlarr-firewall-rules config)
  (nftables-rules
   (input (list (string-append
                 "tcp dport "
                 (number->string (prowlarr-configuration-port config))
                 " accept comment \"prowlarr\"")))))

(define (prowlarr-profile config)
  (list (prowlarr-configuration-package config)))

(define-public prowlarr-service-type
  (service-type
   (name 'prowlarr)
   (description "Prowlarr indexer manager for the *Arr suite.")
   (extensions
    (list
     (service-extension account-service-type       prowlarr-accounts)
     (service-extension activation-service-type    prowlarr-activation)
     (service-extension shepherd-root-service-type prowlarr-shepherd-service)
     (service-extension firewall-service-type      prowlarr-firewall-rules)
     (service-extension profile-service-type       prowlarr-profile)))
   (default-value (prowlarr-configuration))))
