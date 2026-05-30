;; peteches/system-services/sonarr.scm — Sonarr TV automation service.

(define-module (peteches system-services sonarr)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (peteches packages sonarr)
  #:use-module (peteches system-services firewall)
  #:use-module (peteches system-services media-accounts)
  #:export (sonarr-configuration
            sonarr-configuration?
            sonarr-service-type))

(define-record-type* <sonarr-configuration>
  sonarr-configuration make-sonarr-configuration
  sonarr-configuration?
  (package  sonarr-configuration-package  (default sonarr))
  (port     sonarr-configuration-port     (default 8989))
  (data-dir sonarr-configuration-data-dir (default "/var/lib/sonarr"))
  (log-file sonarr-configuration-log-file (default "/var/log/sonarr.log")))

(define (sonarr-accounts config)
  (list
   (user-account
    (name "sonarr")
    (group "media")
    (system? #t)
    (comment "Sonarr TV automation daemon")
    (home-directory (sonarr-configuration-data-dir config))
    (shell (file-append shadow "/sbin/nologin")))))

(define (sonarr-activation config)
  (let ((data-dir (sonarr-configuration-data-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw  (getpwnam "sonarr"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (mkdir-p #$data-dir)
          (chown #$data-dir uid gid)))))

(define (sonarr-shepherd-service config)
  (let* ((pkg      (sonarr-configuration-package config))
         (data-dir (sonarr-configuration-data-dir config))
         (log-file (sonarr-configuration-log-file config)))
    (list
     (shepherd-service
      (provision '(sonarr))
      (documentation "Sonarr TV show automation.")
      (requirement '(networking file-systems))
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/sonarr")
                      (string-append "--data=" #$data-dir)
                      "--nobrowser")
                #:user "sonarr"
                #:group "media"
                #:log-file #$log-file))
      (stop #~(make-kill-destructor))))))

(define (sonarr-firewall-rules config)
  (nftables-rules
   (input (list (string-append
                 "tcp dport "
                 (number->string (sonarr-configuration-port config))
                 " accept comment \"sonarr\"")))))

(define (sonarr-profile config)
  (list (sonarr-configuration-package config)))

(define-public sonarr-service-type
  (service-type
   (name 'sonarr)
   (description "Sonarr TV show automation service.")
   (extensions
    (list
     (service-extension account-service-type       sonarr-accounts)
     (service-extension activation-service-type    sonarr-activation)
     (service-extension shepherd-root-service-type sonarr-shepherd-service)
     (service-extension firewall-service-type      sonarr-firewall-rules)
     (service-extension profile-service-type       sonarr-profile)))
   (default-value (sonarr-configuration))))
