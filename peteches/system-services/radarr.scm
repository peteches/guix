;; peteches/system-services/radarr.scm — Radarr movie automation service.

(define-module (peteches system-services radarr)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (peteches packages radarr)
  #:use-module (peteches system-services firewall)
  #:use-module (peteches system-services media-accounts)
  #:export (radarr-configuration
            radarr-configuration?
            radarr-service-type))

(define-record-type* <radarr-configuration>
  radarr-configuration make-radarr-configuration
  radarr-configuration?
  (package  radarr-configuration-package  (default radarr))
  (port     radarr-configuration-port     (default 7878))
  (data-dir radarr-configuration-data-dir (default "/var/lib/radarr"))
  (log-file radarr-configuration-log-file (default "/var/log/radarr.log")))

(define (radarr-accounts config)
  (list
   (user-account
    (name "radarr")
    (group "media")
    (system? #t)
    (comment "Radarr movie automation daemon")
    (home-directory (radarr-configuration-data-dir config))
    (shell (file-append shadow "/sbin/nologin")))))

(define (radarr-activation config)
  (let ((data-dir (radarr-configuration-data-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw  (getpwnam "radarr"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (mkdir-p #$data-dir)
          (chown #$data-dir uid gid)))))

(define (radarr-shepherd-service config)
  (let* ((pkg      (radarr-configuration-package config))
         (data-dir (radarr-configuration-data-dir config))
         (log-file (radarr-configuration-log-file config)))
    (list
     (shepherd-service
      (provision '(radarr))
      (documentation "Radarr movie automation.")
      (requirement '(networking file-systems))
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/radarr")
                      (string-append "--data=" #$data-dir)
                      "--nobrowser")
                #:user "radarr"
                #:group "media"
                #:log-file #$log-file))
      (stop #~(make-kill-destructor))))))

(define (radarr-firewall-rules config)
  (nftables-rules
   (input (list (string-append
                 "tcp dport "
                 (number->string (radarr-configuration-port config))
                 " accept comment \"radarr\"")))))

(define (radarr-profile config)
  (list (radarr-configuration-package config)))

(define-public radarr-service-type
  (service-type
   (name 'radarr)
   (description "Radarr movie automation service.")
   (extensions
    (list
     (service-extension account-service-type       radarr-accounts)
     (service-extension activation-service-type    radarr-activation)
     (service-extension shepherd-root-service-type radarr-shepherd-service)
     (service-extension firewall-service-type      radarr-firewall-rules)
     (service-extension profile-service-type       radarr-profile)))
   (default-value (radarr-configuration))))
