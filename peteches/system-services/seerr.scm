;; peteches/system-services/seerr.scm — Seerr media request manager service.

(define-module (peteches system-services seerr)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (peteches packages seerr)
  #:use-module (peteches system-services firewall)
  #:use-module (peteches system-services media-accounts)
  #:export (seerr-configuration
            seerr-configuration?
            seerr-service-type))

(define-record-type* <seerr-configuration>
  seerr-configuration make-seerr-configuration
  seerr-configuration?
  (package  seerr-configuration-package  (default seerr))
  (port     seerr-configuration-port     (default 5055))
  (data-dir seerr-configuration-data-dir (default "/var/lib/seerr"))
  (log-file seerr-configuration-log-file (default "/var/log/seerr.log")))

(define (seerr-accounts config)
  (list
   (user-account
    (name "seerr")
    (group "users")
    (system? #t)
    (comment "Seerr media request manager daemon")
    (home-directory (seerr-configuration-data-dir config))
    (shell (file-append shadow "/sbin/nologin")))))

(define (seerr-activation config)
  (let ((data-dir (seerr-configuration-data-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw  (getpwnam "seerr"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (mkdir-p #$data-dir)
          (chown #$data-dir uid gid)))))

(define (seerr-shepherd-service config)
  (let* ((pkg      (seerr-configuration-package config))
         (data-dir (seerr-configuration-data-dir config))
         (log-file (seerr-configuration-log-file config))
         (port     (seerr-configuration-port config)))
    (list
     (shepherd-service
      (provision '(seerr))
      (documentation "Seerr media request manager.")
      (requirement '(networking file-systems))
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/seerr"))
                #:user "seerr"
                #:group "users"
                #:environment-variables
                (list (string-append "CONFIG_DIRECTORY=" #$data-dir)
                      (string-append "PORT=" #$(number->string port))
                      "NODE_ENV=production")
                #:log-file #$log-file))
      (stop #~(make-kill-destructor))))))

(define (seerr-firewall-rules config)
  (nftables-rules
   (input (list (string-append
                 "tcp dport "
                 (number->string (seerr-configuration-port config))
                 " accept comment \"seerr\"")))))

(define (seerr-profile config)
  (list (seerr-configuration-package config)))

(define-public seerr-service-type
  (service-type
   (name 'seerr)
   (description "Seerr media request manager.")
   (extensions
    (list
     (service-extension account-service-type       seerr-accounts)
     (service-extension activation-service-type    seerr-activation)
     (service-extension shepherd-root-service-type seerr-shepherd-service)
     (service-extension firewall-service-type      seerr-firewall-rules)
     (service-extension profile-service-type       seerr-profile)))
   (default-value (seerr-configuration))))
