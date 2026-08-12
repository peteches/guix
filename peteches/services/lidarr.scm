;; peteches/services/lidarr.scm — Lidarr music automation service.

(define-module (peteches services lidarr)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (peteches packages lidarr)
  #:use-module (peteches services firewall)
  #:use-module (peteches services media-accounts)
  #:export (lidarr-configuration lidarr-configuration? lidarr-service-type))

(define-record-type* <lidarr-configuration> lidarr-configuration
                     make-lidarr-configuration
  lidarr-configuration?
  (package
    lidarr-configuration-package
    (default lidarr))
  (port lidarr-configuration-port
        (default 8686))
  (data-dir lidarr-configuration-data-dir
            (default "/var/lib/lidarr"))
  (log-file lidarr-configuration-log-file
            (default "/var/log/lidarr.log")))

(define (lidarr-accounts config)
  (list (user-account
          (name "lidarr")
          (group "media")
          (system? #t)
          (comment "Lidarr music automation daemon")
          (home-directory (lidarr-configuration-data-dir config))
          (shell (file-append shadow "/sbin/nologin")))))

(define (lidarr-activation config)
  (let ((data-dir (lidarr-configuration-data-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw (getpwnam "lidarr"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (mkdir-p #$data-dir)
          (chown #$data-dir uid gid)))))

(define (lidarr-shepherd-service config)
  (let* ((pkg (lidarr-configuration-package config))
         (data-dir (lidarr-configuration-data-dir config))
         (log-file (lidarr-configuration-log-file config)))
    (list (shepherd-service (provision '(lidarr))
                            (documentation "Lidarr music automation.")
                            (requirement '(networking file-systems))
                            (start #~(make-forkexec-constructor (list #$(file-append
                                                                         pkg
                                                                         "/bin/lidarr")
                                                                 (string-append
                                                                  "--data="
                                                                  #$data-dir)
                                                                 "--nobrowser")
                                                                #:user
                                                                "lidarr"
                                                                #:group
                                                                "media"
                                                                #:log-file #$log-file))
                            (stop #~(make-kill-destructor))))))

(define (lidarr-firewall-rules config)
  (nftables-rules (input (list (string-append "tcp dport "
                                              (number->string (lidarr-configuration-port
                                                               config))
                                              " accept comment \"lidarr\"")))))

(define (lidarr-profile config)
  (list (lidarr-configuration-package config)))

(define-public lidarr-service-type
  (service-type (name 'lidarr)
                (description "Lidarr music automation service.")
                (extensions (list (service-extension account-service-type
                                                     lidarr-accounts)
                                  (service-extension activation-service-type
                                                     lidarr-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   lidarr-shepherd-service)
                                  (service-extension firewall-service-type
                                                     lidarr-firewall-rules)
                                  (service-extension profile-service-type
                                                     lidarr-profile)))
                (default-value (lidarr-configuration))))
