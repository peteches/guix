;; peteches/system-services/nzbget.scm — NZBGet Usenet downloader service.

(define-module (peteches system-services nzbget)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages networking)   ; upstream nzbget
  #:use-module (peteches system-services firewall)
  #:use-module (peteches system-services media-accounts)
  #:export (nzbget-configuration
            nzbget-configuration?
            nzbget-service-type))

(define-record-type* <nzbget-configuration>
  nzbget-configuration make-nzbget-configuration
  nzbget-configuration?
  (package     nzbget-configuration-package     (default nzbget))
  (port        nzbget-configuration-port        (default 6789))
  (data-dir    nzbget-configuration-data-dir    (default "/var/lib/nzbget"))
  (main-dir    nzbget-configuration-main-dir    (default "/media/downloads/usenet"))
  (log-file    nzbget-configuration-log-file    (default "/var/log/nzbget.log")))

(define (nzbget-initial-config config)
  "Return a string with the initial nzbget.conf content."
  (string-append
   "MainDir=" (nzbget-configuration-main-dir config) "\n"
   "DestDir=${MainDir}/completed\n"
   "TempDir=${MainDir}/tmp\n"
   "QueueDir=" (nzbget-configuration-data-dir config) "/queue\n"
   "LockFile=" (nzbget-configuration-data-dir config) "/nzbget.lock\n"
   "LogFile=" (nzbget-configuration-log-file config) "\n"
   "ControlIP=0.0.0.0\n"
   "ControlPort=" (number->string (nzbget-configuration-port config)) "\n"
   "ControlUsername=nzbget\n"
   "ControlPassword=tegzbN\n"
   "SecureControl=no\n"
   "WriteLog=append\n"
   "RotateLog=3\n"
   "AppendCategoryDir=yes\n"
   "DiskSpace=250\n"
   "ParCheck=auto\n"
   "UnpackPassFile=\n"
   "DirectUnpack=yes\n"))

(define (nzbget-accounts config)
  (list
   (user-account
    (name "nzbget")
    (group "media")
    (system? #t)
    (comment "NZBGet Usenet downloader daemon")
    (home-directory (nzbget-configuration-data-dir config))
    (shell (file-append shadow "/sbin/nologin")))))

(define (nzbget-activation config)
  (let ((data-dir (nzbget-configuration-data-dir config))
        (main-dir (nzbget-configuration-main-dir config))
        (conf     (nzbget-initial-config config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw       (getpwnam "nzbget"))
               (uid      (passwd:uid pw))
               (gid      (passwd:gid pw))
               (conf-file (string-append #$data-dir "/nzbget.conf")))
          (for-each (lambda (d)
                      (mkdir-p d)
                      (chown d uid gid))
                    (list #$data-dir
                          (string-append #$data-dir "/queue")
                          #$main-dir
                          (string-append #$main-dir "/completed")
                          (string-append #$main-dir "/tmp")))
          ;; Write config only on first boot so web-UI changes are preserved.
          (unless (file-exists? conf-file)
            (call-with-output-file conf-file
              (lambda (p) (display #$conf p)))
            (chown conf-file uid gid)
            (chmod conf-file #o640))))))

(define (nzbget-shepherd-service config)
  (let* ((pkg      (nzbget-configuration-package config))
         (data-dir (nzbget-configuration-data-dir config))
         (log-file (nzbget-configuration-log-file config)))
    (list
     (shepherd-service
      (provision '(nzbget))
      (documentation "NZBGet Usenet downloader.")
      (requirement '(networking file-systems))
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/nzbget")
                      "-c" (string-append #$data-dir "/nzbget.conf")
                      "-s")        ; server mode, foreground
                #:user "nzbget"
                #:group "media"
                #:log-file #$log-file))
      (stop #~(make-kill-destructor))))))

(define (nzbget-firewall-rules config)
  (nftables-rules
   (input (list (string-append
                 "tcp dport "
                 (number->string (nzbget-configuration-port config))
                 " accept comment \"nzbget\"")))))

(define (nzbget-profile config)
  (list (nzbget-configuration-package config)))

(define-public nzbget-service-type
  (service-type
   (name 'nzbget)
   (description "NZBGet Usenet binary downloader service.")
   (extensions
    (list
     (service-extension account-service-type       nzbget-accounts)
     (service-extension activation-service-type    nzbget-activation)
     (service-extension shepherd-root-service-type nzbget-shepherd-service)
     (service-extension firewall-service-type      nzbget-firewall-rules)
     (service-extension profile-service-type       nzbget-profile)))
   (default-value (nzbget-configuration))))
