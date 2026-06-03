;; peteches/system-services/nzbget.scm — NZBGet Usenet downloader service.

(define-module (peteches system-services nzbget)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (peteches packages nzbget)
  #:use-module (peteches system-services firewall)
  #:use-module (peteches system-services media-accounts)
  #:export (nzbget-configuration
            nzbget-configuration?
            nzbget-service-type))

(define-record-type* <nzbget-configuration>
  nzbget-configuration make-nzbget-configuration
  nzbget-configuration?
  (package              nzbget-configuration-package              (default nzbget))
  (port                 nzbget-configuration-port                 (default 6789))
  (data-dir             nzbget-configuration-data-dir             (default "/var/lib/nzbget"))
  (main-dir             nzbget-configuration-main-dir             (default "/media/downloads/usenet"))
  (nzb-dir              nzbget-configuration-nzb-dir              (default ""))
  (dest-dir             nzbget-configuration-dest-dir             (default #f))
  (temp-dir             nzbget-configuration-temp-dir             (default #f))
  (log-file             nzbget-configuration-log-file             (default "/var/log/nzbget.log"))
  (username             nzbget-configuration-username             (default "nzbget"))
  (password-file        nzbget-configuration-password-file        (default #f))
  (disk-space           nzbget-configuration-disk-space           (default 250))
  (par-check            nzbget-configuration-par-check            (default "auto"))
  (direct-unpack?       nzbget-configuration-direct-unpack?       (default #t))
  (append-category-dir? nzbget-configuration-append-category-dir? (default #t)))

(define (bool->nzbget b)
  (if b "yes" "no"))

(define (nzbget-initial-config config)
  "Return a string with the initial nzbget.conf content (WebDir is written separately)."
  (let* ((main-dir  (nzbget-configuration-main-dir config))
         (dest-dir  (or (nzbget-configuration-dest-dir config)
                        (string-append "${MainDir}/completed")))
         (temp-dir  (or (nzbget-configuration-temp-dir config)
                        (string-append "${MainDir}/tmp"))))
    (string-append
     "MainDir="  main-dir  "\n"
     "DestDir="  dest-dir  "\n"
     "TempDir="  temp-dir  "\n"
     "NzbDir="   (nzbget-configuration-nzb-dir config) "\n"
     "QueueDir=" (nzbget-configuration-data-dir config) "/queue\n"
     "LockFile=" (nzbget-configuration-data-dir config) "/nzbget.lock\n"
     "LogFile="  (nzbget-configuration-log-file config) "\n"
     "ControlIP=0.0.0.0\n"
     "ControlPort=" (number->string (nzbget-configuration-port config)) "\n"
     "ControlUsername=" (nzbget-configuration-username config) "\n"
     "SecureControl=no\n"
     "WriteLog=append\n"
     "RotateLog=3\n"
     "AppendCategoryDir=" (bool->nzbget (nzbget-configuration-append-category-dir? config)) "\n"
     "DiskSpace=" (number->string (nzbget-configuration-disk-space config)) "\n"
     "ParCheck=" (nzbget-configuration-par-check config) "\n"
     "UnpackPassFile=\n"
     "DirectUnpack=" (bool->nzbget (nzbget-configuration-direct-unpack? config)) "\n")))

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
  (let* ((pkg      (nzbget-configuration-package config))
         (data-dir (nzbget-configuration-data-dir config))
         (main-dir (nzbget-configuration-main-dir config))
         (dest-dir (or (nzbget-configuration-dest-dir config)
                       (string-append main-dir "/completed")))
         (temp-dir (or (nzbget-configuration-temp-dir config)
                       (string-append main-dir "/tmp")))
         (nzb-dir  (nzbget-configuration-nzb-dir config))
         (web-dir  (file-append pkg "/share/nzbget/webui"))
         (conf     (nzbget-initial-config config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw        (getpwnam "nzbget"))
               (uid       (passwd:uid pw))
               (gid       (passwd:gid pw))
               (conf-file (string-append #$data-dir "/nzbget.conf")))
          (for-each (lambda (d)
                      (mkdir-p d)
                      (chown d uid gid))
                    (append
                     (list #$data-dir
                           (string-append #$data-dir "/queue")
                           #$main-dir
                           #$dest-dir
                           #$temp-dir)
                     (if (string-null? #$nzb-dir) '() (list #$nzb-dir))))
          (call-with-output-file conf-file
            (lambda (p)
              (display #$conf p)
              (display (string-append "WebDir=" #$web-dir "\n") p)))
          (chown conf-file uid gid)
          (chmod conf-file #o640)))))

(define (nzbget-shepherd-service config)
  (let* ((pkg           (nzbget-configuration-package config))
         (data-dir      (nzbget-configuration-data-dir config))
         (main-dir      (nzbget-configuration-main-dir config))
         (dest-dir      (or (nzbget-configuration-dest-dir config)
                            (string-append main-dir "/completed")))
         (temp-dir      (or (nzbget-configuration-temp-dir config)
                            (string-append main-dir "/tmp")))
         (nzb-dir       (nzbget-configuration-nzb-dir config))
         (log-file      (nzbget-configuration-log-file config))
         (port          (nzbget-configuration-port config))
         (username      (nzbget-configuration-username config))
         (disk-space    (nzbget-configuration-disk-space config))
         (par-check     (nzbget-configuration-par-check config))
         (direct-unpack? (nzbget-configuration-direct-unpack? config))
         (append-cat?   (nzbget-configuration-append-category-dir? config))
         (password-file (nzbget-configuration-password-file config))
         (requirements  (if password-file
                            '(networking file-systems sops-secrets)
                            '(networking file-systems))))
    (list
     (shepherd-service
      (provision '(nzbget))
      (documentation "NZBGet Usenet downloader.")
      (requirement requirements)
      (start #~(lambda _
                 (use-modules (ice-9 rdelim))
                 (let* ((pw-file #$password-file)
                        (pw-opt  (if pw-file
                                     (list "--option"
                                           (string-append "ControlPassword="
                                                          (string-trim-right
                                                           (call-with-input-file pw-file read-line))))
                                     '())))
                   ((make-forkexec-constructor
                     (append
                      (list #$(file-append pkg "/bin/nzbget")
                            "--daemon"
                            "--configfile" (string-append #$data-dir "/nzbget.conf")
                            "--option" (string-append "MainDir="           #$main-dir)
                            "--option" (string-append "DestDir="           #$dest-dir)
                            "--option" (string-append "TempDir="           #$temp-dir)
                            "--option" (string-append "NzbDir="            #$nzb-dir)
                            "--option" (string-append "WebDir="            #$(file-append pkg "/share/nzbget/webui"))
                            "--option" (string-append "ConfigTemplate="    #$(file-append pkg "/share/nzbget/webui") "/nzbget.conf")
                            "--option" (string-append "ControlPort="       #$(number->string port))
                            "--option" (string-append "ControlUsername="   #$username)
                            "--option" (string-append "DiskSpace="         #$(number->string disk-space))
                            "--option" (string-append "ParCheck="          #$par-check)
                            "--option" (string-append "DirectUnpack="      #$(bool->nzbget direct-unpack?))
                            "--option" (string-append "AppendCategoryDir=" #$(bool->nzbget append-cat?)))
                      pw-opt)
                     #:user "nzbget"
                     #:group "media"
                     #:pid-file (string-append #$data-dir "/nzbget.lock")
                     #:log-file #$log-file
                     #:environment-variables
                     '("PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin"
                       "TERM=dumb"))))))
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
