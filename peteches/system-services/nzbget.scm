;; peteches/system-services/nzbget.scm — NZBGet Usenet downloader service.

(define-module (peteches system-services nzbget)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (peteches packages nzbget)
  #:use-module (peteches system-services firewall)
  #:use-module (peteches system-services media-accounts)
  #:export (nzbget-category
            nzbget-category?
            nzbget-news-server
            nzbget-news-server?
            nzbget-configuration
            nzbget-configuration?
            nzbget-service-type))

(define-record-type* <nzbget-category>
  nzbget-category make-nzbget-category
  nzbget-category?
  (name       nzbget-category-name)
  (dest-dir   nzbget-category-dest-dir   (default #f))
  (unpack?    nzbget-category-unpack?    (default #t))
  (extensions nzbget-category-extensions (default '())))

(define-record-type* <nzbget-news-server>
  nzbget-news-server make-nzbget-news-server
  nzbget-news-server?
  (name          nzbget-news-server-name)
  (host          nzbget-news-server-host)
  (port          nzbget-news-server-port          (default 563))
  (username      nzbget-news-server-username      (default ""))
  (password-file nzbget-news-server-password-file (default #f))
  (connections   nzbget-news-server-connections   (default 8))
  (encryption?   nzbget-news-server-encryption?   (default #t))
  (retention     nzbget-news-server-retention     (default 0))
  (level         nzbget-news-server-level         (default 0))
  (optional?     nzbget-news-server-optional?     (default #f))
  (active?       nzbget-news-server-active?       (default #t)))

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
  (append-category-dir? nzbget-configuration-append-category-dir? (default #t))
  (categories           nzbget-configuration-categories           (default '()))
  (news-servers         nzbget-configuration-news-servers         (default '())))

(define (bool->nzbget b)
  (if b "yes" "no"))

(define (render-nzbget-categories categories)
  "Return a config string fragment for CATEGORIES, numbered from 1."
  (let loop ((cats categories) (n 1) (result ""))
    (if (null? cats)
        result
        (let* ((cat      (car cats))
               (num      (number->string n))
               (name     (nzbget-category-name cat))
               (dest-dir (nzbget-category-dest-dir cat))
               (unpack?  (nzbget-category-unpack? cat))
               (exts     (nzbget-category-extensions cat))
               (chunk
                (string-append
                 "Category" num ".Name="       name "\n"
                 (if dest-dir
                     (string-append "Category" num ".DestDir=" dest-dir "\n")
                     "")
                 "Category" num ".Unpack="     (bool->nzbget unpack?) "\n"
                 "Category" num ".Extensions=" (string-join exts ",") "\n")))
          (loop (cdr cats) (+ n 1) (string-append result chunk))))))

(define (render-nzbget-news-servers servers)
  "Return a config string fragment for news SERVERS, numbered from 1.
Passwords are omitted and injected at startup via --option."
  (let loop ((svrs servers) (n 1) (result ""))
    (if (null? svrs)
        result
        (let* ((s   (car svrs))
               (num (number->string n))
               (chunk
                (string-append
                 "Server" num ".Active="      (bool->nzbget (nzbget-news-server-active? s))       "\n"
                 "Server" num ".Name="        (nzbget-news-server-name s)                         "\n"
                 "Server" num ".Host="        (nzbget-news-server-host s)                         "\n"
                 "Server" num ".Port="        (number->string (nzbget-news-server-port s))        "\n"
                 "Server" num ".Username="    (nzbget-news-server-username s)                     "\n"
                 "Server" num ".Connections=" (number->string (nzbget-news-server-connections s)) "\n"
                 "Server" num ".Encryption="  (bool->nzbget (nzbget-news-server-encryption? s))   "\n"
                 "Server" num ".Retention="   (number->string (nzbget-news-server-retention s))   "\n"
                 "Server" num ".Level="       (number->string (nzbget-news-server-level s))       "\n"
                 "Server" num ".Optional="    (bool->nzbget (nzbget-news-server-optional? s))     "\n"
                 "Server" num ".Password=" "\n")))
          (loop (cdr svrs) (+ n 1) (string-append result chunk))))))

(define (nzbget-initial-config config)
  "Return a string with the initial nzbget.conf content (WebDir is written separately)."
  (let* ((main-dir   (nzbget-configuration-main-dir config))
         (dest-dir   (or (nzbget-configuration-dest-dir config)
                         (string-append "${MainDir}/completed")))
         (temp-dir   (or (nzbget-configuration-temp-dir config)
                         (string-append "${MainDir}/tmp")))
         (categories (nzbget-configuration-categories config))
         (news-servers (nzbget-configuration-news-servers config)))
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
     "DirectUnpack=" (bool->nzbget (nzbget-configuration-direct-unpack? config)) "\n"
     (render-nzbget-categories categories)
     (render-nzbget-news-servers news-servers))))

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
  (let* ((pkg        (nzbget-configuration-package config))
         (data-dir   (nzbget-configuration-data-dir config))
         (main-dir   (nzbget-configuration-main-dir config))
         (dest-dir   (or (nzbget-configuration-dest-dir config)
                         (string-append main-dir "/completed")))
         (temp-dir   (or (nzbget-configuration-temp-dir config)
                         (string-append main-dir "/tmp")))
         (nzb-dir    (nzbget-configuration-nzb-dir config))
         (web-dir    (file-append pkg "/share/nzbget/webui"))
         (conf       (nzbget-initial-config config))
         (cat-dirs   (filter string?
                             (map nzbget-category-dest-dir
                                  (nzbget-configuration-categories config)))))
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
                     (if (string-null? #$nzb-dir) '() (list #$nzb-dir))
                     '#$cat-dirs))
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
         (news-servers  (nzbget-configuration-news-servers config))
         (server-pw-pairs
          (let loop ((svrs news-servers) (n 1) (acc '()))
            (if (null? svrs)
                (reverse acc)
                (let ((pf (nzbget-news-server-password-file (car svrs))))
                  (loop (cdr svrs) (+ n 1)
                        (if pf (cons (cons (number->string n) pf) acc) acc))))))
         (requirements  (if (or password-file (not (null? server-pw-pairs)))
                            '(networking file-systems sops-secrets)
                            '(networking file-systems))))
    (list
     (shepherd-service
      (provision '(nzbget))
      (documentation "NZBGet Usenet downloader.")
      (requirement requirements)
      (start #~(lambda _
                 (let* ((pw-file #$password-file)
                        (pw-opt  (if pw-file
                                     (list "--option"
                                           (string-append "ControlPassword="
                                                          (string-trim-right
                                                           (call-with-input-file pw-file
                                                             (@ (ice-9 rdelim) read-line)))))
                                     '())))
                   ;; Inject server passwords into config file (sops-secrets is a requirement)
                   (for-each
                    (lambda (pair)
                      (let* ((key   (string-append "Server" (car pair) ".Password="))
                             (pw    (string-trim-right
                                     (call-with-input-file (cdr pair)
                                       (@ (ice-9 rdelim) read-line))))
                             (conf  (string-append #$data-dir "/nzbget.conf"))
                             (lines (call-with-input-file conf
                                      (lambda (port)
                                        (let loop ((line ((@ (ice-9 rdelim) read-line) port))
                                                   (acc '()))
                                          (if (eof-object? line)
                                              (reverse acc)
                                              (loop ((@ (ice-9 rdelim) read-line) port)
                                                    (cons line acc))))))))
                        (call-with-output-file conf
                          (lambda (p)
                            (for-each
                             (lambda (line)
                               (display (if (string=? line key)
                                            (string-append key pw)
                                            line)
                                        p)
                               (newline p))
                             lines)))))
                    '#$server-pw-pairs)
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
  (append (map specification->package
	       '("unrar"
		 "7zip"
		 "par2cmdline"))
	  (list
	   (nzbget-configuration-package config))))

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
