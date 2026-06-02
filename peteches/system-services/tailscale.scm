;; (peteches system-services tailscale)
;; Tailscale for Guix System.
;;
;; Runs tailscaled directly in the default network namespace.
;; Multiple instances are supported via compose/extend append.
;;
;; Bring up a tailnet:
;;   tailscale --socket=/run/tailscale/peteches/tailscaled.sock up
;;
(define-module (peteches system-services tailscale)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages base)          ; coreutils (chown for taildrop)
  #:use-module (peteches packages tailscale)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (tailscale-instance-configuration
            tailscale-instance-configuration?
            tailscale-service-type))

;; Tun device names must be <= 15 chars (IFNAMSIZ - 1).
(define (ts-tun-name name)
  (let* ((raw (string-append "ts-" name))
         (maxlen 15))
    (if (<= (string-length raw) maxlen)
        raw
        (string-copy raw 0 maxlen))))

(define-record-type* <tailscale-instance-configuration>
  tailscale-instance-configuration make-tailscale-instance-configuration
  tailscale-instance-configuration?
  (name          tailscale-instance-configuration-name
                 (default "default"))
  (package       tailscale-instance-configuration-package
                 (default tailscale))

  ;; optional path overrides (default to /var/lib/tailscale/<name>/... )
  (state-file    tailscale-instance-configuration-state-file
                 (default #f))
  (socket-file   tailscale-instance-configuration-socket-file
                 (default #f))
  (tun           tailscale-instance-configuration-tun
                 (default #f))
  (port          tailscale-instance-configuration-port
                 (default #f))
  (log-file      tailscale-instance-configuration-log-file
                 (default #f))
  (extra-args    tailscale-instance-configuration-extra-args
                 (default '()))

  ;; Persistent preferences applied via `tailscale set' after daemon start.
  (accept-dns    tailscale-instance-configuration-accept-dns
                 (default #f))
  (accept-routes tailscale-instance-configuration-accept-routes
                 (default #t))

  ;; Taildrop receive: set taildrop-dir to enable the file-get polling service.
  (taildrop-dir      tailscale-instance-configuration-taildrop-dir
                     (default #f))
  (taildrop-user     tailscale-instance-configuration-taildrop-user
                     (default #f))
  (taildrop-schedule tailscale-instance-configuration-taildrop-schedule
                     (default "*/1 * * * *"))

  ;; When set to a file path, a one-shot shepherd service calls
  ;; `tailscale up --auth-key=<contents>' on first boot.
  (auth-key-file tailscale-instance-configuration-auth-key-file
                 (default #f)))

(define (instance-default-paths name)
  (let ((base (string-append "/var/lib/tailscale/" name)))
    (list (string-append base "/tailscaled.state")
          (string-append "/var/log/tailscaled-" name ".log")
          (ts-tun-name name))))

(define (instance->resolved cfg)
  (match cfg
    (($ <tailscale-instance-configuration>
        name package state-file socket-file tun port log-file extra-args
        accept-dns accept-routes taildrop-dir taildrop-user taildrop-schedule auth-key-file)
     (let* ((defaults (instance-default-paths name))
            (dstate   (list-ref defaults 0))
            (dlog     (list-ref defaults 1))
            (dtun     (list-ref defaults 2)))
       (tailscale-instance-configuration
        (name name)
        (package package)
        (state-file    (or state-file dstate))
        (socket-file   socket-file)          ; #f means use tailscaled's built-in default
        (tun           (or tun dtun))
        (port          port)
        (log-file      (or log-file dlog))
        (extra-args    extra-args)
        (accept-dns    accept-dns)
        (accept-routes accept-routes)
        (taildrop-dir      taildrop-dir)
        (taildrop-user     taildrop-user)
        (taildrop-schedule taildrop-schedule)
        (auth-key-file     auth-key-file))))))

(define (tailscale-instance->shepherd-service cfg)
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package state-file socket-file tun port log-file extra-args
          _ _ _ _ _ _)
       (let* ((service-name (string->symbol (string-append "tailscaled-" name)))
              (run-args
               (append (list (file-append package "/bin/tailscaled")
                             (string-append "--statedir=/var/lib/tailscale/" name))
                       (if socket-file
                           (list (string-append "--socket=" socket-file))
                           '())
                       (list (string-append "--tun=" tun))
                       (if port
                           (list (string-append "--port=" (number->string port)))
                           '())
                       extra-args)))
         (shepherd-service
          (provision (list service-name))
          (auto-start? #t)
          (one-shot? #f)
          (documentation (string-append "Tailscale daemon (" name ")"))
          (requirement '(user-processes networking))
          (start #~(make-forkexec-constructor
                    (list #$@run-args)
                    #:log-file #$log-file
                    #:environment-variables
                    (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
          (stop #~(make-kill-destructor))))))))

(define (tailscale-instance->prefs-service cfg)
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package state-file socket-file tun port log-file extra-args
          accept-dns accept-routes _ _ _ auth-key-file)
       (let* ((svc-name  (string->symbol (string-append "tailscale-prefs-" name)))
              (tsd-svc   (string->symbol (string-append "tailscaled-" name)))
              (up-svc    (string->symbol (string-append "tailscale-up-" name)))
              (tailscale (file-append package "/bin/tailscale"))
              (requires  (if auth-key-file
                             (list 'user-processes 'networking tsd-svc up-svc)
                             (list 'user-processes 'networking tsd-svc)))
              (args      (append
                          (list tailscale)
                          (if socket-file
                              (list (string-append "--socket=" socket-file))
                              '())
                          (list "set"
                                (string-append "--accept-dns="
                                               (if accept-dns "true" "false"))
                                (string-append "--accept-routes="
                                               (if accept-routes "true" "false"))))))
         (shepherd-service
          (provision (list svc-name))
          (documentation (string-append "Apply persisted Tailscale prefs for instance " name))
          (requirement requires)
          (one-shot? #t)
          (auto-start? #t)
          (start #~(make-forkexec-constructor
                    (list #$@args)
                    #:log-file #$(string-append "/var/log/tailscale-prefs-" name ".log")
                    #:environment-variables
                    (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
          (stop #~(lambda _ #t))))))))

(define (tailscale-instance->taildrop-service cfg)
  "Return a scheduled shepherd service that retrieves Taildrop files, or #f."
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package state-file socket-file tun port log-file extra-args
          accept-dns accept-routes taildrop-dir taildrop-user taildrop-schedule _)
       (if (not taildrop-dir)
           #f
           (let* ((svc-name  (string->symbol (string-append "tailscale-taildrop-" name)))
                  (tsd-svc   (string->symbol (string-append "tailscaled-" name)))
                  (tailscale (file-append package "/bin/tailscale"))
                  (chown-bin (file-append coreutils "/bin/chown"))
                  (lf        (string-append "/var/log/tailscale-taildrop-" name ".log"))
                  (get-script
                   (program-file
                    (string-append "tailscale-taildrop-get-" name)
                    #~(begin
                        (let ((rc (apply system*
                                         (append
                                          (list #$tailscale)
                                          (if #$socket-file
                                              (list (string-append "--socket=" #$socket-file))
                                              '())
                                          (list "file" "get" #$taildrop-dir)))))
                          (when (and (zero? rc) #$(if taildrop-user #t #f))
                            (system* #$chown-bin "-R"
                                     #$(if taildrop-user taildrop-user "")
                                     #$taildrop-dir))
                          (exit (if (zero? rc) 0 1)))))))
             (shepherd-service
              (provision (list svc-name))
              (documentation
               (string-append "Poll Taildrop files for Tailscale instance " name
                              " into " taildrop-dir
                              ". Trigger: herd trigger " (symbol->string svc-name)))
              (requirement (list 'user-processes tsd-svc))
              (modules '((shepherd service timer)))
              (start #~(make-timer-constructor
                        (cron-string->calendar-event #$taildrop-schedule)
                        (command (list #$get-script))))
              (stop #~(make-timer-destructor))
              (actions
               (list (shepherd-action
                      (name 'trigger)
                      (documentation "Retrieve Taildrop files immediately.")
                      (procedure #~(lambda (running . _)
                                     (system* #$get-script)
                                     running)))))))))))  )

(define (tailscale-instance->up-service cfg)
  "Return a one-shot shepherd service that calls `tailscale up --auth-key=…'
when auth-key-file is set, or #f otherwise."
  (let ((cfg (instance->resolved cfg)))
    (match cfg
      (($ <tailscale-instance-configuration>
          name package state-file socket-file tun port log-file extra-args
          accept-dns accept-routes _ _ _ auth-key-file)
       (if (not auth-key-file)
           #f
           (let* ((svc-name  (string->symbol (string-append "tailscale-up-" name)))
                  (tsd-svc   (string->symbol (string-append "tailscaled-" name)))
                  (tailscale (file-append package "/bin/tailscale"))
                  (up-script
                   (program-file
                    (string-append "tailscale-up-" name)
                    #~(begin
                        (use-modules (ice-9 rdelim))
                        (let* ((key (string-trim-right
                                     (call-with-input-file #$auth-key-file read-line)))
                               (rc  (apply system*
                                           (append
                                            (list #$tailscale)
                                            (if #$socket-file
                                                (list (string-append "--socket=" #$socket-file))
                                                '())
                                            (list "up"
                                                  (string-append "--auth-key=" key))))))
                          (exit (if (zero? rc) 0 1)))))))
             (shepherd-service
              (provision (list svc-name))
              (documentation
               (string-append "Enroll Tailscale instance " name " using auth-key from file"))
              (requirement (list 'user-processes 'networking 'sops-secrets tsd-svc))
              (one-shot? #t)
              (auto-start? #t)
              (start #~(make-forkexec-constructor
                        (list #$up-script)
                        #:log-file #$(string-append "/var/log/tailscale-up-" name ".log")
                        #:environment-variables
                        (list "PATH=/run/setuid-programs:/run/current-system/profile/bin:/run/current-system/profile/sbin")))
              (stop #~(lambda _ #t)))))))))

(define (tailscale-profile-entries instances)
  (let ((resolved (map instance->resolved instances)))
    (delete-duplicates
     (map tailscale-instance-configuration-package resolved)
     eq?)))

(define (tailscale-activation instances)
  (let ((resolved (map instance->resolved instances)))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p "/var/log")
        (mkdir-p "/run/tailscale")
        (mkdir-p "/var/lib/tailscale")
        #$@(map
            (lambda (cfg)
              (match cfg
                (($ <tailscale-instance-configuration>
                    name _ state-file socket-file _ _ _ _ _ _ taildrop-dir taildrop-user _ _)
                 (let ((chown-bin (file-append coreutils "/bin/chown")))
                   #~(begin
                       (mkdir-p (dirname #$state-file))
                       (when #$socket-file
                         (mkdir-p (dirname #$socket-file)))
                       (when #$taildrop-dir
                         (mkdir-p #$taildrop-dir)
                         (when #$taildrop-user
                           (system* #$chown-bin #$taildrop-user #$taildrop-dir))))))))
            resolved))))

(define (tailscale-shepherd-services instances)
  (append
   (filter-map tailscale-instance->taildrop-service instances)
   (filter-map tailscale-instance->up-service instances)
   (map tailscale-instance->shepherd-service instances)
   (map tailscale-instance->prefs-service instances)))

(define-public tailscale-service-type
  (service-type
   (name 'tailscale)
   (compose append)
   (extend append)
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailscale-shepherd-services)
          (service-extension activation-service-type
                             tailscale-activation)
          (service-extension profile-service-type
                             tailscale-profile-entries)))
   (default-value '())
   (description "Run one or more Tailscale instances.")))
