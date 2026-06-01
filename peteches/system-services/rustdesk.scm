(define-module (peteches system-services rustdesk)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (peteches packages rustdesk)
  #:export (rustdesk-configuration
            rustdesk-configuration?
            rustdesk-configuration-package
            rustdesk-configuration-data-dir
            rustdesk-configuration-log-dir
            rustdesk-configuration-hbbs-port
            rustdesk-configuration-hbbr-port
            rustdesk-configuration-relay-servers
            rustdesk-configuration-key
            rustdesk-configuration-extra-hbbs-args
            rustdesk-configuration-extra-hbbr-args
            rustdesk-service-type))

;;; ── Configuration record ─────────────────────────────────────────────────

(define-record-type* <rustdesk-configuration>
  rustdesk-configuration make-rustdesk-configuration
  rustdesk-configuration?
  ;; Package providing hbbs and hbbr binaries.
  (package          rustdesk-configuration-package
                    (default rustdesk-server))
  ;; Working directory for hbbs; key files are generated here on first start.
  (data-dir         rustdesk-configuration-data-dir
                    (default "/var/lib/rustdesk"))
  (log-dir          rustdesk-configuration-log-dir
                    (default "/var/log/rustdesk"))
  ;; hbbs (ID/rendezvous server) listen port.
  (hbbs-port        rustdesk-configuration-hbbs-port
                    (default 21115))
  ;; hbbr (relay server) listen port.
  (hbbr-port        rustdesk-configuration-hbbr-port
                    (default 21117))
  ;; List of relay server addresses passed to hbbs via -r.
  ;; Leave empty to have hbbs announce its own address as relay.
  (relay-servers    rustdesk-configuration-relay-servers
                    (default '()))
  ;; Optional encryption key string passed to both hbbs and hbbr via -k.
  ;; #f means each server generates its own key on first start.
  (key              rustdesk-configuration-key
                    (default #f))
  ;; Extra command-line arguments for hbbs and hbbr respectively.
  (extra-hbbs-args  rustdesk-configuration-extra-hbbs-args
                    (default '()))
  (extra-hbbr-args  rustdesk-configuration-extra-hbbr-args
                    (default '())))

;;; ── Account helper ───────────────────────────────────────────────────────

(define (rustdesk-accounts cfg)
  (list
   (user-group
    (name "rustdesk")
    (system? #t))
   (user-account
    (name "rustdesk")
    (group "rustdesk")
    (system? #t)
    (comment "RustDesk server daemon")
    (home-directory (rustdesk-configuration-data-dir cfg))
    (shell (file-append shadow "/sbin/nologin")))))

;;; ── Activation helper ────────────────────────────────────────────────────

(define (rustdesk-activation cfg)
  (let ((data-dir (rustdesk-configuration-data-dir cfg))
        (log-dir  (rustdesk-configuration-log-dir  cfg)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw  (getpwnam "rustdesk"))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (for-each
           (lambda (d)
             (mkdir-p d)
             (chown d uid gid)
             (chmod d #o750))
           (list #$data-dir #$log-dir))))))

;;; ── Shepherd services ────────────────────────────────────────────────────

(define (rustdesk-shepherd-services cfg)
  (let* ((package      (rustdesk-configuration-package       cfg))
         (data-dir     (rustdesk-configuration-data-dir      cfg))
         (log-dir      (rustdesk-configuration-log-dir       cfg))
         (hbbs-port    (rustdesk-configuration-hbbs-port     cfg))
         (hbbr-port    (rustdesk-configuration-hbbr-port     cfg))
         (relay-servers (rustdesk-configuration-relay-servers cfg))
         (key          (rustdesk-configuration-key           cfg))
         (extra-hbbs   (rustdesk-configuration-extra-hbbs-args cfg))
         (extra-hbbr   (rustdesk-configuration-extra-hbbr-args cfg)))
    (list
     (shepherd-service
      (provision '(rustdesk-hbbs))
      (requirement '(user-processes networking))
      (documentation "RustDesk rendezvous/ID server (hbbs)")
      (start #~(make-forkexec-constructor
                (append
                 (list #$(file-append package "/bin/hbbs")
                       "-p" #$(number->string hbbs-port))
                 #$(if (pair? relay-servers)
                       (list "-r" (string-join relay-servers ","))
                       '())
                 #$(if key (list "-k" key) '())
                 '#$extra-hbbs)
                #:user "rustdesk"
                #:group "rustdesk"
                #:directory #$data-dir
                #:log-file #$(string-append log-dir "/hbbs.log")))
      (stop #~(make-kill-destructor)))

     (shepherd-service
      (provision '(rustdesk-hbbr))
      (requirement '(user-processes networking))
      (documentation "RustDesk relay server (hbbr)")
      (start #~(make-forkexec-constructor
                (append
                 (list #$(file-append package "/bin/hbbr")
                       "-p" #$(number->string hbbr-port))
                 #$(if key (list "-k" key) '())
                 '#$extra-hbbr)
                #:user "rustdesk"
                #:group "rustdesk"
                #:log-file #$(string-append log-dir "/hbbr.log")))
      (stop #~(make-kill-destructor))))))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public rustdesk-service-type
  (service-type
   (name 'rustdesk)
   (extensions
    (list
     (service-extension account-service-type       rustdesk-accounts)
     (service-extension activation-service-type    rustdesk-activation)
     (service-extension shepherd-root-service-type rustdesk-shepherd-services)))
   (default-value (rustdesk-configuration))
   (description
    "Run the RustDesk self-hosted servers: hbbs (rendezvous/ID) and
hbbr (relay).  Both run as the @code{rustdesk} system user.")))
