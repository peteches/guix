;;; bolt.scm — System service for the Thunderbolt authorization daemon (boltd)
;;;
;;; Minimal, reproducible, and Guix-idiomatic:
;;; - Runs boltd under Shepherd
;;; - Ensures its state dir exists
;;; - Adds `boltctl` to the system profile
;;; - No desktop assumptions; works headless or with polkit
;;;
;;; Usage (in your OS config):
;;;   (use-modules (peteches services bolt))
;;;   (services
;;;     (cons* (service boltd-service-type)
;;;            %desktop-services))     ; optional, but brings dbus + polkit
;;;
;;; Notes:
;;; - `boltd` talks to the *system* D-Bus. If you don’t use %desktop-services,
;;;   also add (service dbus-root-service-type) yourself.
;;; - Authorize/enroll devices once with `boltctl`.
;;;
(define-module (peteches services bolt)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages linux)     ; for `bolt`
  #:use-mofule (guix records)
  #:use-module (guix gexp)
  #:export (boltd-configuration
            boltd-configuration?
            boltd-service-type))

;; Configuration record (kept tiny on purpose).
(define-record-type* <boltd-configuration>
  boltd-configuration make-boltd-configuration
  boltd-configuration?
  ;; Package providing /bin/boltd and /bin/boltctl.
  (package         boltd-configuration-package
                   (default bolt))
  ;; Extra command-line args for boltd, if you ever need them.
  (extra-arguments boltd-configuration-extra-arguments
                   (default '())))

;; Shepherd service for boltd.
(define (boltd-shepherd-service cfg)
  (let* ((pkg    (boltd-configuration-package cfg))
         (daemon (file-append pkg "/bin/boltd"))
         (args   (boltd-configuration-extra-arguments cfg)))
    (shepherd-service
     (provision '(boltd))
     ;; D-Bus is needed for policy/authorization. If you don’t use
     ;; %desktop-services, also add (service dbus-root-service-type) to your
     ;; system’s (services ...) list.
     (requirement '(dbus))
     (documentation "Thunderbolt/USB4 authorization daemon (boltd).")
     (start #~(make-forkexec-constructor
               (list #$daemon #$@args)
               ;; Make sure /var/lib/boltd exists before starting.
               #:environment-variables
               (list "BOLT_STATE_DIR=/var/lib/boltd")))
     (stop  #~(make-kill-destructor)))))

;; Activation: create boltd state dir with safe permissions.
(define (boltd-activation _cfg)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/boltd")
      (chmod "/var/lib/boltd" #o700)))

;; Service type: install package + run shepherd + activation hook.
(define boltd-service-type
  (service-type
   (name 'boltd)
   (extensions
    (list
     ;; 1) Ensure `bolt` is available system-wide (for `boltctl`).
     (service-extension profile-service-type
                        (lambda (cfg) (list (boltd-configuration-package cfg))))
     ;; 2) Add the shepherd service.
     (service-extension shepherd-root-service-type
                        (lambda (cfg) (list (boltd-shepherd-service cfg))))
     ;; 3) Prepare state directory on activation.
     (service-extension activation-service-type
                        boltd-activation)))
   (default-value (boltd-configuration))
   (description
    "Run the Thunderbolt/USB4 authorization daemon (boltd).  This daemon
listens for Thunderbolt devices and, based on policy, authorizes/enrolls them
so that PCIe/DisplayPort tunnels can be established.  Use `boltctl` to enroll
trusted docks or devices.  Requires the system D-Bus.")))
