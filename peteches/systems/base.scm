;; base.scm — reusable base with grouped flags, gtkgreet greeter, and sane defaults

(define-module (peteches systems base)
  ;; Core Guix/Guile
  #:use-module (gnu)
  #:use-module (guix gexp)

  ;; Services (service constructors + specific service types)
  #:use-module (gnu services)            ; <-- provides 'service', 'simple-service', etc.
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm)
  #:use-module (gnu services networking) ; tor-service-type
  #:use-module (gnu services ssh)        ; openssh-service-type
  #:use-module (gnu services cups)       ; cups-service-type

  ;; Packages used in helpers
  #:use-module (gnu packages admin)
  #:use-module (gnu packages display-managers) ; gtkgreet
  #:use-module (gnu packages wm)               ; cage
  #:use-module (gnu packages linux)            ; linux-firmware, intel-microcode
  
  #:use-module (nongnu packages nvidia)        ; nvidia-firmware
  #:use-module (nongnu packages linux)
  #:export (make-base-os
            %peteches-user
            %common-services
            greetd-gtkgreet-service
            without-gdm
            nonguix-substitute-service))

;; ---------- Common building blocks ----------

(define %peteches-user
  (user-account
   (name "peteches")
   (comment "Pete McCabe")
   (group "users")
   (home-directory "/home/peteches")
   (supplementary-groups '("wheel" "netdev" "audio" "video"))))

;; NOTE: Do NOT add elogind here; it already comes with %desktop-services.
(define %common-services
  (list (service openssh-service-type)
        (service tor-service-type)
        (service gpm-service-type)))

(define (without-gdm)
  (modify-services %desktop-services (delete gdm-service-type)))

;; ------ Greeter (new interface) ------
;; Preferred for up-to-date Guix: configure the greeter at top level.
;; Runs gtkgreet inside cage; *no* dbus-run-session — user sessions
;; are expected to have DBus via dbus-service-type.
(define* (greetd-gtkgreet-service #:key (vt "7"))
  (service greetd-service-type
    (greetd-configuration
      (greeter-supplementary-groups '("video" "input"))
      (terminals
       (list
        (greetd-terminal-configuration
          (terminal-vt vt)
          (terminal-switch #t)
	  (default-session-command
	    (greetd-agreety-session
	     (command (file-append cage "/bin/cage"))
	     (command-args  (list "-s" "--"
                (file-append gtkgreet "/bin/gtkgreet")
                "--remember"))))))))))

(define (nonguix-substitute-service)
  (simple-service 'add-nonguix-substitutes
                  guix-service-type
                  (guix-extension
                   (substitute-urls
                    (append (list "https://substitutes.nonguix.org")
                            %default-substitute-urls))
                   (authorized-keys
                    (append (list (plain-file "non-guix.pub"
					            "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                            %default-authorized-guix-keys)))))

;; ---------- Main API with grouped flags ----------

(define* (make-base-os
          #:key host-name bootloader file-systems
               (mapped-devices '())
               (kernel linux)
               (firmware '())
               (users-extra '())
               (extra-services '())
               (extra-packages '())
               ;; flags
               (laptop? #f)
               (intel-cpu? #t)
               (with-printing? #f)
               (with-bluetooth? #f)
               (with-nonguix? #f)
               (with-nvidia? #f)
               (with-intel-microcode? #f))
  (let* ((firmware*
          (append firmware
                  (if with-intel-microcode? (list intel-microcode) '())))
         (packages*
          (append extra-packages
                  (if with-nvidia? (list nvidia-firmware) '())))
         (laptop-services
          (append (if laptop? (list (service tlp-service-type)) '())
                  (if (and laptop? intel-cpu?)
                      (list (service thermald-service-type))
                      '())))
         (printing-services (if with-printing? (list (service cups-service-type)) '()))
         (bluetooth-services (if with-bluetooth? (list (service bluetooth-service-type)) '()))
         (nonguix-services (if with-nonguix? (list (nonguix-substitute-service)) '()))
         (desktop* (without-gdm))
         (final-services
          (append desktop*
                  %common-services
                  (list (greetd-gtkgreet-service))
                  laptop-services
                  printing-services
                  bluetooth-services
                  nonguix-services
                  extra-services)))
    (operating-system
      (kernel kernel)
      (firmware firmware*)
      (locale "en_GB.utf8")
      (timezone "Europe/London")
      (keyboard-layout (keyboard-layout "us"))
      (host-name host-name)
      (users (append (list %peteches-user)
                     %base-user-accounts
                     users-extra))
      (packages (append packages* %base-packages))
      (services final-services)
      (mapped-devices mapped-devices)
      (file-systems (append file-systems %base-file-systems))
      (bootloader bootloader))))

