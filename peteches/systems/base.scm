;; base.scm — reusable base with grouped flags, gtkgreet greeter, and sane defaults

(define-module (peteches systems base)
  ;; Core Guix/Guile
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  ;; Services (service constructors + specific service types)
  #:use-module (gnu services) ; <-- provides 'service', 'simple-service', etc.
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)
  #:use-module (gnu services pm)
  #:use-module (gnu services networking) ; tor-service-type
  #:use-module (gnu services ssh)        ; openssh-service-type
  #:use-module (gnu services cups)       ; cups-service-type

  ;; Packages used in helpers
  #:use-module (gnu packages admin)
  #:use-module (gnu packages display-managers) ; gtkgreet
  #:use-module (gnu packages wm)               ; cage

  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)  ; linux-firmware, intel-microcode

  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages nvidia)	; nvidia-firmware

  #:use-module (nongnu packages linux)

  #:use-module (peteches system-services boltd)
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

(define transform
  (options->transformation
   '((with-graft . "mesa=nvda"))))

;; NOTE: Do NOT add elogind here; it already comes with %desktop-services.
(define %common-services
  (list (service openssh-service-type)
        (service tor-service-type)
	(service boltd-service-type)
        (service gpm-service-type)))

(define %common-packages
  (list font-terminus))

(define (without-gdm)
  (modify-services
   %desktop-services
   (delete gdm-service-type)
   (guix-service-type
    config => (guix-configuration
	       (inherit config)
	       (substitute-urls
		(append (list "http://nug.peteches.co.uk:3000")
			%default-substitute-urls))
	       (authorized-keys
		(append (list (local-file "./nug-substitute-key.pub"))
			%default-authorized-guix-keys))))))

;; ------ Greeter (new interface) ------
;; Preferred for up-to-date Guix: configure the greeter at top level.
;; Runs gtkgreet inside cage; *no* dbus-run-session — user sessions
;; are expected to have DBus via dbus-service-type.
(define* greetd-gtkgreet-services
  ;; in your operating-system's (services ...)

  ;; In your operating-system's (services ...) field:
  (list
   ;; needs: (use-modules (gnu services base) (gnu packages fonts))
   (service console-font-service-type
    `(("tty7" . ,(file-append font-terminus "/share/consolefonts/ter-132n"))))


   ;; In your operating-system's (services ...)
   (service greetd-service-type
	    (greetd-configuration
	     ;; run greetd on tty1 with agreety (text greeter)
	     (terminals
	      (list
	       (greetd-terminal-configuration
		(terminal-vt "7")
		(terminal-switch #t)
		(default-session-command
		  (greetd-agreety-session
		   ;; Start Hyprland for the logged-in user
		   (command
		    (greetd-user-session
		     (xdg-session-type "wayland")
		     (extra-env '(("XDG_CURRENT_DESKTOP" . "Hyprland")
				  ("XDG_SESSION_TYPE"    . "wayland")))
		     ;; Command must be a gexp that yields an argv list
		     (command #~(string-append #$(file-append hyprland "/bin/Hyprland")))
		     (command-args '()))))))))))))

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
	  (with-docker? #f)
          (with-printing? #f)
          (with-bluetooth? #f)
          (with-nonguix? #f)
          (with-nvidia? #f))
  (let* ((firmware*
          (append firmware
                  (if intel-cpu? (list intel-microcode) '())))
         (packages*
          (append extra-packages
                  (if with-nvidia? (list nvidia-firmware nvidia-driver) '())))
         (laptop-services
          (append (if laptop? (list (service tlp-service-type)) '())
                  (if (and laptop? intel-cpu?)
                      (list (service thermald-service-type))
                      '())))
	 (nvidia-services (if with-nvidia?
			      (list
			       (simple-service 'nvidia-ldpath session-environment-service-type
					       ;; Prepend nvidia-driver’s lib dirs; keep any existing value.
					       `(("LD_LIBRARY_PATH" .
						  ,#~(string-append
						      #$(file-append nvidia-driver "/lib") ":"
						      #$(file-append nvidia-driver "/lib64")
						      ":${LD_LIBRARY_PATH}"))))
			       
			       (service nvidia-service-type)
			       (simple-service 'custom-udev-rules udev-service-type
					       (list nvidia-driver))
			       (service kernel-module-loader-service-type
					'("ipmi_devinf"
					  "nvidia"
					  "nvidia_modeset"
					  "nvidia_uvm")))
			      '()))
	 (docker-services (if with-docker? (list (service containerd-service-type) (service docker-service-type)) '()))
         (printing-services (if with-printing? (list (service cups-service-type)) '()))
         (bluetooth-services (if with-bluetooth? (list (service bluetooth-service-type)) '()))
         (nonguix-services (if with-nonguix? (list (nonguix-substitute-service)) '()))
         (desktop* (without-gdm))
         (final-services
          (append desktop*
                  %common-services
                  greetd-gtkgreet-services
                  laptop-services
                  printing-services
                  bluetooth-services
                  nonguix-services
		  nvidia-services
		  docker-services
                  extra-services)))
    (operating-system
     (kernel kernel)
     (kernel-arguments (append (if with-nvidia?
				   (list "modprobe.blacklist=nouveau" "nvidia_drm.modeset=1")
				   '())
			       %default-kernel-arguments))
     (kernel-loadable-modules (if with-nvidia?
				  (list nvidia-module)
				  '()))
     (firmware firmware*)
     (locale "en_GB.utf8")
     (timezone "Europe/London")
     (keyboard-layout (keyboard-layout "us"))
     (host-name host-name)
     (users (append (list (if with-docker?
			      (user-account
			       (inherit %peteches-user)
			       (supplementary-groups
				(append (user-account-supplementary-groups %peteches-user)
					'("docker"))))
			      %peteches-user))
                    %base-user-accounts
                    users-extra))
     (packages (append packages* %base-packages))
     (services final-services)
     (mapped-devices mapped-devices)
     (file-systems (append file-systems %base-file-systems))
     (bootloader bootloader))))

