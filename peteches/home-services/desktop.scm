(define-module (peteches home-services desktop)
  ;; your modules
  #:use-module (peteches packages fonts)
  #:use-module (peteches packages scripts)
  #:use-module (peteches home-services wofi)
  ;; guix home/service modules
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)      ; home-pipewire-service-type
  ;; packages
  #:use-module (gnu packages glib)            ; provides 'dbus' package
  #:use-module (gnu packages video)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:export (home-desktop-service-type))

;; 1) Desktop packages as before
(define (home-desktop-profile-service _config)
  (list shell-scripts helvum dbus
        pipewire wireplumber
        alsa-utils pulseaudio))  

;; 2) User D-Bus session bus as a Shepherd service.
;;    We provision it as 'dbus' because home-pipewire requires that.
(define (home-desktop-shepherd-services _config)
  (list
   (shepherd-service
     (provision '(dbus))
     (documentation "Virtual: mark dbus ready when dbus-run-session is up.")
     (requirement '())
     (start #~(lambda _ #t))
     (one-shot? #t))))


;; 3) PipeWire (with Pulse shim) through Home’s sound service.
(define (home-desktop-pipewire-service _config)
  (home-pipewire-configuration
    (enable-pulseaudio? #t)))

;; 4) The desktop meta-service ties it all together.
(define home-desktop-service-type
  (service-type
   (name 'home-desktop)
   (description "My desktop environment service.")
   (extensions
    (list
     ;; Shepherd: supply dbus session bus
     (service-extension
      home-shepherd-service-type
      home-desktop-shepherd-services)
     ;; Profile packages
     (service-extension
      home-profile-service-type
      home-desktop-profile-service)
     ;; PipeWire + pipewire-pulse + wireplumber
     (service-extension
      home-pipewire-service-type
      home-desktop-pipewire-service)))
   (default-value #f)))
