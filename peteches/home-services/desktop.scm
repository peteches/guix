(define-module (peteches home-services desktop)
  ;; your modules
  #:use-module (peteches packages fonts)
  #:use-module (peteches packages scripts)
  #:use-module (peteches home-services wofi)
  ;; guix home/service modules
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)      ; home-pipewire-service-type
  ;; packages
  #:use-module (gnu packages glib)            ; provides 'dbus' package
  #:use-module (gnu packages video)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages qt)
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
  (list
   materia-theme      ; GTK Materia (includes Materia-dark)
   kvantum            ; Qt style engine (reads Kvantum configs)
   papirus-icon-theme ; optional: nice low-contrast dark icons
   shell-scripts helvum dbus
   pipewire wireplumber
   alsa-utils pulseaudio))

(define (home-desktop-environment-variables config)
  (list
   `("QT_STYLE_OVERRIDE" . "kvantum")))

;; 2) PipeWire (with Pulse shim) through Home’s sound service.
(define (home-desktop-pipewire-service _config)
  (home-pipewire-configuration
   (enable-pulseaudio? #t)))

(define (home-desktop-xdg-files config)
  (list
   `("gtk-3.0/settings.ini"
     ,(plain-file "gtk3-settings.ini"
		  (string-append
		   "[Settings]\n"
		   "gtk-theme-name=Materia-dark\n"
		   "gtk-icon-theme-name=Papirus-Dark\n"
		   "gtk-application-prefer-dark-theme=1\n")))
   `("gtk-4.0/settings.ini"
     ,(plain-file "gtk4-settings.ini"
		  (string-append
		   "[Settings]\n"
		   "gtk-theme-name=Materia-dark\n"
		   "gtk-icon-theme-name=Papirus-Dark\n"
		   "gtk-application-prefer-dark-theme=1\n")))
   `("Kvantum/kvantum.kvconfig"
     ,(plain-file "kvantum.kvconfig"
		  (string-append
		   "[General]\n"
		   "theme=MateriaDark\n")))))


;; 3) The desktop meta-service ties it all together.
(define home-desktop-service-type
  (service-type
   (name 'home-desktop)
   (description "My desktop environment service.")
   (extensions
    (list  
     (service-extension
      home-xdg-configuration-files-service-type
      home-desktop-xdg-files)
     (service-extension
      home-environment-variables-service-type
      home-desktop-environment-variables)
     ;; Profile packages
     (service-extension
      home-profile-service-type
      home-desktop-profile-service)
     ;; PipeWire + pipewire-pulse + wireplumber
     (service-extension
      home-pipewire-service-type
      home-desktop-pipewire-service)))
   (default-value #f)))
