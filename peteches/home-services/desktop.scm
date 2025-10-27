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
  #:use-module (gnu packages)            ; provides 'dbus' package
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:export (home-desktop-service-type))

;; 1) Desktop packages as before
(define (home-desktop-profile-service _config)
  (map specification->package
       (list
	"shell-scripts"
	"libnotify"
	"materia-theme"	     ; GTK Materia (includes Materia-dark)
	"kvantum"		     ; Qt style engine (reads Kvantum configs)
	"papirus-icon-theme"   ; optional: nice low-contrast dark icons
	"helvum" "dbus"
	"ocean-sound-theme"
	"libcanberra"
	"pipewire" "wireplumber"
	"alsa-utils" "pulseaudio")))

(define (home-desktop-environment-variables config)
  (list
   `("XDG_SOUND_THEME_NAME" . "ocean")
   `("GTK_MODULES" . "canberra-gtk-module")
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
		   "gtk-enable-event-sounds = 1\n"
		   "gtk-enable-input-feedback-sounds = 1\n"
		   "gtk-sound-theme-name = ocean\n"
		   "gtk-theme-name=Materia-dark\n"
		   "gtk-icon-theme-name=Papirus-Dark\n"
		   "gtk-application-prefer-dark-theme=1\n")))
   `("gtk-4.0/settings.ini"
     ,(plain-file "gtk4-settings.ini"
		  (string-append
		   "[Settings]\n"
		   "gtk-enable-event-sounds = 1\n"
		   "gtk-enable-input-feedback-sounds = 1\n"
		   "gtk-sound-theme-name = ocean\n"
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
