(define-module (peteches home-services desktop)
  #:use-module (peteches packages fonts)
  #:use-module (peteches packages scripts)
  #:use-module (gnu home services)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages video)
  #:use-module (gnu services)
  #:use-module (gnu packages )
  #:use-module (guix gexp)
  #:use-module (peteches home-services wofi)
  #:export (home-desktop-service-type))

(define (home-desktop-profile-service config)
  (list
   ;; Generically useful packages
))

(define home-desktop-service-type
  (service-type (name 'home-desktop)
                (description "My desktop environment service.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-desktop-profile-service)))
                (default-value #f)))
