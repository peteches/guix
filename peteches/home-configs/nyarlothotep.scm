(define-module (peteches home-configs nyarlothotep)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (peteches channels base)
  #:use-module (peteches packages lycheeslicer)
  #:use-module (peteches home-services hyprland)
  #:use-module (peteches home-configs base))

(home-environment
 (packages (append (list lycheeslicer-7.6.2) base-packages))
 (services
  (modify-services
   (append
    (list
     (service home-channels-service-type
	      %base-channels))
    base-services)
   (home-hyprland-service-type config =>
    (home-hyprland-configuration
     (inherit config)
     (monitors (list
		(monitor
		 (name "eDP-1")
		 (scale 1)))))))))
