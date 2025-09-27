(define-module (peteches home-configs bhiyaki.peteches.co.uk)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (guix records)
  #:use-module (peteches home-services hyprland)
  #:use-module (peteches home-configs base))


(define custom-hyprland-binds
  (list
   ))

(home-environment
  (packages base-packages)
  (services
    (modify-services
     base-services
      (home-hyprland-service-type cfg =>
        (match-record cfg <home-hyprland-configuration> (binds)
          (home-hyprland-configuration
            (inherit cfg)
            (binds (append (or binds '())
                           custom-hyprland-binds))))))))
