(define-module (peteches home-services wofi)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages xdisorg))


(define-public (wofi-profile-service config)
       (list wofi))

(define-configuration wofi-configuration)

(define-public wofi-service-type
  (service-type (name 'wofi-config)
		(extensions
		 (list
		  (service-extension
		   home-profile-service-type
		   wofi-profile-service)))
		(default-value (wofi-configuration))
		(description "Applies my personal Wofi configuration and scripts.")))
