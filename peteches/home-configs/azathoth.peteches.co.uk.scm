(define-module (peteches home-configs azathoth.peteches.co.uk)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (peteches channels azathoth)
  #:use-module (peteches home-configs base))

(home-environment
 (packages base-packages)
 (services
  (append
   (list
    (service home-channels-service-type
	     %azathoth-channels))
   base-services)))
