(define-module (peteches home-configs bhiyaki.peteches.co.uk)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (peteches channels bhiyaki)
  #:use-module (peteches home-configs base))

(home-environment
 (packages base-packages)
 (services
  (append
   (list
    (service home-channels-service-type
	     %bhiyaki-channels))
   base-services)))
