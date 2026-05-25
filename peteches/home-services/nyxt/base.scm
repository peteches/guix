(define-module (peteches home-services nyxt base)
  #:use-module (gnu home services)
  #:use-module (gnu packages gstreamer)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (peteches packages nyxt))

(define (nyxt-profile config)
  (cons nyxt-webkit
        (map specification->package
             (list "gst-plugins-ugly" "gst-plugins-good"))))

(define (nyxt-files-service config)
  (list
   `("nyxt/config.lisp" ,(local-file "./main.lisp"))
   `("nyxt/modules"     ,(local-file "./modules" "nyxt-modules" #:recursive? #t))))


(define-public nyxt-service-type
  (service-type (name 'nyxt)
		(description "My nyxt configs")
		(default-value '())
		(extensions
		 (list
		  (service-extension
		   home-xdg-configuration-files-service-type
		   nyxt-files-service)
		  (service-extension
		   home-profile-service-type
		   nyxt-profile)))))
