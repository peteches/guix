(define-module (peteches home-services nyxt)
  #:use-module (gnu home services)
  #:use-module (gnu packages gstreamer)
  #:use-module (guix gexp)
  #:use-module (gnu packages web-browsers))

(define (nyxt-profile config)
  (list nyxt gst-plugins-ugly gst-plugins-good-qt gst-plugins-good))


(define (nyxt-files-service config)
  (list
   `("nyxt/config.lisp" ,(local-file "nyxt.lisp"))))


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
