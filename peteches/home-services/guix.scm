;; home-channels.scm  — managed by Guix Home
;; Pin exact commits for reproducibility.
(define-module (peteches home-services guix)
  #:use-module (gnu home services)
  #:use-module (guix gexp))

(define (home-guix-channels-xdg-files-service-type config)
  (list
   ;; Write ~/.config/guix/channels.scm from a store file
   `("guix/channels.scm" ,(local-file "guix-home-channels.scm"))))

(define-public home-guix-service-type
  (service-type (name 'home-guix-channels)
		(description "Holds home guix configs")
		(default-value '())
		(extensions
		 (list
		  (service-extension
		   home-xdg-configuration-files-service-type
		   home-guix-channels-xdg-files-service-type)))))
