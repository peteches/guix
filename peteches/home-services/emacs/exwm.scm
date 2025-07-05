(define-module (peteches home-services emacs exwm)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-emacs-exwm-service-type))

(define (home-emacs-exwm-profile-service config)
       (list emacs-exwm))

(define (home-emacs-exwm-files-service config)
  (list
   `("emacs/peteches-exwm.el" ,(local-file "./configs/peteches-exwm.el"))))

(define home-emacs-exwm-service-type
  (service-type (name 'home-emacs-exwm-config)
		(extensions
		 (list
		  (service-extension
		   home-profile-service-type
		   home-emacs-exwm-profile-service)
		  (service-extension
		   home-xdg-configuration-files-service-type
		   home-emacs-exwm-files-service)))
		(default-value 'nil)
		(description "Applies my personal Emacs EXWM configuration")))
