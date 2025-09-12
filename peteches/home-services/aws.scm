(define-module (peteches home-services aws)
  #:use-module (gnu home services)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages password-utils))

(define (aws-profile config)
  (list awscli aws-vault))

(define-public home-aws-service-type
  (service-type (name 'aws)
		(description "My aws setup")
		(default-value '())
		(extensions
		 (list
		  (service-extension
		   home-profile-service-type
		   aws-profile)))))
