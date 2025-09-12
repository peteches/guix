(define-module (peteches home-services aws)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages password-utils)
  #:export (home-aws-configuration))

(define (serialize-string field val)
  (cond
   ((string? val) val)
   ((symbol? val) (symbol->string val))))

(define (aws-profile config)
  (list awscli aws-vault))

(define-configuration home-aws-configuration
  (vault-pass-prefix
   (string "aws-vault")
   "The AWS_VAULT_PASS_PREFIX to set. default aws-vault")
  (vault-backend
   (string "pass")
   "The AWS_VAULT_BACKEND to set. default pass"))

(define (home-aws-environment-variables-service-type config)
  `(("AWS_VAULT_BACKEND" . ,(home-aws-configuration-vault-backend config))
    ("AWS_VAULT_PASS_PREFIX" . ,(home-aws-configuration-vault-pass-prefix config))))

(define-public home-aws-service-type
  (service-type (name 'aws)
		(description "My aws setup")
		(default-value (home-aws-configuration))
		(extensions
		 (list
		  (service-extension
		   home-environment-variables-service-type
		   home-aws-environment-variables-service-type)
		  (service-extension
		   home-profile-service-type
		   aws-profile)))))
