(define-module (peteches home-services aws)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages password-utils)
  #:use-module (peteches packages aws)
  #:use-module (gnu packages)
  #:export (home-aws-configuration
            home-aws-service-type))

;; Serialize a boolean to "true"/"false"
(define (serialize-boolean field val)
  (if val "true" "false"))

(define (serialize-string field val)
  (cond
   ((string? val) val)
   ((symbol? val) (symbol->string val))))

(define-configuration home-aws-configuration
  (vault-pass-prefix
   (string "aws-vault")
   "The AWS_VAULT_PASS_PREFIX to set. default aws-vault")
  (vault-backend
   (string "pass")
   "The AWS_VAULT_BACKEND to set. default pass")
  (install-session-manager?
   (boolean #t)
   "If true, install the AWS Session Manager Plugin package.")
  ;; Let callers override the package if they want a fork/custom build.
  (session-manager-package
   (package aws-session-manager-plugin)
   "Package providing the 'session-manager-plugin' binary."))

(define (aws-profile config)
  (let ((base (list awscli aws-vault)))
    (if (home-aws-configuration-install-session-manager? config)
        (append base (list (home-aws-configuration-session-manager-package config)))
        base)))

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
