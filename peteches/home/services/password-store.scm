(define-module (peteches home services password-store)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages version-control)
  #:use-module ((gnu packages password-utils) #:hide (password-store))
  #:use-module (gnu packages browser-extensions)
  #:use-module (guix gexp)
  ;; Upstream `password-store' bakes in its own gnupg (2.5.20 dev
  ;; snapshot) via wrap-program -- see peteches/packages/gnupg.scm.
  #:use-module (peteches packages gnupg)
  #:export (home-password-store-configuration home-password-store-service-type))

(define (serialize-string field val)
  (cond
    ((string? val)
     val)
    ((symbol? val)
     (symbol->string val))))

(define-configuration home-password-store-configuration
  (repo-uri (string) "The repo url containing the password store")
  (password-store-dir (string)
   "The local directory to clone the password store to. This will be set as the value to the PASSWORD_STORE_DIR var and added to the home-environment-variables-service-type."))

(define (push-pull-password-store-job config)
  (let ((cmd (string-append "[[ -d "
                            (home-password-store-configuration-password-store-dir
                             config)
                            " ]] || "
                            "git clone "
                            (home-password-store-configuration-repo-uri config)
                            " "
                            (home-password-store-configuration-password-store-dir
                             config)
                            ";"
                            "pass git pull --rebase; pass git push")))
    #~(job '(next-hour)
           #$cmd)))

(define (home-password-store-profile-service-type config)
  (cons password-store
        (map specification->package
             '("pass-otp"
               "passff-host"
               "grimblast"
               "wl-clipboard"
               "zbar"
               "git"))))

(define (home-password-store-mcron-service-type config)
  (list (push-pull-password-store-job config)))

(define (home-password-store-environment-variables-service-type config)
  `(("PASSWORD_STORE_DIR" unquote
     (home-password-store-configuration-password-store-dir config))
    ("PASSWORD_STORE_ENABLE_EXTENSIONS" . "true")
    ("PASSWORD_STORE_EXTENSIONS_DIR" unquote
     (string-append (getenv "HOME")
                    "/.guix-home/profile/lib/password-store/extensions"))))

(define-public home-password-store-service-type
  (service-type (name 'peteches-passwords)
                (description "Setup and sync password store.")
                (extensions (list (service-extension
                                   home-environment-variables-service-type
                                   home-password-store-environment-variables-service-type)
                                  (service-extension home-mcron-service-type
                                   home-password-store-mcron-service-type)
                                  (service-extension home-profile-service-type
                                   home-password-store-profile-service-type)))))
