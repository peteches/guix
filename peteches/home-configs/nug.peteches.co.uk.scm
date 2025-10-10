(define-module (peteches home-configs nug.peteches.co.uk)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (gnu packages base)
  #:use-module (gnu packages video)
  #:use-module (gnu packages node)
  #:use-module (gnu packages version-control)

  ;; services
  #:use-module (gnu home)
  #:use-module (gnu home services)  
  #:use-module (gnu home services guix)
  
  ;; base composer
  #:use-module (peteches home-configs base)

  ;; my packages
  #:use-module (peteches packages koboldcpp)

  #:use-module (peteches channels nug)
  
  ;; host-only services
  #:use-module (peteches home-services ai)
  #:use-module (peteches home-services agixt)
  #:use-module (peteches home-services koboldcpp)
  )

(define (home-abs-path path)
  "Expand PATH relative to $HOME if it is not absolute."
  (let* ((p  path)
         (abs (and (positive? (string-length p))
                   (char=? (string-ref p 0) #\/))))
    (if abs
        p
        (string-append (or (getenv "HOME") ".") "/" p))))

;; Packages unique to nug (examples you had: locales, node, pre-commit, etc.)
(define nug-extra-packages
  (list glibc-locales v4l-utils node pre-commit))

;; Services unique to nug (AI stacks, AGiXT bots, etc.)
(define nug-extra-services
  (list
   (service home-channels-service-type
	%nug-channels)

   (service koboldcpp-service-type
	    (koboldcpp-configuration
	     (package koboldcpp-bin)
	     (host "0.0.0.0")
	     (model-path (home-abs-path ".local/share/models/qwen2.5-coder-14b-instruct-q6_k.gguf"))))

   (service home-agixt-backend-service-type
	    (agixt-backend-configuration
	     (instance-name "default")
	     (credentials-file ".config/agixt/cred-backend")
	     (base-uri "http://localhost:7437")
	     (workdir ".local/share")
	     (respawn? #t)))

   ;; web app (Streamlit)
   (service home-agixt-webui-service-type
	    (agixt-webui-configuration
	     (instance-name "default") ; requires agixt-default
	     (base-uri "http://localhost:7437")
	     (credentials-file ".config/agixt/cred-backend")
	     (port 8501)
	     (respawn? #t)))

   ;; chat client (separate port; same UI by default)
   (service home-agixt-chatui-service-type
	    (agixt-chatui-configuration
	     (instance-name "default")
	     (base-uri "http://localhost:7437")
	     (credentials-file ".config/agixt/cred-backend")
	     (port 3437)
	     (respawn? #t)))
   
   (service home-agixt-telegram-bots-service-type
	    (agixt-telegram-bots-configuration
	     (bots
	      (list
	       (agixt-telegram-bot-configuration
		(name "leah")
		(backend-instance-name "default")
		(credentials-file ".config/agixt/cred-bot-leah")
		(base-uri "http://localhost:7437")
		(agent "AGiXT")
		(chain "")
		(allowed-user-ids '("7642100300")))
	       (agixt-telegram-bot-configuration
		(name "kim")
		(backend-instance-name "default")
		(credentials-file ".config/agixt/cred-bot-kim")
		(base-uri "http://localhost:7437")
		(agent "Secretary")
		(chain "morning-briefing")
		(allowed-user-ids '("7642100300")))))
	     (respawn? #t)))	   ))

(home-environment
  (packages
   (append nug-extra-packages base-packages))
  (services
    (append nug-extra-services base-services)))

