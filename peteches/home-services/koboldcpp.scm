;; peteches/home-services/koboldcpp.scm  (HOME service)
(define-module (peteches home-services koboldcpp)
  ;; packages
  #:use-module (peteches packages koboldcpp)
  ;; HOME services (not system)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  ;; guix
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (koboldcpp-configuration
            koboldcpp-service-type))

;; -----------------------------------------------------------------------------
;; Configuration
;; -----------------------------------------------------------------------------

(define-record-type* <koboldcpp-configuration>
  koboldcpp-configuration make-koboldcpp-configuration
  koboldcpp-configuration?
  ;; choose koboldcpp-cuda in your home config if you want CUDA
  (package      koboldcpp-configuration-package (default koboldcpp-bin))
  (model-path   koboldcpp-configuration-model-path (default (string-append (or (getenv "HOME") "")
                                                                           "/.local/share/koboldcpp/model.gguf")))
  (host         koboldcpp-configuration-host       (default "127.0.0.1"))
  (port         koboldcpp-configuration-port       (default 5001))
  (extra-args   koboldcpp-configuration-extra-args (default '()))
  ;; Home services run as the current user; keep a work dir in $XDG_DATA_HOME
  (work-dir     koboldcpp-configuration-work-dir   (default (string-append (or (getenv "HOME") "")
                                                                           "/.local/share/koboldcpp"))))

;; -----------------------------------------------------------------------------
;; Home shepherd service
;; -----------------------------------------------------------------------------

(define (koboldcpp-shepherd-service cfg)
  (let* ((pkg        (koboldcpp-configuration-package cfg))
         (bin        (file-append pkg "/bin/koboldcpp"))
         (model      (koboldcpp-configuration-model-path cfg))
         (host       (koboldcpp-configuration-host cfg))
         (port       (koboldcpp-configuration-port cfg))
         (work       (koboldcpp-configuration-work-dir cfg))
         (extra-args (koboldcpp-configuration-extra-args cfg)))
    (list
     (shepherd-service
      (documentation "KoboldCpp text-generation server (user session).")
      (provision '(koboldcpp))
      (requirement '()) ; add '("pipewire") etc. if your user shepherd provides them
      (start #~(make-forkexec-constructor
                (list #$bin
                      "--host" #$host
                      "--port" (number->string #$port)
                      "--model" #$model
                      #$@extra-args)
                ;; Home shepherd: no #:user/#:group — runs as the current user
                #:directory #$work
		#:log-file (string-append
			    (or (getenv "XDG_LOG_HOME")
				(format #f "~a/.local/var/log"
					(getenv "HOME")))
			    "/koboldcpp.log")))
      (stop #~(make-kill-destructor))
      (auto-start? #t)))))

(define (create-dir config)
  #~(begin
	   (use-modules (guix build utils))
	   (let* ((home   (getenv "HOME"))
		  (work   (string-append home "/.local/share/koboldcpp"))
		  (logdir (or (getenv "XDG_LOG_HOME")
			      (string-append home "/.local/var/log"))))
	     (mkdir-p work)
	     (mkdir-p logdir))))

(define koboldcpp-service-type
  (service-type
   (name 'koboldcpp)
   (extensions
    (list
     (service-extension home-activation-service-type
			create-dir)
     (service-extension home-shepherd-service-type
                        koboldcpp-shepherd-service)))
   (default-value (koboldcpp-configuration))
   (description "Run KoboldCpp under the user's Home Shepherd.")))
