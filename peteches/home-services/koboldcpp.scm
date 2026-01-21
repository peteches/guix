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
                                                                           "/.local/share/models")))
  (service-name koboldcpp-configuration-service-name (default "koboldcpp"))
  (model-name   koboldcpp-configuration-model-name (default "model.gguf"))
  (whisper-model koboldcpp-configuration-whisper-model (default ""))
  (tts-model koboldcpp-configuration-tts-model (default ""))
  (draft-model koboldcpp-configuration-draft-model (default ""))
  (mmproj-model koboldcpp-configuration-mmproj-model (default ""))
  (wavtokeniser-model koboldcpp-configuration-wavtokeniser-model (default ""))
  (sd-model koboldcpp-configuration-sd-model (default ""))
  (ssl-cert   koboldcpp-configuration-ssl-cert (default ""))
  (ssl-key    koboldcpp-configuration-ssl-key (default ""))
  (auto-start   koboldcpp-configuration-auto-start (default #f))
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
	 (service-name (string->symbol (koboldcpp-configuration-service-name cfg)))
	 (model-path (koboldcpp-configuration-model-path cfg))
         (model-name      (koboldcpp-configuration-model-name cfg))
	 (model      (string-append model-path "/" model-name))
	 (autostart  (koboldcpp-configuration-auto-start cfg))
         (host       (koboldcpp-configuration-host cfg))
         (port       (koboldcpp-configuration-port cfg))
         (work       (koboldcpp-configuration-work-dir cfg))
	 (ssl-cert   (koboldcpp-configuration-ssl-cert cfg))
	 (ssl-key   (koboldcpp-configuration-ssl-key cfg))
	 (whisper-model       (koboldcpp-configuration-whisper-model cfg))
	 (tts-model       (koboldcpp-configuration-tts-model cfg))
	 (draft-model (koboldcpp-configuration-draft-model cfg))
	 (mmproj-model (koboldcpp-configuration-mmproj-model cfg))
	 (sd-model (koboldcpp-configuration-sd-model cfg))
	 (wavtokeniser-model (koboldcpp-configuration-wavtokeniser-model cfg))
         (extra-args (append
		      (koboldcpp-configuration-extra-args cfg)
		      (if (and (not (string=? ssl-cert ""))
			       (not (string=? ssl-key "")))
			  (list "--ssl" ssl-cert ssl-key)
			  '())
		      (if (not (string=? sd-model ""))
			  (list "--sdmodel"
				(string-append model-path "/" sd-model))
			  '())
		      (if (not (string=? tts-model ""))
			  (list "--ttsmodel"
				(string-append model-path "/" tts-model))
			  '())
		      (if (not (string=? draft-model ""))
			  (list "--draftmodel"
				(string-append model-path "/" draft-model))
			  '())
		      (if (not (string=? mmproj-model ""))
			  (list "--mmproj"
				(string-append model-path "/" mmproj-model))
			  '())
		      (if (not (string=? wavtokeniser-model ""))
			  (list "--ttswavtokenizer"
				(string-append model-path "/" wavtokeniser-model))
			  '())
		      (if (not (string=? whisper-model ""))
			  (list "--whispermodel"
				(string-append model-path "/" whisper-model))
			  '()))))
    (list
     (shepherd-service
      (documentation "KoboldCpp text-generation server (user session).")
      (provision (list service-name))
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
			    (string-append "/koboldcpp-" #$model-name ".log"))))
      (stop #~(make-kill-destructor))
      (auto-start? autostart)))))

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
