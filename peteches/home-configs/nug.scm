(define-module (peteches home-configs nug)
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
  #:use-module (peteches home-services koboldcpp)

  #:use-module (peteches channels nug)

  ;; host-only services
  #:use-module (peteches home-services ai)
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
   (service koboldcpp-service-type
	    (koboldcpp-configuration
	     (service-name "koboldcpp-qwen")
	     (model-name "qwen2.5-coder-14b-instruct-q6_k.gguf")
	     (host "::")
	     (whisper-model "whisper-small-q5_1.bin")
	     (tts-model "Kokoro_no_espeak_Q4.gguf")
	     (draft-model "qwen2.5-coder-1.5b-instruct-q4_k_m.gguf")
	     (port 5001)
	     (ssl-cert (home-abs-path ".local/share/certs/nug.peteches.co.uk.pem"))
	     (ssl-key  (home-abs-path ".local/share/certs/nug.peteches.co.uk.pem"))
	     (extra-args (list "--usecuda"
			       "--websearch"
			       "--draftamount" "16"
			       "--gpulayers" "999"
			       "--contextsize" "16384"
			       "--flashattention"))))
   (service koboldcpp-service-type
	    (koboldcpp-configuration
	     (service-name "koboldcpp-dolphin-sd")
	     (model-name "Dolphin-Mistral-24B-Venice-Edition-Q4_K_S.gguf")
	     (host "::")
	     (port 5002)
	     (whisper-model "whisper-small-q5_1.bin")
	     (tts-model "Kokoro_no_espeak_Q4.gguf")
	     (sd-model "pornworksRealPorn_v03.safetensors")
	     (ssl-cert (home-abs-path ".local/share/certs/nug.peteches.co.uk.pem"))
	     (ssl-key  (home-abs-path ".local/share/certs/nug.peteches.co.uk.pem"))
	     (extra-args (list "--usecuda"
			       "--websearch"
			       "--ttsgpu"
			       "--gpulayers" "999"
			       "--contextsize" "8192"
			       "--flashattention"
			       "--quantkv" "1"))))
   (service koboldcpp-service-type
	    (koboldcpp-configuration
	     (service-name "koboldcpp-dolphin")
	     (model-name "Dolphin-Mistral-24B-Venice-Edition-Q5_K_S.gguf")
	     (sd-model "cyberrealisticLCM_cyberrealistic42.safetensors")
	     (whisper-model "whisper-small-q5_1.bin")
	     (tts-model "Dia_Q5_DAC_F16.gguf")
	     (host "::")
	     (port 5003)
	     (ssl-cert (home-abs-path ".local/share/certs/nug.peteches.co.uk.pem"))
	     (ssl-key  (home-abs-path ".local/share/certs/nug.peteches.co.uk.pem"))
	     (extra-args (list "--usecuda"
			       "--websearch"
       			       "--gpulayers" "999"
			       "--contextsize" "16386"
			       "--flashattention"
			       "--quantkv" "1"))))

   (service home-channels-service-type
	    %nug-channels)))

(home-environment
  (packages
   (append nug-extra-packages base-packages))
  (services
    (append nug-extra-services base-services)))
