;;; peteches/home/configs/nug.scm — home-environment for nug (desktop).
;;;
;;;   guix home -L . reconfigure peteches/home/configs/nug.scm
;;;
;;; Composes base-packages / base-services from (peteches home modules base)
;;; with nug-only extras.  The file evaluates to a bare `home-environment'
;;; record as its last expression — that is what `guix home' consumes.
;;;
;;; nug-specific:
;;;   - NVIDIA/Steam/CUDA userspace, Blender, 3D-printing (lycheeslicer).
;;;   - Three koboldcpp instances, one per model/port:
;;;       5001 qwen2.5-coder  — the coding model; ECA points here
;;;                             (see (peteches home modules ai)) and
;;;                             $COMFYUI_URL/KoboldCpp ports are opened in
;;;                             the firewall in (peteches systems base).
;;;       5002 dolphin + SD   — chat + image gen
;;;       5003 dolphin        — chat + image gen
;;;     All bind host "::" (all interfaces) and serve HTTPS using the
;;;     certbot-issued cert that peteches/systems/nug.scm's %fix-perms-hook
;;;     concatenates into ~/.local/share/certs/nug.peteches.co.uk.pem.
;;;     That deploy hook must run before these services can start.
;;;   - wireplumber rules renaming two USB audio interfaces, matched on
;;;     their PCI/USB topology path (api.alsa.card.longname).  These match a
;;;     specific physical port; moving the device to another port silently
;;;     stops the rule from applying.
;;;   - %nug-channels (adds guix-hpc-non-free on top of %base-channels).
;;;
;;; nyarlothotep is the sibling config and stays deliberately lean.

(define-module (peteches home configs nug)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (gnu packages base)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages gl)


  #:use-module (gnu packages graphics)
  #:use-module (gnu packages node)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)

  #:use-module (nongnu packages game-client)
  #:use-module (nongnu packages nvidia)



  ;; services
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)

  ;; base composer
  #:use-module (peteches home modules base)
  #:use-module (peteches home modules mpv)

  ;; my packages
  #:use-module (peteches home services koboldcpp)
  #:use-module (peteches home services hyprland)
  #:use-module (peteches home services mpv)

  #:use-module (peteches packages lycheeslicer)

  #:use-module (peteches channels nug)
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
  (list lycheeslicer-7.6.2
	steam-nvidia
	nvda
	nvidia-prime
	protonup
	vulkan-tools
	mesa-utils
	blender
	libwacom
	glibc-locales
	v4l-utils
	node
	pre-commit))

;; Services unique to nug (AI stacks, AGiXT bots, etc.)
(define nug-extra-services
  (list
   (service home-mpv-service-type
	    (mpv-config
	     (hwdec "nvdec")
	     (demuxer-max-bytes "4096MiB")
	     (demuxer-max-back-bytes "1024MiB")
	     (demuxer-readahead-secs 1200)
	     (cache-secs 7200)

	     (profiles %mpv-profiles)))

   (simple-service 'wireplumber-audio-device-names
		   home-xdg-configuration-files-service-type
		   `(("wireplumber/wireplumber.conf.d/51-audio-device-names.conf"
		      ,(plain-file
			"51-audio-device-names.conf"
			(string-join
			 (list
			  "monitor.alsa.rules = ["
			  "# Wharfedale MixDesk"
			  "  {"
			  "    matches = ["
			  "      {"
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-9.1.4.3.*\""
			  "      }"
			  "    ]"
			  "    actions = {"
			  "      update-props = {"
			  "        device.description = \"Wharfedale MixDesk\""
			  "        device.nick = \"Wharfedale USB MixDesk\""
			  "      }"
			  "    }"
			  "  }"
			  "  {"
			  "    matches = ["
			  "      {"
			  "        media.class = \"Audio/Sink\""
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-9.1.4.3.*\""
			  "      }"
			  "    ]"
			  "    actions = {"
			  "      update-props = {"
			  "        node.description = \"Wharfedale MixDesk Sink\""
			  "        node.nick = \"WharfeDale MixDesk Sink\""
			  "        priority.session = 2000"
			  "        priority.driver = 2000"
			  "      }"
			  "    }"
			  "  }"
			  "  {"
			  "    matches = ["
			  "      {"
			  "        media.class = \"Audio/Source\""
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-9.1.4.3.*\""
			  "      }"
			  "    ]"
			  "    actions = {"
			  "      update-props = {"
			  "        node.description = \"Wharfedale MixDesk Audio Source\""
			  "        node.nick = \"Wharfedale MixDesk Audio Source\""
			  "      }"
			  "    }"
			  "  }"
			  ""
			  "  # M-Audio device:"
			  "  {"
			  "    matches = ["
			  "      {"
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-9.1.4.1.*\""
			  "      }"
			  "    ]"
			  "    actions = {"
			  "      update-props = {"
			  "        device.description = \"M-Audio\""
			  "        device.nick = \"M-Audio\""
			  "      }"
			  "    }"
			  "  }"
			  "  {"
			  "    matches = ["
			  "      {"
			  "        media.class = \"Audio/Source\""
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-9.1.4.1.*\""
			  "      }"
			  "    ]"
			  "    actions = {"
			  "      update-props = {"
			  "        node.description = \"M-Audio Microphone\""
			  "        node.nick = \"M-Audio Microphone\""
			  "      }"
			  "    }"
			  "  }"
			  "  # The M-Audio device exposes an output sink, but you do not want apps routed to it."
			  "  {"
			  "    matches = ["
			  "      {"
			  "        media.class = \"Audio/Sink\""
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-9.1.4.1.*\""
			  "      }"
			  "    ]"
			  "    actions = {"
			  "      update-props = {"
			  "        node.description = \"M-Audio Output\""
			  "        node.nick = \"M-Audio Output\""
			  "        priority.session = 1"
			  "        priority.driver  = 1"
			  "      }"
			  "    }"
			  "  }"
			  "]")
			 "\n")))))

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
   (modify-services (append nug-extra-services base-services))))
