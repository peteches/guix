;;; peteches/home/configs/dagon.scm — home-environment for dagon (desktop).
;;;
;;;   guix home -L . reconfigure peteches/home/configs/dagon.scm
;;;
;;; Composes base-packages / base-services from (peteches home modules base)
;;; with dagon-only extras.  The file evaluates to a bare `home-environment'
;;; record as its last expression — that is what `guix home' consumes.
;;;
;;; Deliberately a copy of nug.scm minus the local LLM/image-gen stack:
;;;   - Same NVIDIA/Steam/CUDA userspace, Blender, 3D-printing (lycheeslicer)
;;;     packages as nug.
;;;   - No koboldcpp instances and no Stable Diffusion service — dagon does
;;;     not host local model inference.
;;;   - Still carries the docker-compose CLI plugin service (used for
;;;     ComfyUI elsewhere) and the wireplumber audio device renaming rules;
;;;     both were retargeted to dagon's own USB bus paths after the hub
;;;     was moved over from nug — the original nug-specific paths
;;;     (9.1.4.3 / 9.1.4.1) never matched on dagon, so the rules silently
;;;     did nothing until this fix (see 2026-08-24 audio troubleshooting).
;;;     Wharfedale MixDesk speakers are usb-0000:00:14.0-10.3; the M-Audio
;;;     mic is usb-0000:00:14.0-10.1 — both confirmed against dagon's live
;;;     `pactl`/`wpctl` output.
;;;   - %nug-channels (adds guix-hpc-non-free on top of %base-channels),
;;;     same as nug.
;;;
;;; nug is the sibling this was copied from; nyarlothotep is the lean laptop
;;; config.

(define-module (peteches home configs dagon)
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
  #:use-module (peteches home services mpv)

  #:use-module (peteches packages lycheeslicer)
  #:use-module (peteches packages docker-compose)
  #:use-module (peteches packages zoom)

  #:use-module (peteches channels nug)
)

;; Packages unique to dagon (same set as nug).
(define dagon-extra-packages
  (list lycheeslicer-7.6.2
	zoom
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

;; Services unique to dagon — same as nug, minus the koboldcpp/Stable
;; Diffusion instances.
(define dagon-extra-services
  (list
   ;; `docker compose'.  Guix's only compose package is the Python v1
   ;; (1.29.2), which cannot talk to the daemon at all -- see
   ;; (peteches packages docker-compose).  The plugin manager searches
   ;; $DOCKER_CONFIG/cli-plugins (i.e. ~/.docker/cli-plugins) and some
   ;; FHS paths, never PATH, so putting the package in `packages' would
   ;; not be found; it has to be symlinked into place.  This replaces a
   ;; binary that was previously installed there by hand.
   (simple-service 'docker-compose-cli-plugin
		   home-files-service-type
		   `((".docker/cli-plugins/docker-compose"
		      ,(file-append docker-compose-cli-plugin
				    "/libexec/docker/cli-plugins/docker-compose"))))

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
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-10.3.*\""
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
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-10.3.*\""
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
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-10.3.*\""
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
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-10.1.*\""
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
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-10.1.*\""
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
			  "        api.alsa.card.longname = \"~.*usb-0000:00:14.0-10.1.*\""
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

   (service home-channels-service-type
	    %nug-channels)))

(home-environment
  (packages
   (append dagon-extra-packages base-packages))
  (services
   (modify-services (append dagon-extra-services base-services))))
