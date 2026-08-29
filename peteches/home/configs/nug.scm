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
;;;   - Two koboldcpp instances, one per model/port:
;;;       5001 qwen3.6-a3b    — the coding model; ECA points here
;;;                             (see (peteches home modules ai)) and
;;;                             $COMFYUI_URL/KoboldCpp ports are opened in
;;;                             the firewall in (peteches systems base).
;;;                             Qwen3.6-35B-A3B (MoE, hybrid Gated
;;;                             Attention + Gated DeltaNet), Q4_K_M
;;;                             GGUF, ~22.3GiB weights. Full GPU
;;;                             offload, --quantkv 2 (Q4 KV cache), no
;;;                             draft/speculative-decode model, since
;;;                             ComfyUI is no longer run alongside
;;;                             koboldcpp on this card and the whole
;;;                             24GB is available -- but the model
;;;                             itself is far bigger than the
;;;                             qwen2.5-coder-14b it replaced, so
;;;                             --contextsize is conservative (32768)
;;;                             pending empirical headroom measurement.
;;;       5002 koboldcpp-rp   — SillyTavern RP backend: dense 24B RP
;;;                             finetune (MS3.2-PaintedFantasy-v4.1-24B),
;;;                             16K context, TTS/whisper attached (CPU),
;;;                             partial GPU offload (--gpulayers 20) —
;;;                             ComfyUI's Z-Image-Turbo process holds
;;;                             ~12GB VRAM permanently and must stay
;;;                             loaded concurrently, so full offload
;;;                             doesn't fit.  No SD attached here; use
;;;                             ComfyUI for image gen instead.
;;;     Both bind host "::" (all interfaces) and serve HTTPS using the
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
  #:use-module (peteches packages docker-compose)
  #:use-module (peteches packages rclone)

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
	rclone
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
	     (model-name "Qwen_Qwen3.6-35B-A3B-Q4_K_M.gguf")
	     (host "::")
	     (whisper-model "whisper-small-q5_1.bin")
	     (tts-model "Kokoro_no_espeak_Q4.gguf")
	     (port 5001)
	     (ssl-cert (home-abs-path ".local/share/certs/nug.peteches.co.uk.pem"))
	     (ssl-key  (home-abs-path ".local/share/certs/nug.peteches.co.uk.pem"))
	     ;; Swapped from qwen2.5-coder-14b (Q6_K, ~11GiB weights) to
	     ;; Qwen3.6-35B-A3B (MoE, hybrid Gated Attention + Gated
	     ;; DeltaNet, Q4_K_M, ~22.3GiB weights on disk) -- a newer,
	     ;; better-benchmarked coding model (73.4% SWE-bench Verified)
	     ;; that also needed a recent-enough koboldcpp build (Gated
	     ;; DeltaNet support landed in llama.cpp/koboldcpp fairly
	     ;; recently; confirm on load if this pinned version is new
	     ;; enough). colibri (see peteches/systems/nug.scm) was tried
	     ;; first since it can run 700B+ MoE models via disk-streamed
	     ;; experts, but that's the wrong tool for a model this small:
	     ;; the pre-converted checkpoint's baked-in expert-cache size
	     ;; didn't cover this model's 256 experts, so it fell back to
	     ;; RAM/CPU residency and measured ~1 token/sec. At 22.3GiB,
	     ;; this model's weights alone leave far less headroom than the
	     ;; 14B did. Measured live at --contextsize 32768: 23.3GiB/
	     ;; 24.5GiB used, only ~774MiB free -- but the hybrid
	     ;; architecture's real KV cache only covers 10 of 40 layers
	     ;; (the rest use a fixed ~63MiB recurrent state regardless of
	     ;; context length: llama_memory_recurrent in the startup log),
	     ;; measured at ~5.6 KiB/token, vs qwen2.5-coder-14b's 48-192
	     ;; KiB/token. 131072 (4x) adds only ~543MiB of KV on top of
	     ;; that measurement, comfortably under the observed headroom;
	     ;; the full native 262144 would add ~1.27GiB, more than what
	     ;; was free at 32768, so left for a later attempt if this
	     ;; measures out with more margin than expected.
	     (extra-args (list "--usecuda"
			       "--websearch"
			       "--gpulayers" "999"
			       "--contextsize" "131072"
			       "--flashattention"
			       "--quantkv" "2"))))
   ;; Roleplay instance (SillyTavern backend).  Replaces the former
   ;; koboldcpp-dolphin-sd (:5002) / koboldcpp-dolphin (:5003) pair —
   ;; consolidated to a single dense 24B model so the full 24GB card goes
   ;; to model weights + a 32K KV cache instead of splitting VRAM across
   ;; two simultaneously-loaded ~24B checkpoints.  Dolphin-Mistral-24B
   ;; Venice-Edition replaced with MS3.2-PaintedFantasy-v4.1-24B
   ;; (zerofata) Q5_K_M: a dense Mistral-Small-3.2-24B-based finetune
   ;; purpose-built for character-driven RP, chosen over the
   ;; Qwen3.5-35B-A3B/Qwen3-30B-A3B abliterated MoE candidates because
   ;; that MoE family has documented severe RP repetition
   ;; (huggingface.co/Qwen/Qwen3-30B-A3B/discussions/23) and neither has
   ;; RP-specific tuning — abliteration only strips refusals.
   ;;
   ;; ComfyUI (/srv/comfyui, Z-Image-Turbo) is a separate always-on
   ;; process that permanently holds ~12GB VRAM on this card (see
   ;; `nvidia-smi` — PID resident, not on-demand), and it must stay
   ;; loaded concurrently with this instance.  That leaves only ~12GB
   ;; for koboldcpp, so full GPU offload of a Q5_K_M 24B model (~16GB
   ;; weights alone) is not possible here — partial CPU/RAM offload is
   ;; mandatory, not optional.  --gpulayers 20 (of 40 dense layers) and
   ;; --ttsgpu removed (TTS now runs on CPU, matching koboldcpp-qwen's
   ;; default) are a conservative starting point sized from
   ;; ~380MiB/layer weights + proportional KV share, leaving safety
   ;; margin against the ~12GB ComfyUI reserves.  --contextsize dropped
   ;; from 32768 to 16384 for the same reason.  Offloading costs
   ;; generation speed, not quality — tune --gpulayers/--contextsize
   ;; upward from real `nvidia-smi`/`herd status koboldcpp-rp` numbers
   ;; once this is confirmed stable; this pairing was not live-tested
   ;; before being committed.
   (service koboldcpp-service-type
	    (koboldcpp-configuration
	     (service-name "koboldcpp-rp")
	     (model-name "MS3.2-PaintedFantasy-v4.1-24B-Q5_K_M.gguf")
	     (host "::")
	     (port 5002)
	     (whisper-model "whisper-small-q5_1.bin")
	     (tts-model "Kokoro_no_espeak_Q4.gguf")
	     (ssl-cert (home-abs-path ".local/share/certs/nug.peteches.co.uk.pem"))
	     (ssl-key  (home-abs-path ".local/share/certs/nug.peteches.co.uk.pem"))
	     (extra-args (list "--usecuda"
			       "--websearch"
			       "--gpulayers" "20"
			       "--contextsize" "16384"
			       "--flashattention"
			       "--quantkv" "1"))))

   (service home-channels-service-type
	    %nug-channels)))

(home-environment
  (packages
   (append nug-extra-packages base-packages))
  (services
   (modify-services (append nug-extra-services base-services))))
