;; nug.scm — nug.peteches.co.uk using (peteches systems base)
;;
;; Notes:
;; - Hyprland greeter via gtkgreet (from base.scm).
;; - Intel CPU microcode + NVIDIA firmware toggled on via flags.
;; - If you want root-on-LUKS, uncomment the LUKS section and comment the non-LUKS one.
;; - make-base-os should append %base-file-systems internally.

(define-module (peteches systems nug)
  #:use-module (nongnu packages linux)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services certbot)
  #:use-module (gnu packages base)           ; e.g. glibc-locales if you want it
  #:use-module (sops secrets)
  #:use-module (peteches systems base)
  #:use-module (peteches services firewall)
  #:use-module (peteches services comfyui)
  #:use-module (peteches packages colibri)
  #:use-module (peteches services colibri)
  #:use-module (peteches services sillytavern)
  #:use-module (peteches systems network-mounts)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages rust-apps)
  #:use-module ((gnu packages linux) #:select (linux-libre-headers))
  #:use-module (guix-science-nonfree packages cuda))

;; Bring service types into scope for any host-specific additions.
(use-service-modules base linux cups desktop networking ssh xorg)

(define %fix-perms-hook
  (with-imported-modules
   (source-module-closure '((ice-9 rdelim)
                            (guix build utils)))
   (program-file
    "generate-cert-key-pem-file"
    #~(begin
        (use-modules (ice-9 rdelim)
                     (guix build utils))

        (define cert "/etc/letsencrypt/live/nug.peteches.co.uk/fullchain.pem")
        (define key  "/etc/letsencrypt/live/nug.peteches.co.uk/privkey.pem")
        (define dst-dir "/home/peteches/.local/share/certs")
        (define dst (string-append dst-dir "/nug.peteches.co.uk.pem"))

        ;; absolute store paths, no PATH reliance
        (define chown #$(file-append coreutils "/bin/chown"))
        (define chmod #$(file-append coreutils "/bin/chmod"))
	(define cat #$(file-append coreutils "/bin/cat"))

        (mkdir-p dst-dir)

	;; Concatenate by streaming, no get-string-all, no invoke keywords
	(call-with-output-file dst
          (lambda (out)
            (call-with-input-file cert (lambda (in) (dump-port in out)))
            (newline out)
            (call-with-input-file key  (lambda (in) (dump-port in out)))))

        ;; owner only, no need to assume "users" group exists
        (invoke chown "-R" "peteches:" dst-dir)
        (invoke chmod "600" dst)))))


(define %comfyui-model-paths
  (plain-file "extra_model_paths.yaml"
              "comfyui:
    base_path: /media/ColdStorage/models/comfyui

    checkpoints: checkpoints/
    clip: clip/
    clip_vision: clip_vision/
    configs: configs/
    controlnet: controlnet/
    diffusion_models: diffusion_models/
    embeddings: embeddings/
    hypernetworks: hypernetworks/
    loras: loras/
    text_encoders: text_encoders/
    upscale_models: upscale_models/
    vae: vae/
    unet: unet/
    ipadapter: ipadapter/
    style_models: style_models/
    photomaker: photomaker/
    gligen: gligen/
    vae_approx: vae_approx/
    insightface: insightface/
    facerestore_models: facerestore_models/
    facedetection: facedetection/
    reactor: reactor/
    sams: sams/
    ultralytics: ultralytics/
    audio_encoders: audio_encoders/
    diffusers: diffusers/
    LLavacheckpoints: LLavacheckpoints/
    TTS: TTS/
"))

(make-base-os
 #:host-name "nug"
 #:kernel linux
 ;; Base will append %base-file-systems — you only give machine-specific mounts here.
 #:firmware (list linux-firmware)
 #:users-extra (list (user-account
		      (name "guix-offload")
		      (comment "Build offload user")
		      (group "users")
		      (system? #t)
		      (home-directory "/var/empty")))
 #:mapped-devices
 (list
  (mapped-device
   (source (uuid "820e9368-484a-4bc0-af58-f3f0c29fe0fa"))
   (target "cryptroot")
   (type luks-device-mapping))
  (mapped-device
   (source (uuid "3d049249-8d28-45a3-bd06-980429edf7b7"))
   (target "ColdStorage")
   (type luks-device-mapping)
   (arguments '(#:key-file "/etc/keys/cold-storage.key")))
  (mapped-device
   (source (uuid "8e15e4a6-a1ac-4638-b9d2-814257363cab"))
   (target "HotStorage")
   (type luks-device-mapping)
   (arguments '(#:key-file "/etc/keys/hot-storage.key")))
  (mapped-device
   (source (uuid "65689173-95c7-4f7a-83ff-7a66cb1c6695"))
   (target "WarmStorage")
   (type luks-device-mapping)
   (arguments '(#:key-file "/etc/keys/warm-storage.key"))))

 #:file-systems
 (list
  (file-system
   (mount-point "/")
   (device "/dev/mapper/cryptroot")	; ← ext4 root
   (type "ext4"))
  (file-system
   (mount-point "/boot")
   (device (uuid "2c8fb9c4-f41d-4415-9540-86b588e91bac" 'ext4))
   (type "ext4"))
  (file-system
   (mount-point "/boot/efi")
   (device (uuid "7222-0EC9" 'fat32)) ; ← EFI system partition
   (type "vfat"))
  (file-system
   (mount-point "/media/ColdStorage")
   (device (uuid "0cd3cea0-2ffc-4bf7-9cd0-91b9bbfa716b" 'ext4))
   (create-mount-point? #t)
   (type "ext4"))
  (file-system
   (mount-point "/media/HotStorage")
   (device (uuid "0b30a1c2-64d8-47ec-bfeb-d6ec47292886" 'ext4))
   (create-mount-point? #t)
   (type "ext4"))
  (file-system
   (mount-point "/media/WarmStorage")
   (device (uuid "2f08c1cc-68cb-47ca-8b1a-e4aff62def08" 'ext4))
   (create-mount-point? #t)
   (type "ext4"))
  scoreplay-cifs-mount)

 ;; Bootloader (UEFI)
 #:bootloader
 (bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (keyboard-layout (keyboard-layout "us")))

 ;; Host-specific packages (optional)
 #:extra-packages
 '()

 ;; Feature flags
 #:laptop? #f	       ; nug is a desktop
 #:intel-cpu? #t     ; used for thermald when laptop?; harmless here
 #:with-printing? #f
 #:with-bluetooth? #t
 #:with-nonguix? #t
 #:with-nvidia? #t
 #:with-docker? #t
 #:offload-builds? #f
 ;; Generates nug's age keypair on first boot (/etc/age/keys.pub) — see
 ;; the module comment on with-sops? in peteches/systems/base.scm.
 #:with-sops? #t
 ;; secrets/shared/colibri.yaml carries the colibri API key. It is
 ;; currently encrypted with nug's age key only, NOT the operator PGP key
 ;; every other secret in this repo also carries — that recipient needs
 ;; gpg-agent with the real private key material, which isn't available
 ;; in the environment that generated this file. Run, from a machine that
 ;; does have it:
 ;;   sops updatekeys secrets/shared/colibri.yaml
 ;; secrets/shared/ is for keys used by more than one machine (client or
 ;; server) — add a machine's age key to its .sops.yaml recipient list
 ;; and run `sops updatekeys` again to let it decrypt this too.
 #:sops-secrets
 (list
  (sops-secret
   (key '("api-key"))
   (file (local-file "../../secrets/shared/colibri.yaml"))
   (path "/run/secrets/colibri-api-key")
   (permissions #o400))
  (sops-secret
   (key '("password"))
   (file (local-file "../../secrets/shared/sillytavern.yaml"))
   (path "/run/secrets/sillytavern-password")
   (permissions #o400)))

 ;; Host-only services
 #:extra-services
 (list
  ;; Restored 2026-08-09: deleted in ec83cfe ("Remove unused Tailscale
  ;; and Certbot configurations", 2026-06-05) as apparently-dead code —
  ;; it wasn't unused, %fix-perms-hook's only caller was this service.
  ;; Deleting it silently stopped LE renewal for nug.peteches.co.uk,
  ;; which koboldcpp-rp and sillytavern's SSL both depend on. Renews
  ;; twice daily via the 'certbot-certificate-renewal shepherd timer
  ;; (manually: `sudo herd trigger certbot-certificate-renewal`), plus
  ;; once at boot via 'renew-certbot-certificates. deploy-hook re-runs
  ;; %fix-perms-hook to refresh ~/.local/share/certs/nug.peteches.co.uk.pem.
  (service certbot-service-type
           (certbot-configuration
            (email "certbot@peteches.co.uk")
            (certificates
             (list
              (certificate-configuration
               (domains '("nug.peteches.co.uk"))
               (deploy-hook %fix-perms-hook))))))
  (service comfyui-service-type
           (list
	    (comfyui-configuration
	     (listen "0.0.0.0,::")
	     (extra-model-paths-config %comfyui-model-paths)
	     (runtime-packages (list uv))
	     (open-firewall? #t)
	     (container-extra-shares (list "/media/ColdStorage"))
	     ;; nvcc for SageAttention's build-from-source install, via
	     ;; CUDA_HOME only — NOT container-extra-packages, since guix shell
	     ;; can't resolve a guix-science-nonfree package by name inside the
	     ;; FHS container (see comfyui.scm's container-extra-packages field
	     ;; comment); torch's own nvcc discovery checks CUDA_HOME first, so
	     ;; this direct store-path reference is all it needs, made reachable
	     ;; by the container's existing --expose=/gnu/store.
	     ;;
	     ;; cuda-13 (13.0.2), not the channel's default `cuda' (12.9,
	     ;; colibri-engine-cuda's build-time dependency in
	     ;; peteches/packages/colibri.scm — a separate, unrelated build) —
	     ;; torch's cpp_extension build refuses to compile against a CUDA
	     ;; toolkit that doesn't match the version torch.version.cuda
	     ;; reports for whatever torch wheel `uv sync' resolved for
	     ;; ComfyUI, and that was 13.0 as of the last sync. If a future
	     ;; ComfyUI/torch bump resolves a different CUDA-tagged wheel, this
	     ;; needs to be bumped to match — check
	     ;; /var/log/comfyui/comfyui/update.log's
	     ;; "RuntimeError: ('The detected CUDA version...)" message, which
	     ;; names both versions directly.
	     ;;
	     ;; CPATH is also required, separately: nvcc's own host-preprocessing
	     ;; pass doesn't go through the gcc driver's normal default system
	     ;; include search (confirmed live — a plain `gcc' invocation finds
	     ;; <linux/limits.h> in the FHS-unioned /usr/include fine, but nvcc
	     ;; invoking the same gcc for the CUDA-runtime-header preinclude
	     ;; pass reports "fatal error: linux/limits.h: No such file or
	     ;; directory" for that exact file). CPATH is Guix's own
	     ;; native/non-FHS header-search convention and gets honored
	     ;; regardless of how a build tool invokes the compiler internally,
	     ;; so it fixes this generally for any custom node's C/CUDA build,
	     ;; not just SageAttention's.
	     (extra-environment-variables
	      (list #~(string-append "CUDA_HOME=" #$cuda-13)
		    #~(string-append "CPATH="
				     #$(file-append linux-libre-headers
						    "/include"))
		    ;; Makes ComfyUI_VLM_nodes' llama-cpp-python source build
		    ;; (see its custom-node entry below) target the CUDA
		    ;; backend instead of GGML's CPU-only default. CMake's
		    ;; FindCUDAToolkit doesn't consult CUDA_HOME (that's
		    ;; torch's own convention) — it needs CUDAToolkit_ROOT
		    ;; explicitly, or `nvcc' fails with "CUDA Toolkit not
		    ;; found" even though CUDA_HOME above is set correctly.
		    ;; CUDAToolkit_ROOT alone finds the toolkit's headers/libs
		    ;; but enable_language(CUDA) still needs the compiler
		    ;; binary itself named explicitly via CMAKE_CUDA_COMPILER
		    ;; (or CUDACXX) — without it CMake reports "No
		    ;; CMAKE_CUDA_COMPILER could be found" despite the
		    ;; toolkit having already been located successfully.
		    #~(string-append "CMAKE_ARGS=-DGGML_CUDA=on -DCUDAToolkit_ROOT="
				     #$cuda-13
				     " -DCMAKE_CUDA_COMPILER="
				     #$(file-append cuda-13 "/bin/nvcc"))))
	     ;; Package installed but the global --use-sage-attention flag is
	     ;; deliberately left off (enable-sage-attention? default #f) — a
	     ;; per-workflow node (e.g. KJNodes' "Patch Sage Attention") gives
	     ;; finer control and is what video-model wrappers actually expect;
	     ;; see peteches/services/comfyui.scm's install-sage-attention?
	     ;; field comment.
	     (install-sage-attention? #t)
	     (custom-nodes
	      (list
	       ;; KJNodes — includes the "Patch Sage Attention" node used to
	       ;; apply SageAttention per-workflow instead of via the global
	       ;; --use-sage-attention flag (deliberately left off above).
	       (comfyui-custom-node
		(name "ComfyUI-KJNodes")
		(git-repo-url "https://github.com/kijai/ComfyUI-KJNodes"))
	       ;; rgthree-comfy — workflow QoL: Power Lora Loader, Fast Groups
	       ;; Bypasser/Muter, context/reroute nodes.
	       (comfyui-custom-node
		(name "rgthree-comfy")
		(git-repo-url "https://github.com/rgthree/rgthree-comfy"))
	       ;; VideoHelperSuite — VHS_LoadVideo/VHS_VideoCombine load/encode
	       ;; nodes used by the video workflows alongside KJNodes.
	       (comfyui-custom-node
		(name "ComfyUI-VideoHelperSuite")
		(git-repo-url "https://github.com/Kosinkadink/ComfyUI-VideoHelperSuite"))
	       ;; Spectrum: training-free diffusion acceleration via cached/
	       ;; forecasted denoiser features, one repo per model backend.
	       (comfyui-custom-node
		(name "ComfyUI-Spectrum-Proper")
		(git-repo-url "https://github.com/xmarre/ComfyUI-Spectrum-Proper"))
	       (comfyui-custom-node
		(name "ComfyUI-Spectrum-SDXL-Proper")
		(git-repo-url "https://github.com/xmarre/ComfyUI-Spectrum-SDXL-Proper"))
	       (comfyui-custom-node
		(name "ComfyUI-Spectrum-WAN-Proper")
		(git-repo-url "https://github.com/xmarre/ComfyUI-Spectrum-WAN-Proper"))
	       (comfyui-custom-node
		(name "ComfyUI-Spectrum-MiniMax-H3")
		(git-repo-url "https://github.com/xmarre/ComfyUI-Spectrum-MiniMax-H3"))
	       ;; LoadImageFromUrl / LoadVideoFromUrl.
	       (comfyui-custom-node
		(name "comfyui-art-venture")
		(git-repo-url "https://github.com/sipherxyz/comfyui-art-venture"))
	       ;; Local GGUF LLM loader for MiniMax H3 / Qwen-Image prompt
	       ;; writing: LLMLoader runs llama.cpp in-process (no external
	       ;; server), feeding LLMSampler/LLMPromptGenerator/
	       ;; CreativeArtPromptGenerator/Suggester. VLMJSONExtract (same
	       ;; pack) parses Suggester's raw JSON output. llama-cpp-python
	       ;; itself is split into requirements-llama-cpp.txt (not
	       ;; requirements.txt) so a plain install doesn't pull in a
	       ;; from-source compile by default — opt in via
	       ;; extra-requirements-files. CMAKE_ARGS below (in
	       ;; extra-environment-variables) makes that source build target
	       ;; CUDA instead of the GGML CPU backend.
	       (comfyui-custom-node
		(name "ComfyUI_VLM_nodes")
		(git-repo-url "https://github.com/gokayfem/ComfyUI_VLM_nodes")
		(extra-requirements-files (list "requirements-llama-cpp.txt")))
	       ;; MiniMax H3 "Plan v2" prompt-building nodes (Project Setup,
	       ;; Reference/Shot nodes, Prompt Merge, Apply Reference Plan,
	       ;; Qwen enhancement) used by the H3-enhanced-prompt workflow.
	       ;; No Python dependencies; requires ComfyUI's native H3 nodes
	       ;; (Image/Reference to Video, tokenizer), already present.
	       (comfyui-custom-node
		(name "ComfyUI-MiniMax-H3-Guide")
		(git-repo-url "https://github.com/ethanfel/ComfyUI-MiniMax-H3-Guide"))
	       ;; Voice generation: Qwen3-TTS VoiceDesign (unique voice from a
	       ;; text description) feeding ChatterBox's zero-shot voice
	       ;; conversion (clone that sample onto our own performance
	       ;; recordings, no per-voice training). Model files go under
	       ;; the TTS category registered in %comfyui-model-paths above.
	       ;; extra-pip-packages: OmniVoice's `omnivoice' PyPI package is
	       ;; deliberately left out of TTS-Audio-Suite's own
	       ;; requirements.txt ("We install official OmniVoice separately
	       ;; to avoid letting pip reshape shared deps") — upstream's own
	       ;; install.py installs it standalone with --no-deps, which our
	       ;; sync service doesn't run. Without this, OmniVoice's engine
	       ;; node fails at runtime with "ModuleNotFoundError: No module
	       ;; named 'omnivoice'" — hit this trying OmniVoice's documented
	       ;; "british accent" instruct attribute (k2-fsa/OmniVoice model
	       ;; card), the one engine in this suite with real trained accent
	       ;; control rather than delivery/tone-only instructions.
	       (comfyui-custom-node
		(name "TTS-Audio-Suite")
		(git-repo-url "https://github.com/diodiogod/TTS-Audio-Suite")
		(extra-pip-packages (list "omnivoice")))
	       ;; Semitone pitch-shift for layered voice-design mixes (e.g. a
	       ;; tritone doubling of a TTS Text render before AudioMerge) —
	       ;; TTS-Audio-Suite's own pitch control lives inside its RVC
	       ;; engine and requires a pretrained per-voice .pth model, which
	       ;; doesn't exist for a freshly-designed voice. Lightweight,
	       ;; pure-CPU phase vocoder — no torchaudio/ML dependency.
	       (comfyui-custom-node
		(name "comfyui-audio-pitch")
		(git-repo-url "https://github.com/Takenoko3333/comfyui-audio-pitch")))))))
  ;; CUDA-enabled (colibri-engine-cuda, sm_89/Ada — see
  ;; peteches/packages/colibri.scm), capped at vram-gb 12: a static middle
  ;; ground, not a dynamic split — colibri has no live/signal-based VRAM
  ;; reconfiguration (fixed at process start) and this repo has no cross-
  ;; service reactive/watcher pattern to build "colibri yields when ComfyUI
  ;; needs VRAM" on, so a restart-cycling watcher was ruled out as its own
  ;; project that would cost the warm cache it's meant to protect. 12GB +
  ;; the engine's own ~2GB CUDA_RESERVE_GB leaves ComfyUI ~10GB — measured
  ;; via `coli plan`/`coli doctor` to matter: projected_hit_rate was 9.35%
  ;; at the previous 6GB cap vs 24.13% with the full card free. ComfyUI is
  ;; needed alongside colibri specifically for in-roleplay illustrations via
  ;; SillyTavern's image-gen extension, not just separate heavy sessions, so
  ;; some headroom stays reserved rather than maximizing colibri outright.
  ;; Reached via Caddy at
  ;; colibri.ts.peteches.co.uk -> nug.spaniel-cordylus.ts.net:8000 (see
  ;; peteches/systems/caddy.scm); --allowed-host must match that public
  ;; domain because Caddy forwards the client's original Host header
  ;; unchanged, and colibri's DNS-rebinding guard otherwise 403s anything
  ;; that isn't loopback or its own literal bind address.
  ;;
  ;; model-dir lives on HotStorage (NVMe), not ColdStorage (spinning HDD):
  ;; expert streaming is a random-read workload, which a platter disk
  ;; handles catastrophically worse than NVMe.
  ;;
  ;; model-dir and model-mirror both point at a stable "current" symlink
  ;; under models/colibri/ (identical layout on both drives) rather than a
  ;; specific model's directory directly, so switching models is a
  ;; filesystem operation (repoint both symlinks, chown the new targets,
  ;; herd restart) — no guix system reconfigure needed for routine swaps.
  ;; Model directories live as siblings of current, e.g. .../glm-5.2,
  ;; .../deepseek-v4-flash. The config was made to match each drive's
  ;; actual on-disk layout rather than the other way around.
  ;;
  ;; Keeping both symlinks pointing at the SAME model is a manual
  ;; discipline requirement now, not something enforced automatically:
  ;; colibri expects primary and mirror to be byte-identical copies of the
  ;; same model, and there's nothing stopping HotStorage's current and
  ;; WarmStorage's current from being repointed independently and
  ;; silently desyncing. Repoint them together.
  ;;
  ;; direct-io? and pipe? aren't specific to the mirror existing — DIRECT
  ;; (O_DIRECT) is a general NVMe win per upstream's own guidance,
  ;; independent of whether a second copy exists; pipe? overlaps expert
  ;; disk-loads with matmul via I/O worker threads instead of doing them
  ;; sequentially — byte-identical output, purely a scheduling change.
  ;; pipe-workers left at the engine's own default (8) until there's a
  ;; measurement to tune it against.
  (service colibri-service-type
           (colibri-configuration
            (package colibri-engine-cuda)
            (model-dir "/media/HotStorage/models/colibri/current")
            (model-mirror "/media/WarmStorage/models/colibri/current")
            (direct-io? #t)
            (pipe? #t)
            ;; Per-turn latency percentiles, expert-I/O totals, cache-tier
            ;; fill and a tuning verdict on stderr -> colibri.log. Purely
            ;; diagnostic output, no behavioral effect.
            (perf-metrics? #t)
            ;; Both straight from coli plan/doctor's own tuning verdict for
            ;; this deployment: draft-tokens 0 disables MTP speculation,
            ;; which at this cache-hit rate widens the expert union and
            ;; adds disk reads rather than saving time (byte-identical
            ;; output regardless — drafts are always verified, never
            ;; trusted blindly); cuda-pipe? engages the multi-step GPU
            ;; attention pipeline, recommended for this single-GPU box.
            (draft-tokens 0)
            (cuda-pipe? #t)
            (host "0.0.0.0")
            (open-firewall? #t)
            (auto-start? #t)
            (gpu "auto")
            (vram-gb 12)
            ;; Required: colibri refuses to bind beyond localhost without an
            ;; API key (COLI_ALLOW_INSECURE_BIND=1 exists to bypass this,
            ;; deliberately not used — this is reachable over Tailscale via
            ;; Caddy, not just loopback). Decrypted from
            ;; secrets/shared/colibri.yaml by the sops-secrets entry above
            ;; at boot, not a manually-placed file — see #:sops-secrets.
            (api-key-file "/run/secrets/colibri-api-key")
            (allowed-hosts (list "colibri.ts.peteches.co.uk"))))
  ;; Verified directly (not just reasoned about): a plain `npm install` on
  ;; this codebase needed no native compilation, and `node server.js`
  ;; served HTTP cleanly — no FHS-container treatment needed here, unlike
  ;; ComfyUI. Port 8001, not upstream's default 8000, to avoid colliding
  ;; with colibri on the same box. Same host-header-guard reasoning as
  ;; colibri: reachable via Caddy over Tailscale, so whitelistMode (IP-
  ;; based) is off in favour of hostWhitelist (Host-header based) + basic
  ;; auth — see peteches/services/sillytavern.scm's module comment.
  (service sillytavern-service-type
           (sillytavern-configuration
            (host "0.0.0.0")
            (open-firewall? #t)
            (basic-auth? #t)
            (basic-auth-password-file "/run/secrets/sillytavern-password")
            (allowed-hosts (list "sillytavern.ts.peteches.co.uk"))))
  (simple-service 'nug-guix-publish-firewall
                  firewall-service-type
                  (nftables-rules
                   (input (list "tcp dport 3000 accept comment \"guix-publish\""))))
  (service guix-publish-service-type
           (guix-publish-configuration
            (host "::")
            (port 3000)
            (compression '(("zstd" 9)))
            (advertise? #t)
            (cache "/var/cache/guix/publish")))
  ;; Trusts claude-workstation's Guix archive signing key so nug's daemon
  ;; accepts store items it sends during offload -- separate from the SSH
  ;; guix-offload key above, which only grants login. Without this, offload
  ;; connects fine but every export fails with "unauthorized public key"
  ;; and silently falls back to a local build.
  (simple-service 'guix-offload-signing-keys
                  guix-service-type
                  (guix-extension
                   (authorized-keys
                    (list (plain-file "claude-workstation-signing-key.pub"
                                      "(public-key \n (ecc \n  (curve Ed25519)\n  (q #EFED7FDADFFF4E2559977AFD10310E21C4EEF7685C6297595D5333CBEF037EDE#)\n  )\n )\n")))))
  (simple-service 'guix-offload-authorized-keys
                  openssh-service-type
                  `(("guix-offload"
                     ,(plain-file "claude-workstation-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIVVAw63bjPSR1Pmm8MixkVZgNYty3IrWbJMyWe7CEVo guix-offload@claude-workstation\n")
                     ,(plain-file "arr-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJCmWvNZoNQhpVpeYU6VXtYcrtS8XfgrK5S5WCs5OtM1 guix-offload@arr\n")
                     ,(plain-file "nyarlothotep.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEP+/uHdoUNfL+LuniZGTEwPJkxvSgDpuR58yxfw/u74 guix-build@nyarlothotep\n")
                     ,(plain-file "caddy-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ5OyvcOFlI3lnunv9FzkOms2CO9i7y12EnSSBDmp6ob guix-offload@caddy\n")
                     ,(plain-file "downloads-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOTVFFGPzZsX0hV4fY2bhptvW1Zs6lilcYMGTOli1UoL guix-offload@downloads\n")
                     ,(plain-file "git-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDP/krMF4ECdoMVIqv9K5mZHvbJUv7+ZFSx2FlVlHSOf guix-offload@git\n")
                     ,(plain-file "grafana-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINw3+hJHHSzwhGquTWRtXx5+uVdvarpu3gJGCnXrj61Q guix-offload@grafana\n")
                     ,(plain-file "jellyfin-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILGGBMFH7Bei/lTu2s4xqFveXOmxOdqHGQiVmnDRfBt5 guix-offload@jellyfin\n")
                     ,(plain-file "loki-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN9R8rSJi1VRa2okQhXFxxBHJXwmV1rVOl8HelpepFVg guix-offload@loki\n")
                     ,(plain-file "pihole-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMNAir7xhAl7Z50tloQKOfCeVPqTqDgmIuSVxtfFdLES guix-offload@pihole\n")
                     ,(plain-file "prometheus-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM+nhIk+ySFYMj7I4SDwA/LKyM8MH3+8NMIabyAIuMSC guix-offload@prometheus\n")
                     ,(plain-file "prowlarr-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPxWQLDvXJGgp4HsOpSEMyLTGi0lL2zYcvRvARuVv/nU guix-offload@prowlarr\n")
                     ,(plain-file "rustdesk-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII0DTgAJjaG1+0STwTBDRfUrbP/q0KFVnY5OdjrqKasS guix-offload@rustdesk\n"))))))
