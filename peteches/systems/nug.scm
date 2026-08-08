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
  #:use-module (gnu packages rust-apps))

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
  (service comfyui-service-type
           (list
	    (comfyui-configuration
	     (listen "0.0.0.0,::")
	     (extra-model-paths-config %comfyui-model-paths)
	     (runtime-packages (list uv))
	     (open-firewall? #t)
	     (container-extra-shares (list "/media/ColdStorage")))))
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
  ;; handles catastrophically worse than NVMe. model-mirror duplicates the
  ;; model onto the WarmStorage NVMe so expert reads split across both
  ;; drives (COLI_MODEL_MIRROR) instead of bottlenecking on one. direct-io?
  ;; is enabled to keep the two copies from competing for page cache, per
  ;; upstream's own guidance for a mirrored setup — worth re-measuring if
  ;; either drive turns out to be QLC/DRAM-less, where DIRECT can be neutral
  ;; to negative instead. pipe? is direct-io?'s documented pairing:
  ;; overlaps expert disk-loads with matmul via I/O worker threads instead
  ;; of doing them sequentially — byte-identical output, purely a
  ;; scheduling change. pipe-workers left at the engine's own default (8)
  ;; until there's a measurement to tune it against.
  (service colibri-service-type
           (colibri-configuration
            (package colibri-engine-cuda)
            (model-dir "/media/HotStorage/models/colibri")
            (model-mirror "/media/WarmStorage/models/colibri")
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
  (simple-service 'guix-offload-authorized-keys
                  openssh-service-type
                  `(("guix-offload"
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
