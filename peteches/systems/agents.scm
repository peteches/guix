;; agents.scm — "agents" VM: hosts rules-lawyer (Discord bot) and sillytavern
;; (LLM roleplay frontend), both backed by the koboldcpp OpenAI endpoint on
;; the comfyui box (proxied via caddy as koboldcpp.ts.peteches.co.uk).
;;
;; NOTE: rules-lawyer service requires (gnu packages admin) shadow import —
;; fixed in 3806683. Placeholder SOPS files added for image build;
;; real encryption pending age key generation.
;;
;; Services:
;;   rules-lawyer — Discord bot, uses koboldcpp.ts.peteches.co.uk as its LLM
;;   sillytavern  — LLM roleplay web frontend on :8001, basic-auth protected
;;   tailscale    — MagicDNS mesh connectivity
;;
;; Secrets (SOPS-decrypted at boot):
;;   /run/secrets/rules-lawyer-discord-token  — Discord bot token
;;   /run/secrets/sillytavern-password        — sillytavern basic-auth password
;;   /run/secrets/restic-password             — restic backup password
;;   /run/secrets/restic-ssh-key              — restic backup SSH key
;;   /run/secrets/guix-offload-key            — guix build-offload SSH key
;;
;; FIRST-BOOT TODO:
;;   * age-keys/agents.pub + a .sops.yaml creation rule
;;   * create + encrypt secrets/hosts/agents/rules-lawyer.yaml (Discord token)
;;   * create + encrypt secrets/hosts/agents/sillytavern.yaml (basic-auth pw)
;;   * create + encrypt secrets/hosts/agents/restic.yaml (restic creds)
;;   * create + encrypt secrets/hosts/agents/guix-build.yaml (offload key)
;; See AGENTS.md "Adding a New VM" for the remaining fleet-integration files.

(define-module (peteches systems agents)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services restic)
  #:use-module (peteches services rules-lawyer)
  #:use-module (peteches services sillytavern)
  #:use-module (peteches services tailscale)
  #:use-module (sops secrets)
  #:export (agents-os))

(define-public agents-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "agents.peteches.co.uk"
     #:ipv4-address "192.168.51.208/23"
     #:ipv6-address "2a10:d582:ef59::108/64"
     #:bootloader
     (bootloader-configuration
      (bootloader grub-efi-removable-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout (keyboard-layout "us")))
     #:file-systems
     (list
      (file-system
       (mount-point "/boot/efi")
       (device (file-system-label "GNU-ESP"))
       (type "vfat"))
      (file-system
       (mount-point "/")
       (device "/dev/vda2")
       (type "ext4")))
     #:restic-config
     (restic-vm-backup-configuration
      (vm-name "agents")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/rules-lawyer" "/var/lib/sillytavern"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("discord-token"))
       (file (local-file "../../secrets/hosts/agents/rules-lawyer.yaml"))
       (path "/run/secrets/rules-lawyer-discord-token")
       (permissions #o400))
      (sops-secret
       (key '("basic-auth-password"))
       (file (local-file "../../secrets/hosts/agents/sillytavern.yaml"))
       (path "/run/secrets/sillytavern-password")
       (permissions #o400))
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/agents/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/agents/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400))
      (sops-secret
       (key '("ssh-private-key"))
       (file (local-file "../../secrets/hosts/agents/guix-build.yaml"))
       (path "/run/secrets/guix-offload-key")
       (permissions #o400)))
     #:extra-services
     (list
      ;; rules-lawyer — Discord bot, uses the koboldcpp endpoint on the
      ;; comfyui box (proxied via caddy) as its LLM backend.
      (service rules-lawyer-service-type
               (rules-lawyer-configuration
                (llm-base-url "http://koboldcpp.ts.peteches.co.uk/v1")
                (llm-model "local-model")
                (discord-token-file "/run/secrets/rules-lawyer-discord-token")))
      ;; sillytavern — LLM roleplay web frontend on :8001, basic-auth
      ;; protected. LLM backend (koboldcpp) is configured through the
      ;; SillyTavern web UI after first start, not via service config.
      ;; (host "0.0.0.0") rather than the 127.0.0.1 default: caddy's
      ;; upstream for sillytavern.ts.peteches.co.uk is
      ;; agents.spaniel-cordylus.ts.net:8001 — it connects over Tailscale,
      ;; so the bind has to cover the tailscale interface, exactly as
      ;; guix-build.scm and comfyui.scm do for the same reason. Off-box
      ;; exposure stays gated: --whitelist false plus
      ;; SILLYTAVERN_HOSTWHITELIST_HOSTS below rejects any Host header
      ;; outside the two names, and basic auth is on.
      (service sillytavern-service-type
               (sillytavern-configuration
                (host "0.0.0.0")
                (port 8001)
                (basic-auth? #t)
                (basic-auth-username "peteches")
                (basic-auth-password-file "/run/secrets/sillytavern-password")
                (allowed-hosts '("agents.spaniel-cordylus.ts.net" "sillytavern.ts.peteches.co.uk"))))
      ;; Open the firewall for sillytavern.
      (simple-service 'sillytavern-firewall firewall-service-type
                      (nftables-rules
                       (input (list "tcp dport 8001 accept comment \"sillytavern\""))))
      ;; Tailscale — MagicDNS mesh connectivity.
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches"))))
      ;; Alloy — telemetry collector.
      (service alloy-service-type
               (alloy-configuration
                (hostname "agents.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/rules-lawyer.log" "rules-lawyer")
                                 (cons "/var/log/alloy.log" "alloy")
                                  (cons "/var/log/tailscaled-*.log" "tailscale"))))))))))

agents-os