;; agents.scm — "agents" VM: hosts rules-lawyer (Discord bot) and sillytavern
;; (LLM roleplay frontend).  rules-lawyer's LLM backend is the strata MoE
;; engine on the comfyui box (proxied via caddy as strata.ts.peteches.co.uk);
;; sillytavern picks its own backend in its web UI (see below).
;;
;; NOTE: rules-lawyer service requires (gnu packages admin) shadow import —
;; fixed in 3806683, and the service's start gexp was rewritten in the
;; launch fix (see (peteches services rules-lawyer) commentary — the bot was
;; not running at all before that).  The SOPS secrets here are the real
;; encrypted files (age key + .sops.yaml rule for agents landed in 75b6ce9).
;;
;; Services:
;;   rules-lawyer — Discord bot, uses strata.ts.peteches.co.uk as its LLM
;;   sillytavern  — LLM roleplay web frontend on :8001, basic-auth protected
;;   tailscale    — MagicDNS mesh connectivity
;;   guix-home    — ONE home environment, `peteches', from
;;                  peteches/home/configs/agents-peteches.scm: the box's agent
;;                  account (`pi' + the shared configs/pi/defaults, git, ssh,
;;                  proxychains, pinned channels, plus the store-served herdr
;;                  binary that lets the desktop client attach here).  It exists so
;;                  `ssh agents "pi …"' has a real pi to run under a real
;;                  account; headless VMs have no login shell that would
;;                  otherwise give an agent CLI anywhere to live.  No Claude
;;                  Code anywhere on this box — that is the whole point of
;;                  the agents/vm-jumpbox split (see that config's header).
;;                  Since 2026-11-05 it also carries the FLEET DEPLOY IDENTITY:
;;                  `Host * -> IdentityFile /run/secrets/peteches-automation-ssh-key',
;;                  the private half of the automation keypair every VM
;;                  authorizes for the peteches login, moved here from
;;                  claude-workstation when that box stopped being a deploy
;;                  origin.  So scripts/deploy.scm / the deploy-vms skill can
;;                  run from here as well as from the desktops -- %deploy-identity
;;                  in peteches/machines.scm resolves to that path on whatever
;;                  box it is evaluated on, and it now exists here. The other
;;                  half of "can this box deploy" is not an SSH key at all:
;;                  this box's own guix-daemon signing key has to be in
;;                  %authorize-coordinator-key (peteches/systems/common.scm),
;;                  or the last deploy step -- the live
;;                  upgrade-shepherd-services archive send -- fails with
;;                  `unauthorized public key' (docs/fleet-deployment.org §4).
;;
;; DEPLOY ORIGIN, and the honest caveats attached to it: holding the
;; automation key makes a deploy FROM this box possible, not sufficient. Two
;; things are still missing on the live box as of 2026-11-05: no ~/area_51/guix
;; checkout for scripts/deploy.scm to run from (~/ here is empty of repos),
;; and no critical-grind deploy key in an ssh-agent, so that one channel cannot
;; be pulled here. Deploys therefore still run from the desktops until someone
;; clones the repo here -- see the deploy-vms skill, which documents the repo
;; root as its working directory.
;;   herdr-server -- the box's agent-session server, run as `peteches' so pi
;;                  sessions survive disconnects AND reboots.  Supervised HERE,
;;                  at the system layer, not by home-shepherd as the retired
;;                  (peteches home modules claude-workstation) did: a headless
;;                  VM never runs a login shell, so user shepherd never starts
;;                  on it -- confirmed on this box, where `herd status' fails
;;                  with /run/user/1000/shepherd/socket missing.  See the long
;;                  note in (peteches services herdr).  The client half lives
;;                  on dagon, whose herdr-mx sidebar attaches over SSH; the
;;                  store binary in this account's home profile is what makes
;;                  that attach see an already-installed remote instead of
;;                  trying to seed one (and ~/.local/bin/herdr must stay
;;                  absent -- it is what those failed seedings left behind).
;;                  Unix-socket only, so nothing is opened in the firewall:
;;                  the attach rides the already-open ssh port.
;;
;; Secrets (SOPS-decrypted at boot):
;;   /run/secrets/rules-lawyer-discord-token  — Discord bot token
;;   /run/secrets/sillytavern-password        — sillytavern basic-auth password
;;   /run/secrets/restic-password             — restic backup password
;;   /run/secrets/restic-ssh-key              — restic backup SSH key
;;   /run/secrets/guix-offload-key            — guix build-offload SSH key
;;   /run/secrets/peteches-automation-ssh-key — private half of the fleet-wide
;;     peteches automation keypair, i.e. this box's deploy identity (moved
;;     here from claude-workstation on 2026-11-05; only this box and the
;;     operator's PGP key can decrypt it)
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
  #:use-module (gnu services guix)           ;; guix-home-service-type
  #:use-module (peteches systems vm-base)
  #:use-module (peteches home configs agents-peteches)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services herdr)
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
       (permissions #o400))
      ;; Private half of the peteches automation SSH keypair, moved here from
      ;; claude-workstation on 2026-11-05 -- its public half is decrypted on
      ;; EVERY vm by vm-base's %automation-authorized-key-secret and authorized
      ;; for the peteches login by that module's sshd Match block, so holding
      ;; this file is what lets one key open every VM in the fleet. Read
      ;; (user "peteches") because two consumers need it and both run as that
      ;; user: the `Host *' IdentityFile in
      ;; peteches/home/configs/agents-peteches.scm, and %deploy-identity in
      ;; peteches/machines.scm when a deploy is run from here. Only this box
      ;; decrypts it -- the file is age-encrypted for `agents' key plus the
      ;; operator's PGP key (see .sops.yaml's hosts/agents rule), which is
      ;; also why claude-workstation, whose age key was the sole age recipient
      ;; before this move, can no longer read it at all.
      (sops-secret
       (key '("private-key"))
       (file (local-file "../../secrets/hosts/agents/peteches-automation-ssh.yaml"))
       (user "peteches")
       (group "users")
       (permissions #o400)
       (path "/run/secrets/peteches-automation-ssh-key")))
     #:extra-services
     (list
      ;; One home environment for this box's agent account -- see
      ;; peteches/home/configs/agents-peteches.scm for what it contains and
      ;; what it deliberately leaves out (no Claude Code, no anvil, no MCP
      ;; servers -- the herdr *server* is supervised at the system level above,
      ;; this profile only carries the binary the ssh PATH probe has to find).
      ;; `guix-home-peteches' owns ~/.gitconfig, ~/.ssh/config,
      ;; ~/.profile and ~/.config/guix/channels.scm as managed symlinks, so a
      ;; hand-edited dotfile in this account gets replaced on the next
      ;; activation -- that is the trade this box makes for having an
      ;; agent-capable account at all.  Nothing here needs XDG_RUNTIME_DIR:
      ;; (peteches home modules vm-jumpbox) sets no runtime directory, and
      ;; home-pi-service-type only writes files under ~/.pi.
      (service guix-home-service-type
               `(("peteches" ,agents-peteches-home)))
      ;; This account's herdr server.  The configuration record's defaults are
      ;; what this box needs: it requires the `guix-home-peteches' one-shot
      ;; first so the home profile PATH points at really exists, and its log
      ;; lands in /var/log where alloy ships it to Loki (next block below).
      ;; PATH has to name the home profile by hand because herdr's pane shells
      ;; are not login shells on Linux -- see (peteches services herdr).
      (service herdr-server-service-type
               (herdr-server-configuration
                (user "peteches")
                (group "users")))
      ;; rules-lawyer — Discord bot, uses the strata engine on the comfyui
      ;; box (Intel Arc B70, SC117 Qwen3.8-Flash-Next IQ2_XS, proxied via
      ;; caddy as strata.ts.peteches.co.uk -> comfyui:8000) as its LLM
      ;; backend.  Successor to the koboldcpp endpoint: koboldcpp holds the
      ;; RTX 4090 and is auto-start? #f, so answers depended on someone
      ;; remembering to `herd start koboldcpp`, whereas strata auto-starts
      ;; at boot.  Strata runs with no API key, so no llm-api-key-file is
      ;; configured (and none of the comfyui strata config sets one).
      ;; The model name is the id strata reports from /v1/models; the
      ;; '(ice-9 textual-ports) get-string-all' / quoted-splice / deferred
      ;; constructor gotchas that kept this service from ever starting live
      ;; in (peteches services rules-lawyer)'s commentary.
      (service rules-lawyer-service-type
               (rules-lawyer-configuration
                (llm-base-url "http://strata.ts.peteches.co.uk/v1")
                (llm-model "qwen3.8-flash-next")
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
                                 (cons "/var/log/herdr-server.log" "herdr")
                                 (cons "/var/log/alloy.log" "alloy")
                                  (cons "/var/log/tailscaled-*.log" "tailscale"))))))))))

agents-os
