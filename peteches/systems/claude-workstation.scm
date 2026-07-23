;; claude-workstation.scm — headless VM hosting Claude Code for two accounts.
;;
;; One VM, two login users, one per Anthropic account:
;;   peteches      — the personal account (the baseline %vm-peteches-user
;;                   that every VM already gets from vm-base.scm)
;;   criticalgrind — the Critical Grind work account (added here)
;;
;; Each user runs their OWN `guix home reconfigure' against a config in
;; peteches/home/configs/:
;;   claude-workstation-peteches.scm
;;   claude-workstation-criticalgrind.scm
;; Both instantiate the shared constructor in
;; (peteches home modules claude-workstation): it installs claude-code,
;; symlinks ~/.claude from configs/claude/defaults, registers that account's
;; MCP servers, and pre-clones repos into ~/area_51/<repo>.
;;
;; WHY separate OS users rather than the container-session wrapper: each user
;; gets its own ~/.claude.json (auth + per-project state), so the two accounts
;; never race on that file and run in parallel for free — the exact thing the
;; container wrapper worked hard to fake on one identity.
;;
;; FIRST-BOOT TODO (none of these can be done before the VM exists):
;;   * age-keys/claude-workstation.pub + a .sops.yaml creation rule, then wire
;;     any sops-secrets (e.g. the Plane/Outline API keys for criticalgrind).
;;   * flip #:with-nug-offload? back to #t once a guix-build.yaml secret AND
;;     the VM's guix-offload public key in nug.scm both exist (half-wiring it
;;     fails silently — see vm-base.scm).
;;   * fill the host-key in machines.scm (ssh-keyscan after first boot).
;; See CLAUDE.md "Adding a New VM" for the remaining fleet-integration files
;; (ssh.scm, machines.scm, scripts/deploy.scm, pihole.scm, monitored-hosts,
;; prometheus.scm, proxmox-vms.org, infra/terraform).

(define-module (peteches systems claude-workstation)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services tailscale)
  #:use-module (sops secrets)
  #:use-module (gnu services guix)
  #:use-module (peteches home configs claude-workstation-peteches)
  #:use-module (peteches home configs claude-workstation-criticalgrind)
  #:export (claude-workstation-os))

;; Console-recovery user for the criticalgrind account.  Same shape as
;; %vm-peteches-user (vm-base.scm): day-to-day access is SSH key-only, so this
;; pre-hashed password exists only for the Proxmox console.  It reuses the
;; already-committed peteches hash so there is a known console login; give it
;; its own hash later if the accounts should differ.
(define %criticalgrind-user
  (user-account
   (name "criticalgrind")
   (comment "Critical Grind (Claude account)")
   (group "users")
   (home-directory "/home/criticalgrind")
   (supplementary-groups '("wheel" "netdev"))
   (password "$6$yk5pnJr/ECPPOvGv$/HoWZNE7fWDslHHIVHAcaxk0AyhnthoHGhs3RrXaXqvVL8W5UI9OUVHndx4RfSqnWnnPw/.q2KhkfrPRKkw.11")))

(define-public claude-workstation-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "claude-workstation.peteches.co.uk"
     #:ipv4-address "192.168.51.205/23"
     #:ipv6-address "2a10:d582:ef59::111/64"
     ;; Offload needs a guix-build.yaml secret + nug authorized-key that do
     ;; not exist until first boot; enabling half of it fails silently.
     #:with-nug-offload? #f
     #:users-extra (list %criticalgrind-user)
     ;; Tailscale unattended join.  The auth-key is a SHARED sops secret
     ;; (secrets/shared/tailscale.yaml), decrypted at boot with the VM's own
     ;; age key.  The VM pipeline must add this host's age key as a recipient
     ;; of that file (re-encrypt) before the join can succeed; until then
     ;; tailscaled starts but cannot authenticate.
     #:sops-secrets
     (list
      (sops-secret
       (key '("auth-key"))
       (file (local-file "../../secrets/shared/tailscale.yaml"))
       (path "/run/secrets/tailscale-auth-key")
       (permissions #o400)))
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
     #:extra-services
     (list
      (service guix-home-service-type
       `(("peteches" ,claude-workstation-peteches-home)
         ("criticalgrind" ,claude-workstation-criticalgrind-home)))
      ;; Claude reaches Anthropic outbound; the base firewall permits
      ;; established/related + output and opens ssh inbound.  Tailscale needs
      ;; no inbound rule here: ssh over the tailnet still lands on tcp/22
      ;; (already open) and tailscaled's own traffic is outbound.
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (auth-key-file "/run/secrets/tailscale-auth-key"))))
      (service alloy-service-type
               (alloy-configuration
                (hostname "claude-workstation.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy"))))))))))

claude-workstation-os
