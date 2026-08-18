;; claude-workstation.scm — headless VM hosting Claude Code for three accounts.
;;
;; One VM, three login users, one per identity:
;;   peteches      — the personal account (the baseline %vm-peteches-user
;;                   that every VM already gets from vm-base.scm)
;;   criticalgrind — the Critical Grind work account (added here)
;;   ygo           — Peter McCabe's ygo.ai identity (added here)
;;
;; Each user runs their OWN `guix home reconfigure' against a config in
;; peteches/home/configs/:
;;   claude-workstation-peteches.scm
;;   claude-workstation-criticalgrind.scm
;;   claude-workstation-ygo.scm
;; All three instantiate the shared constructor in
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
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services tailscale)
  #:use-module (peteches services wireguard-socks5)
  #:use-module (sops secrets)
  #:use-module (gnu services guix)
  #:use-module (peteches home configs claude-workstation-peteches)
  #:use-module (peteches home configs claude-workstation-criticalgrind)
  #:use-module (peteches home configs claude-workstation-ygo)
  #:export (claude-workstation-os))

;; This VM has no elogind/PAM session management (vm-base.scm starts from
;; bare %base-services, not %desktop-services), so nothing ever creates
;; /run/user/<uid> -- and /run itself isn't even tmpfs here, only /dev/shm
;; is. (peteches home modules claude-workstation) points each account's
;; XDG_RUNTIME_DIR at ~/.cache/xdg-runtime instead, since that's always
;; writable, but that directory lives on the root ext4 filesystem and
;; therefore survives reboots -- which silently breaks Guix Home's
;; on-first-login bootstrap (the script that starts shepherd-for-home,
;; supervising anvil and herdr): it's guarded by a flag file inside that
;; same directory, meant to fire once per boot, that without a real tmpfs
;; there only ever fires once for the account's entire lifetime. A small
;; per-account tmpfs mounted at that exact path restores real "cleared
;; every boot" semantics without pulling in elogind's full PAM/D-Bus
;; session stack. UIDs are static for these three accounts (verified via
;; `id`): peteches=1000, criticalgrind=1001, ygo=1002.
(define (xdg-runtime-tmpfs user uid)
  (file-system
   (mount-point (string-append "/home/" user "/.cache/xdg-runtime"))
   (device "none")
   (type "tmpfs")
   (options (string-append "size=64M,mode=0700,uid=" (number->string uid)
                            ",gid=998"))
   (check? #f)))

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

;; Console-recovery user for the ygo account.  Same shape as %criticalgrind-user
;; and reuses the same pre-hashed console password -- day-to-day access is
;; SSH key-only.
(define %ygo-user
  (user-account
   (name "ygo")
   (comment "Peter McCabe (ygo.ai account)")
   (group "users")
   (home-directory "/home/ygo")
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
     #:users-extra (list %criticalgrind-user %ygo-user)
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
       (permissions #o400))
      ;; Full wg-quick config (private key, peer public key, endpoint,
      ;; tunnel address) as ONE opaque secret value -- see (peteches
      ;; services wireguard-socks5) and docs/secrets-management.org for the
      ;; expected wg0.conf template. wg0 runs inside its own network
      ;; namespace, so the template no longer needs Table/PostUp/PreDown
      ;; lines -- a plain default route via wg0 already sends everything in
      ;; that namespace over the tunnel. The path MUST be named
      ;; "wg0.conf" (basename "wg0") to match the "wg0" interface name used
      ;; below, since wg-quick derives the interface name from the config
      ;; file's basename.
      (sops-secret
       (key '("wg0-conf"))
       (file (local-file "../../secrets/hosts/claude-workstation/wireguard.yaml"))
       (path "/run/secrets/wg0.conf")
       (permissions #o400))
      ;; criticalgrind's Plane/Outline MCP keys, decrypted here with this
      ;; VM's own age key. secrets/hosts/claude-workstation/critical-grind.env
      ;; also carries the operator's PGP key as a recipient, purely so it
      ;; can be edited from a desktop with gpg-agent -- see
      ;; docs/secrets-management.org.
      (sops-secret
       (key '("PLANE_API_KEY"))
       (file (local-file "../../secrets/hosts/claude-workstation/critical-grind.env"))
       (user "criticalgrind")
       (group "users")
       (permissions #o400)
       (path "/run/secrets/plane-api-key"))
      (sops-secret
       (key '("OUTLINE_API_KEY"))
       (file (local-file "../../secrets/hosts/claude-workstation/critical-grind.env"))
       (user "criticalgrind")
       (group "users")
       (permissions #o400)
       (path "/run/secrets/outline-api-key")))
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
        (type "ext4"))
      (xdg-runtime-tmpfs "peteches" 1000)
      (xdg-runtime-tmpfs "criticalgrind" 1001)
      (xdg-runtime-tmpfs "ygo" 1002))
     #:extra-services
     (list
      (service guix-home-service-type
       `(("peteches" ,claude-workstation-peteches-home)
         ("criticalgrind" ,claude-workstation-criticalgrind-home)
         ("ygo" ,claude-workstation-ygo-home)))
      ;; Authorize the same admin keys (nug + nyarlothotep) for the
      ;; criticalgrind and ygo users, so `ssh criticalgrind@…' / `ssh ygo@…'
      ;; work key-only just like the peteches account.  openssh-service-type
      ;; coalesces this with the peteches entry vm-base already sets.
      (simple-service 'criticalgrind-authorized-keys
                      openssh-service-type
                      `(("criticalgrind" ,@%vm-peteches-authorized-keys)))
      (simple-service 'ygo-authorized-keys
                      openssh-service-type
                      `(("ygo" ,@%vm-peteches-authorized-keys)))
      ;; Claude reaches Anthropic outbound; the base firewall permits
      ;; established/related + output and opens ssh inbound.  Tailscale needs
      ;; no inbound rule here: ssh over the tailnet still lands on tcp/22
      ;; (already open) and tailscaled's own traffic is outbound.
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (auth-key-file "/run/secrets/tailscale-auth-key"))))
      ;; Split-tunnel WireGuard behind a local SOCKS5 proxy (127.0.0.1:1080).
      ;; The system default route is untouched -- only processes that dial
      ;; the SOCKS5 proxy get routed over the tunnel. See
      ;; (peteches services wireguard-socks5).
      (service wireguard-socks5-service-type
               (wireguard-socks5-configuration
                (config-file "/run/secrets/wg0.conf")))
      (service alloy-service-type
               (alloy-configuration
                (hostname "claude-workstation.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy"))))))))))

claude-workstation-os
