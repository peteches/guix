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
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services docker)
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
   (supplementary-groups '("wheel" "netdev" "docker"))
   (password "$6$yk5pnJr/ECPPOvGv$/HoWZNE7fWDslHHIVHAcaxk0AyhnthoHGhs3RrXaXqvVL8W5UI9OUVHndx4RfSqnWnnPw/.q2KhkfrPRKkw.11")))

;; Guix's docker-service-type storage driver defaults to overlay2, which
;; needs kernel overlayfs support this VM's linux-libre kernel lacks (see
;; vm-base.scm's #:kernel note — the Podman hosts plane.scm/
;; critical-grind-outline.scm hit the same wall and pin vfs for the same
;; reason). vfs is slower but is the only driver that actually works here.
(define %docker-daemon-config
  (plain-file "docker-daemon.json" "{\"storage-driver\": \"vfs\"}\n"))

;; Docker/Podman-style container networking on this VM.
(define %docker-firewall-rules
  (nftables-rules
   ;; Docker always names its default bridge "docker0" (fixed, unlike
   ;; Podman's netavark-assigned br-* names — see plane.scm's comment on why
   ;; that one matches by subnet instead).
   (forward
    (list
     "iifname \"docker0\" oifname \"eth0\" accept comment \"docker containers to lan/internet\""
     "iifname \"eth0\" oifname \"docker0\" ct state established,related accept comment \"lan/internet return to docker containers\""))
   (nat-postrouting
    (list
     "ip saddr 172.17.0.0/16 oifname \"eth0\" masquerade comment \"nat docker containers to lan/internet\""))))

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
     #:with-docker? #t
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
      ;; cgroup2 mount required by containerd/docker for container management
      ;; (not part of %base-file-systems — see plane.scm/
      ;; critical-grind-outline.scm for the same mount, needed there by Podman).
      (file-system
        (mount-point "/sys/fs/cgroup")
        (device "cgroup2")
        (type "cgroup2")
        (flags '(no-exec no-suid no-dev))
        (check? #f)
        (create-mount-point? #t)))
     #:extra-services
     (list
      (service guix-home-service-type
       `(("peteches" ,claude-workstation-peteches-home)
         ("criticalgrind" ,claude-workstation-criticalgrind-home)))
      ;; Guix's docker-service-type hard-codes 'dbus-system and 'elogind in
      ;; its shepherd requirement list. This VM base runs neither (headless,
      ;; no elogind/PAM session management — see vm-base.scm and CLAUDE.md's
      ;; "Adding a New VM" note), so stub both out as one-shot no-ops. Same
      ;; technique plane.scm uses to satisfy oci-service-type's Podman
      ;; requirements without a real elogind/dbus-system running.
      (simple-service 'docker-requirement-stubs
                      shepherd-root-service-type
                      (list
                       (shepherd-service
                        (provision '(dbus-system))
                        (one-shot? #t)
                        (start #~(lambda _ #t))
                        (documentation "Stub."))
                       (shepherd-service
                        (provision '(elogind))
                        (one-shot? #t)
                        (start #~(lambda _ #t))
                        (documentation "Stub."))))
      (service containerd-service-type)
      ;; enable-iptables? is off deliberately: dockerd's own iptables-nft
      ;; rule insertion would be wiped out on every `nft -f' the peteches
      ;; firewall does (its shepherd service flushes the whole ruleset on
      ;; every boot AND every reconfigure — see (peteches services
      ;; firewall)). Egress NAT for docker0 is handled explicitly below
      ;; instead, the same way concourse-worker01.scm owns NAT for its own
      ;; container bridge rather than relying on the runtime's iptables
      ;; integration. Consequence: `docker run -p' host port publishing
      ;; needs its own nftables DNAT rule added here if/when it's used —
      ;; it is NOT wired up by this change.
      (service docker-service-type
               (docker-configuration
                (enable-iptables? #f)
                (config-file %docker-daemon-config)))
      (simple-service 'docker-firewall
                      firewall-service-type
                      %docker-firewall-rules)
      ;; Authorize the same admin keys (nug + nyarlothotep) for the
      ;; criticalgrind user, so `ssh criticalgrind@…' works key-only just
      ;; like the peteches account.  openssh-service-type coalesces this with
      ;; the peteches entry vm-base already sets.
      (simple-service 'criticalgrind-authorized-keys
                      openssh-service-type
                      `(("criticalgrind" ,@%vm-peteches-authorized-keys)))
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
