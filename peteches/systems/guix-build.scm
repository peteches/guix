;; guix-build.scm — Guix substitute server + build-offload target, on a
;; Proxmox QEMU/KVM VM.
;;
;; Successor to nug's guix-publish/offload role — nug itself is being
;; reinstalled as the Proxmox host (proxmox3), so this can no longer live
;; there. Every VM's `with-nug-offload?` (default #t in vm-base.scm) needs
;; %nug-build-machine in (peteches systems common) repointed at this VM's
;; hostname/host-key, and the desktops' `without-gdm` substitute-server URL
;; (currently hardcoded nug.peteches.co.uk:3000) needs the same treatment —
;; see CLAUDE.md's "Channels"/"Adding a New VM" notes for where else that
;; ripples.
;;
;; The guix-publish config, the coordinator-signing-key trust, and the
;; per-VM guix-offload authorized-keys list below are copied verbatim from
;; nug.scm's working setup. Nothing here is new configuration — it's the
;; same service, moved.

(define-module (peteches systems guix-build)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services restic)
  #:use-module (peteches services tailscale)
  #:use-module (sops secrets)
  #:export (guix-build-os))

(define-public guix-build-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "guix-build.peteches.co.uk"
     #:ipv4-address "192.168.51.207/23"
     #:ipv6-address "2a10:d582:ef59::113/64"
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
     #:users-extra
     (list (user-account
            (name "guix-offload")
            (comment "Build offload user")
            (group "users")
            (system? #t)
            (home-directory "/var/empty")))
     ;; restic-config deliberately omitted for now: it needs the fleet's
     ;; shared restic SSH key (the one nas.peteches.co.uk already trusts),
     ;; not a freshly generated one -- reusing the real credential requires
     ;; verifying it against an existing host's secret first rather than
     ;; guessing. Revisit once this VM exists and that can be confirmed.
     #:sops-secrets '()
     ;; This VM IS the offload target now, not a client of one.
     #:with-nug-offload? #f
     #:extra-services
     (list
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches"))))
      (simple-service 'guix-publish-firewall
                      firewall-service-type
                      (nftables-rules
                       (input (list "tcp dport 3000 accept comment \"guix-publish\""))))
      (service guix-publish-service-type
               (guix-publish-configuration
                (host "::")
                (port 3000)
                (compression '(("zstd" 9)))
                ;; #f, not nug.scm's #t: advertise? needs avahi-daemon for
                ;; mDNS, which nug had as a desktop service but this headless
                ;; VM (vm-base.scm) does not run. Nothing in this fleet
                ;; discovers the substitute server via mDNS anyway -- every
                ;; consumer hardcodes guix-build.spaniel-cordylus.ts.net:3000
                ;; (see common.scm's %authorize-coordinator-key).
                (advertise? #f)
                (cache "/var/cache/guix/publish")))
      ;; Trusts claude-workstation's Guix archive signing key so this VM's
      ;; daemon accepts store items it sends during offload -- separate from
      ;; the SSH guix-offload key below, which only grants login. Without
      ;; this, offload connects fine but every export fails with
      ;; "unauthorized public key" and silently falls back to a local build.
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
                         ,(plain-file "rustdesk-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII0DTgAJjaG1+0STwTBDRfUrbP/q0KFVnY5OdjrqKasS guix-offload@rustdesk\n")
                         ;; New since this list was on nug: comfyui itself
                         ;; offloads TO this VM (with-nug-offload? default #t
                         ;; in comfyui.scm), so its key needs to land here
                         ;; too once generated.
                         )))
      (service alloy-service-type
               (alloy-configuration
                (hostname "guix-build.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale"))))))))))

guix-build-os
