;; peteches/systems/caddy.scm — Caddy HTTPS reverse proxy on a Proxmox QEMU/KVM VM.
;; Terminates TLS for prometheus, grafana, git, jellyfin, and pihole using
;; deSEC DNS-01 ACME challenges.  EFI bootloader, virtio root on vda2 with
;; ESP on vda1, static networking.

(define-module (peteches systems caddy)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches system-services alloy)
  #:use-module (peteches system-services caddy)
  #:use-module (peteches system-services restic)
  #:use-module (peteches system-services tailscale)
  #:use-module (sops secrets)
  #:export (caddy-os))

(define-public caddy-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "caddy.peteches.co.uk"
     #:ipv4-address "192.168.51.193/23"
     #:ipv6-address "2a10:d582:ef59::106/64"
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
      (vm-name "caddy")
      (synology-host "nas.peteches.co.uk")
      (schedule "50 2 * * *")
      (backup-paths '("/var/lib/caddy"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("token"))
       (file (local-file "../../secrets/hosts/caddy/desec.yaml"))
       (path "/run/secrets/desec-token")
       (permissions #o400))
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/caddy/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/caddy/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (magic-dns-suffix "spaniel-cordylus.ts.net")
                      (forward-ports '((22 . 22)))
                      (host-forward-ports '((80 . 80) (443 . 443))))))
      (service caddy-service-type
               (caddy-configuration
                (netns "ts-peteches")
                (email "desec@peteches.co.uk")
                (tls-subjects '("*.ts.peteches.co.uk"))
                (virtual-hosts
                 (list
                  (caddy-reverse-proxy
                   (domain "prometheus.ts.peteches.co.uk")
                   (upstream "prometheus.spaniel-cordylus.ts.net:9090"))
                  (caddy-reverse-proxy
                   (domain "grafana.ts.peteches.co.uk")
                   (upstream "grafana.spaniel-cordylus.ts.net:3000"))
                  (caddy-reverse-proxy
                   (domain "git.ts.peteches.co.uk")
                   (upstream "git.spaniel-cordylus.ts.net:80"))
                  (caddy-reverse-proxy
                   (domain "jellyfin.ts.peteches.co.uk")
                   (upstream "jellyfin.spaniel-cordylus.ts.net:8096"))
                  (caddy-reverse-proxy
                   (domain "pihole.ts.peteches.co.uk")
                   (upstream "pihole.spaniel-cordylus.ts.net:80"))))))
      (service alloy-service-type
               (alloy-configuration
                (hostname "caddy.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")
                                 (cons "/var/log/caddy.log" "caddy"))))))))))

caddy-os
