;; loki.scm — Grafana Loki log aggregation server on a Proxmox QEMU/KVM VM.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.

(define-module (peteches systems loki)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches system-services alloy)
  #:use-module (peteches system-services loki)
  #:use-module (peteches system-services restic)
  #:use-module (peteches system-services tailscale)
  #:use-module (sops secrets)
  #:export (loki-os))

(define-public loki-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "loki.peteches.co.uk"
     #:ipv4-address "192.168.51.190/23"
     #:ipv6-address "2a10:d582:ef59::102/64"
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
      (vm-name "loki")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/loki"))
      (password-file "/run/secrets/restic-password"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("restic-password"))
       (file (local-file "../../secrets/hosts/loki/restic.yaml"))
       (path "/run/secrets/restic-password")))
     #:extra-services
     (list
      (service alloy-service-type
               (alloy-configuration
                (hostname "loki.peteches.co.uk")
                (syslog-listen-port 514)
                (syslog-format "rfc3164")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/loki.log" "loki")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")))))
      (service loki-service-type (loki-configuration))
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (forward-ports '((22 . 22) (3100 . 3100)))))))))))

loki-os
