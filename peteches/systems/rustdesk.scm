;; rustdesk.scm — RustDesk relay/rendezvous server.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.

(define-module (peteches systems rustdesk)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches system-services alloy)
  #:use-module (peteches system-services restic)
  #:use-module (peteches system-services tailscale)
  #:use-module (peteches system-services rustdesk)
  #:use-module (peteches system-services firewall)
  #:use-module (sops secrets)
  #:export (rustdesk-os))

(define-public rustdesk-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "rustdesk.peteches.co.uk"
     #:ipv4-address "192.168.51.197/23"
     #:ipv6-address "2a10:d582:ef59::107/64"
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
      (vm-name "rustdesk")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/rustdesk"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("restic-password"))
       (file (local-file "../../secrets/hosts/rustdesk/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/rustdesk/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (forward-ports '((22 . 22)
                                       (21115 . 21115)
                                       (21116 . 21116)
                                       (21117 . 21117)
                                       (21118 . 21118)
                                       (21119 . 21119)))
                      (udp-forward-ports '((21116 . 21116))))))
      (service rustdesk-service-type
               (rustdesk-configuration
                (relay-servers (list "100.112.48.78:21117"))))
      (simple-service 'rustdesk-firewall
                      firewall-service-type
                      (nftables-rules
                       (input
                        (list
                         "tcp dport { 21114, 21115, 21116, 21117, 21118, 21119 } accept comment \"rustdesk\""
                         "udp dport 21116 accept comment \"rustdesk udp\""))))
      (service alloy-service-type
               (alloy-configuration
                (hostname "rustdesk.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")
                                 (cons "/var/log/rustdesk/hbbs.log" "rustdesk-hbbs")
                                 (cons "/var/log/rustdesk/hbbr.log" "rustdesk-hbbr"))))))))))

rustdesk-os
