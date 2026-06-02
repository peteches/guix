;; git.scm — Personal git server on a Proxmox QEMU/KVM VM.
;; gitolite for SSH access control, cgit for web UI (Tailscale only).
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.

(define-module (peteches systems git)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services cgit)
  #:use-module (gnu services version-control)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches system-services alloy)
  #:use-module (peteches system-services restic)
  #:use-module (peteches system-services tailscale)
  #:use-module (peteches system-services firewall)
  #:use-module (sops secrets)
  #:export (git-os))

(define-public git-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "git.peteches.co.uk"
     #:ipv4-address "192.168.51.191/23"
     #:ipv6-address "2a10:d582:ef59::103/64"
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
      (vm-name "git")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/gitolite"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/git/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/git/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service gitolite-service-type
               (gitolite-configuration
                (rc-file (gitolite-rc-file
                          (umask #o0022)))
                (admin-pubkey
                 (plain-file "admin.pub"
                             "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM28x2V8tgwfzjyhapMayamDFwviOTHfU4W9BMnmc70w peteches@nyarlothotep.peteches.co.uk"))
                (admin-name "peteches@nyarlothotep")))
      (service cgit-service-type
               (cgit-configuration
                (repository-directory "/var/lib/gitolite/repositories")))
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches"))))
      (service alloy-service-type
               (alloy-configuration
                (hostname "git.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")))))
      (simple-service 'cgit-firewall
                       firewall-service-type
                       (nftables-rules
                        (input (list "tcp dport 80 accept comment \"cgit\"")))))))))

git-os
