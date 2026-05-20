;; pihole.scm — Pi-hole DNS ad-blocking server on a Proxmox QEMU/KVM VM.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.

(define-module (peteches systems pihole)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches system-services alloy)
  #:use-module (peteches system-services pihole)
  #:use-module (peteches system-services restic)
  #:export (pihole-os))

(define-public pihole-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "pihole.peteches.co.uk"
     #:ipv4-address "192.168.51.189/23"
     #:ipv6-address "2a10:d582:ef59::101/64"
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
      (vm-name "pihole")
      (synology-host "nas.peteches.co.uk")
      (schedule "40 2 * * *")
      (backup-paths '("/var/lib/pihole")))
     #:extra-services
     (list
      (service alloy-service-type
               (alloy-configuration
                (hostname "pihole.peteches.co.uk")
                (log-files '("/var/log/messages"
                             "/var/log/prometheus-node-exporter.log"
                             "/var/log/ntpd.log"
                             "/var/log/alloy.log"
                             "/var/log/pihole/FTL.log"
                             "/var/log/pihole/pihole.log"
                             "/var/log/pihole/webserver.log"
                             "/var/log/pihole/unbound.log"
                             "/var/log/pihole/exporter.log"))))
      (service pihole-service-type
               (pihole-configuration
                (interface "eth0")
                (dns-upstreams '()) ; using unbound
                (with-unbound? #t)
		(adlists '("https://raw.githubusercontent.com/r0xd4n3t/pihole-adblock-lists/main/pihole_adlists.txt"))
                (with-exporter? #t)
                (custom-hosts
                 (list
		  (pihole-custom-host (address "192.168.50.244")
				      (hostname "nas.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.1")
                                      (hostname "proxmox.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.187")
                                      (hostname "prometheus.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.188")
                                      (hostname "grafana.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.189")
                                      (hostname "pihole.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.190")
                                      (hostname "loki.peteches.co.uk")))))))))))

pihole-os
