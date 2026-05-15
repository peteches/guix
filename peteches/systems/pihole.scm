;; pihole.scm — Pi-hole DNS ad-blocking server on a Proxmox QEMU/KVM VM.
;; BIOS bootloader, single virtio root partition, static networking.

(define-module (peteches systems pihole)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches system-services pihole)
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
      (bootloader grub-bootloader)
      (targets '("/dev/vda"))
      (keyboard-layout (keyboard-layout "us")))
     #:file-systems
     (list
      (file-system
        (mount-point "/")
        (device "/dev/vda1")
        (type "ext4")))
     #:extra-services
     (list
      (service pihole-service-type
               (pihole-configuration
                (interface "eth0")
                (dns-upstreams '("8.8.8.8" "8.8.4.4"))
                (with-unbound? #t))))))))

pihole-os
