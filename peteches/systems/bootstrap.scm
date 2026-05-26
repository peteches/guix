;; bootstrap.scm — Minimal Proxmox VM template.
;;
;; Build a QCOW2 image to upload as a Proxmox template:
;;   guix system image -L . -t qcow2-gpt --image-size=20G peteches/systems/bootstrap.scm
;;
;; On first boot of each cloned VM the sops-key-generator shepherd service
;; generates a unique age keypair at /etc/age/keys.txt and /etc/age/keys.pub.
;; Subsequent reboots leave the existing keys untouched.
;;
;; Networking: DHCP (no static address — template has no fixed IP).
;; After cloning, manage the VM with guix deploy using a proper system config.

(define-module (peteches systems bootstrap)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches system-services sops-key-generator)
  #:export (bootstrap-os))

(define-public bootstrap-os
  (make-vm-os
   #:host-name "bootstrap"
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
   (list (service sops-key-generator-service-type))))

bootstrap-os
