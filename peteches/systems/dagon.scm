;; dagon.scm — dagon.peteches.co.uk, new workstation (similar to nug).
;;
;;   sudo guix system -L . reconfigure peteches/systems/dagon.scm
;;
;; Reconfigured locally, not deployed — desktops are absent from
;; (peteches machines) by design.  Built on make-base-os from
;; (peteches systems base); see that module for what each flag does.
;;
;; Host specifics:
;;   - Intel CPU (#:intel-cpu? #t, the default) and an NVIDIA GPU
;;     (#:with-nvidia? #t), like nug.
;;   - Disk layout is simpler than nug/nyarlothotep: just an EFI System
;;     Partition and a single LUKS container holding the ext4 root
;;     directly — no separate /boot partition, no LVM.  GRUB's
;;     cryptodisk support is enabled automatically because
;;     `mapped-devices' includes a luks-device-mapping.
;;   - #:offload-builds? is #f for this initial install.  Wiring up
;;     offload to nug needs a guix-offload SSH keypair delivered via
;;     SOPS (secrets/hosts/dagon/guix-build.yaml), which in turn needs
;;     dagon's age public key — only available after first boot.  Once
;;     that exists, follow nyarlothotep.scm's pattern
;;     (sops-key-generator-service-type + sops-secrets-service-type),
;;     flip this to #t, and add dagon's offload pubkey to nug.scm's
;;     guix-offload-authorized-keys.
;;
;; The file evaluates to a bare `operating-system' record as its last
;; expression, which is what `guix system' consumes.

(define-module (peteches systems dagon)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages base)           ; glibc-locales
  #:use-module (nongnu packages linux)
  #:use-module (peteches systems base)
  #:use-module (peteches systems network-mounts))

(use-service-modules base linux cups desktop networking ssh xorg)

(define mapped-devices
  (list
   (mapped-device
    (source (uuid "cf8737a4-d6c9-44aa-88e1-14758a845d5f"))
    (target "cryptroot")
    (type luks-device-mapping))))

(make-base-os
 #:host-name "dagon"
 #:kernel linux
 #:firmware (list linux-firmware)

 #:bootloader
 (bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (keyboard-layout (keyboard-layout "us")))

 #:mapped-devices
 mapped-devices

 #:file-systems
 (list
  (file-system
   (mount-point "/")
   (device "/dev/mapper/cryptroot")
   (type "ext4")
   (dependencies mapped-devices))
  (file-system
   (mount-point "/boot/efi")
   (device (uuid "CC71-F02D" 'fat32))
   (type "vfat"))
  scoreplay-cifs-mount)

 #:extra-packages (list glibc-locales)

 ;; Feature flags
 #:laptop? #f
 #:intel-cpu? #t
 #:with-bluetooth? #t
 #:with-printing? #f
 #:with-nonguix? #t
 #:with-docker? #t
 #:with-nvidia? #t
 #:offload-builds? #f)
