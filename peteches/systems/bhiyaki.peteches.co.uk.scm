;; bhiyaki.scm — host using the base constructor (root on LUKS, EFI unencrypted)
(define-module (peteches systems bhiyaki.peteches.co.uk)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages base)           ; glibc-locales
  #:use-module (nongnu packages linux)
  #:use-module (peteches systems base)
  #:use-module (peteches system-services tailscale)
  #:use-module (peteches systems network-mounts))

(use-service-modules base linux cups desktop networking ssh xorg)

(operating-system
  (inherit
   (make-base-os
    #:host-name "bhiyaki.peteches.co.uk"
    #:kernel linux
    #:firmware (list linux-firmware)
    #:bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout (keyboard-layout "us")))

    ;; LUKS mapping (use the *container* UUID)
    #:mapped-devices
    (list
      (mapped-device
        (source  (uuid "d516a745-caf3-4094-a08b-ec60f3096c0a"))
        (target "cryptroot")
        (type luks-device-mapping)))

    #:extra-services
    (list
     (service tailscale-service-type))
    
    ;; Filesystems (root via mapper; EFI unencrypted)
    #:file-systems
    (append
     (list
      (file-system
        (mount-point "/")
        (device "/dev/mapper/cryptroot")
        (type "ext4"))
      (file-system
        (mount-point "/boot/efi")
        (device (uuid "B6CE-8818" 'fat32))
        (type "vfat")
        (flags '())))
     (list scoreplay-cifs-mount))

    ;; Host-specific packages
    #:extra-packages (list glibc-locales)

    ;; Flags
    #:laptop? #t
    #:intel-cpu? #t
    #:with-bluetooth? #t
    #:with-printing? #f
    #:with-nonguix? #t
    #:with-docker? #t
    #:with-nvidia? #f)))
