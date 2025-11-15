
(define-module (peteches systems nyarlothotep.peteches.co.uk)
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

(define mapped-devices
      (list
      (mapped-device
        (source  (uuid "57aa62d0-75b0-4721-9715-84b04c85b575"))
        (target "cryptroot")
        (type luks-device-mapping))
      (mapped-device
       (source "guix")
       (targets (list "guix-swap" "guix-root"))
       (type lvm-device-mapping))))

(operating-system
  (inherit
   (make-base-os
    #:host-name "nyarlothotep.peteches.co.uk"
    #:kernel linux
    #:firmware (list linux-firmware)
    #:bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout (keyboard-layout "us")))

    ;; LUKS mapping (use the *container* UUID)
    #:mapped-devices
    mapped-devices

    #:extra-services
    (list
     (service tailscale-service-type))
    
    ;; Filesystems (root via mapper; EFI unencrypted)
    #:file-systems
    (append
     (list
      (file-system
        (mount-point "/")
        (device "/dev/mapper/guix-root")
        (type "ext4")
	(dependencies mapped-devices))
      (file-system
       (mount-point "/boot/efi")
        (device (uuid "4763-DCEB" 'fat32))
        (type "vfat")
        (flags '())))
     (list scoreplay-cifs-mount))

    ;; Host-specific packages
    #:extra-packages (list glibc-locales)

    ;; Flags
    #:laptop? #t
    #:intel-cpu? #f
    #:with-bluetooth? #t
    #:with-printing? #f
    #:with-nonguix? #t
    #:with-docker? #t
    #:with-nvidia? #f))
  (swap-devices
   (list (swap-space
	  (target "/dev/mapper/guix-swap")
	  (dependencies mapped-devices)))))
