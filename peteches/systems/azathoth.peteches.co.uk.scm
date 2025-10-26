(define-module (peteches systems azathoth.peteches.co.uk)
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

(define %mapped-devices
   (list
    (mapped-device
     (source "guix")
     (targets (list "guix-root"))
     (type lvm-device-mapping))
    (mapped-device
     (source "/dev/mapper/guix-root")
     (target  "root")
     (type luks-device-mapping))))

(operating-system
 (inherit
  (make-base-os
   #:host-name "azathoth.peteches.co.uk"
   #:kernel linux
   #:firmware (list linux-firmware)
   #:extra-packages (list glibc-locales)

   ;; Flags
   #:laptop? #t
   #:intel-cpu? #t
   #:with-bluetooth? #t
   #:with-printing? #f
   #:with-nonguix? #t
   #:with-docker? #t
   #:with-nvidia? #t
   #:bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))
    (keyboard-layout (keyboard-layout "us")))


   #:mapped-devices
    %mapped-devices
   #:file-systems
   (list

    scoreplay-cifs-mount
    (file-system
     (mount-point "/boot/efi")
     (device (uuid "C4D7-28C3"
                   'fat32))
     (type "vfat"))
    (file-system
     (mount-point "/")
     (device "/dev/mapper/root")
     (dependencies %mapped-devices)
     (type "ext4"))))))
