;; nug.scm — nug.peteches.co.uk using (peteches systems base)
;;
;; Notes:
;; - Hyprland greeter via gtkgreet (from base.scm).
;; - Intel CPU microcode + NVIDIA firmware toggled on via flags.
;; - If you want root-on-LUKS, uncomment the LUKS section and comment the non-LUKS one.
;; - make-base-os should append %base-file-systems internally.

(define-module (peteches systems nug.peteches.co.uk)
  #:use-module (nongnu packages linux)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services certbot)
  #:use-module (gnu packages base)           ; e.g. glibc-locales if you want it
  #:use-module (peteches systems base)
  #:use-module (peteches system-services tailscale)
  #:use-module (peteches systems network-mounts))

;; Bring service types into scope for any host-specific additions.
(use-service-modules base linux cups desktop networking ssh xorg)

(define %fix-perms-hook
  (with-imported-modules
      (source-module-closure '((ice-9 rdelim)
                               (guix build utils)))
    (program-file
     "generate-cert-key-pem-file"
     #~(begin
         (use-modules (ice-9 rdelim)
                      (guix build utils))

         (define cert "/etc/letsencrypt/live/nug.peteches.co.uk/fullchain.pem")
         (define key  "/etc/letsencrypt/live/nug.peteches.co.uk/privkey.pem")
         (define dst-dir "/home/peteches/.local/share/certs")
         (define dst (string-append dst-dir "/nug.peteches.co.uk.pem"))

         ;; absolute store paths, no PATH reliance
         (define chown #$(file-append coreutils "/bin/chown"))
         (define chmod #$(file-append coreutils "/bin/chmod"))
	 (define cat #$(file-append coreutils "/bin/cat"))

         (mkdir-p dst-dir)

	 ;; Concatenate by streaming, no get-string-all, no invoke keywords
	 (call-with-output-file dst
           (lambda (out)
             (call-with-input-file cert (lambda (in) (dump-port in out)))
             (newline out)
             (call-with-input-file key  (lambda (in) (dump-port in out)))))

         ;; owner only, no need to assume "users" group exists
         (invoke chown "-R" "peteches:" dst-dir)
         (invoke chmod "600" dst)))))


(operating-system
 (inherit
  (make-base-os
   #:host-name "nug.peteches.co.uk"
   #:kernel linux
   ;; Base will append %base-file-systems — you only give machine-specific mounts here.
   #:firmware (list linux-firmware)
   #:mapped-devices
   (list
    (mapped-device
     (source (uuid "820e9368-484a-4bc0-af58-f3f0c29fe0fa"))
     (target "cryptroot")
     (type luks-device-mapping)))

   #:file-systems
   (list
    (file-system
     (mount-point "/")
     (device "/dev/mapper/cryptroot")	; ← ext4 root
     (type "ext4"))
    (file-system
     (mount-point "/boot")
     (device (uuid "2c8fb9c4-f41d-4415-9540-86b588e91bac" 'ext4))
     (type "ext4"))
    (file-system
     (mount-point "/boot/efi")
     (device (uuid "7222-0EC9" 'fat32)) ; ← EFI system partition
     (type "vfat"))
    scoreplay-cifs-mount)

   ;; Bootloader (UEFI)
   #:bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))
    (keyboard-layout (keyboard-layout "us")))

   ;; Host-specific packages (optional)
   #:extra-packages
   '(
     ;; glibc-locales
     ;; waybar foot wofi  ; handy for Hyprland sessions
     )

   ;; Feature flags
   #:laptop? #f	       ; nug is a desktop
   #:intel-cpu? #t     ; used for thermald when laptop?; harmless here
   #:with-printing? #f
   #:with-bluetooth? #t
   #:with-nonguix? #t ; flip to #t if you add the Nonguix pubkey in base.scm
   #:with-nvidia? #t  ; adds nvidia-firmware (4090)
   #:with-docker? #t

   ;; Host-only services (examples). Add or remove as you need.
   #:extra-services
   (list
    (service guix-publish-service-type
             (guix-publish-configuration
	      (host "0.0.0.0")
              (port 3000)		; default
              (compression '(("zstd" 9)))
              (advertise? #t)))
    (service tailscale-service-type
            (list (tailscale-instance-configuration
              (name "thumbwar")
              (port 41642))))
    (service tailscale-service-type
            (list (tailscale-instance-configuration
              (name "scoreplay")
              (port 41643))))
    (service certbot-service-type
	     (certbot-configuration
              (email "certbot@peteches.co.uk")
              (certificates
               (list
                (certificate-configuration
                 (domains '("nug.peteches.co.uk"))
		 (deploy-hook %fix-perms-hook))))))))))
