;; nug.scm — nug.peteches.co.uk using (peteches systems base)
;;
;; Notes:
;; - Hyprland greeter via gtkgreet (from base.scm).
;; - Intel CPU microcode + NVIDIA firmware toggled on via flags.
;; - If you want root-on-LUKS, uncomment the LUKS section and comment the non-LUKS one.
;; - make-base-os should append %base-file-systems internally.

(define-module (peteches systems nug)
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
  #:use-module (peteches system-services firewall)
  #:use-module (peteches systems network-mounts)
  #:use-module (gnu packages admin))

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


(make-base-os
 #:host-name "nug"
 #:kernel linux
 ;; Base will append %base-file-systems — you only give machine-specific mounts here.
 #:firmware (list linux-firmware)
 #:users-extra (list (user-account
		     (name "guix-offload")
		     (comment "Build offload user")
		     (group "users")
		     (system? #t)
		     (home-directory "/var/empty")))
 #:mapped-devices
 (list
  (mapped-device
   (source (uuid "820e9368-484a-4bc0-af58-f3f0c29fe0fa"))
   (target "cryptroot")
   (type luks-device-mapping))
  (mapped-device
   (source (uuid "3d049249-8d28-45a3-bd06-980429edf7b7"))
   (target "ColdStorage")
   (type luks-device-mapping)
   (arguments '(#:key-file "/etc/keys/cold-storage.key")))
  (mapped-device
   (source (uuid "8e15e4a6-a1ac-4638-b9d2-814257363cab"))
   (target "HotStorage")
   (type luks-device-mapping)
   (arguments '(#:key-file "/etc/keys/hot-storage.key"))))

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
  (file-system
   (mount-point "/media/ColdStorage")
   (device (uuid "0cd3cea0-2ffc-4bf7-9cd0-91b9bbfa716b" 'ext4))
   (create-mount-point? #t)
   (type "ext4"))
  (file-system
   (mount-point "/media/HotStorage")
   (device (uuid "0b30a1c2-64d8-47ec-bfeb-d6ec47292886" 'ext4))
   (create-mount-point? #t)
   (type "ext4"))
  scoreplay-cifs-mount)

 ;; Bootloader (UEFI)
 #:bootloader
 (bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (keyboard-layout (keyboard-layout "us")))

 ;; Host-specific packages (optional)
 #:extra-packages
 '()

 ;; Feature flags
 #:laptop? #f	       ; nug is a desktop
 #:intel-cpu? #t     ; used for thermald when laptop?; harmless here
 #:with-printing? #f
 #:with-bluetooth? #t
 #:with-nonguix? #t
 #:with-nvidia? #t
 #:with-docker? #t
 #:offload-builds? #f

 ;; Host-only services
 #:extra-services
 (list
  (simple-service 'nug-guix-publish-firewall
                  firewall-service-type
                  (nftables-rules
                   (input (list "tcp dport 3000 accept comment \"guix-publish\""))))
  (service guix-publish-service-type
           (guix-publish-configuration
            (host "::")
            (port 3000)
            (compression '(("zstd" 9)))
            (advertise? #t)
            (cache "/var/cache/guix/publish")))
  (simple-service 'guix-offload-authorized-keys
                  openssh-service-type
                  `(("guix-offload"
                     ,(plain-file "arr-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJCmWvNZoNQhpVpeYU6VXtYcrtS8XfgrK5S5WCs5OtM1 guix-offload@arr\n")
                     ,(plain-file "nyarlothotep.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEP+/uHdoUNfL+LuniZGTEwPJkxvSgDpuR58yxfw/u74 guix-build@nyarlothotep\n")
                     ,(plain-file "caddy-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ5OyvcOFlI3lnunv9FzkOms2CO9i7y12EnSSBDmp6ob guix-offload@caddy\n")
                     ,(plain-file "downloads-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOTVFFGPzZsX0hV4fY2bhptvW1Zs6lilcYMGTOli1UoL guix-offload@downloads\n")
                     ,(plain-file "git-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDP/krMF4ECdoMVIqv9K5mZHvbJUv7+ZFSx2FlVlHSOf guix-offload@git\n")
                     ,(plain-file "grafana-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINw3+hJHHSzwhGquTWRtXx5+uVdvarpu3gJGCnXrj61Q guix-offload@grafana\n")
                     ,(plain-file "jellyfin-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILGGBMFH7Bei/lTu2s4xqFveXOmxOdqHGQiVmnDRfBt5 guix-offload@jellyfin\n")
                     ,(plain-file "loki-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN9R8rSJi1VRa2okQhXFxxBHJXwmV1rVOl8HelpepFVg guix-offload@loki\n")
                     ,(plain-file "pihole-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMNAir7xhAl7Z50tloQKOfCeVPqTqDgmIuSVxtfFdLES guix-offload@pihole\n")
                     ,(plain-file "prometheus-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM+nhIk+ySFYMj7I4SDwA/LKyM8MH3+8NMIabyAIuMSC guix-offload@prometheus\n")
                     ,(plain-file "prowlarr-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPxWQLDvXJGgp4HsOpSEMyLTGi0lL2zYcvRvARuVv/nU guix-offload@prowlarr\n")
                     ,(plain-file "rustdesk-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII0DTgAJjaG1+0STwTBDRfUrbP/q0KFVnY5OdjrqKasS guix-offload@rustdesk\n"))))))
