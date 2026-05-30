;; arr.scm — Sonarr (TV) + Radarr (movies) on a Proxmox QEMU/KVM VM.
;; Sonarr on port 8989, Radarr on port 7878, both via Tailscale.
;;
;; Bootstrap note: the CIFS mount and SOPS secrets require the VM's age key
;; to be enrolled first.  See the deployment plan for the two-pass sequence.

(define-module (peteches systems arr)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches system-services alloy)
  #:use-module (peteches system-services media-accounts)
  #:use-module (peteches system-services sonarr)
  #:use-module (peteches system-services radarr)
  #:use-module (peteches system-services tailscale)
  #:use-module (sops secrets)
  #:export (arr-os))

(define-public arr-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "arr.peteches.co.uk"
     #:ipv4-address "192.168.51.195/23"
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
        (type "ext4"))
      (file-system
        (device "//nas.peteches.co.uk/media")
        (mount-point "/media")
        (type "cifs")
        (mount? #t)
        (check? #f)
        (mount-may-fail? #f)
        (create-mount-point? #t)
        (shepherd-requirements '(sops-secrets networking))
        (options "credentials=/run/secrets/media-smb-credentials,file_mode=0664,dir_mode=0775,vers=3.1.1,cache=loose,actimeo=1800,rsize=1048576,wsize=65536,serverino,iocharset=utf8,noperm,nobrl")))
     #:sops-secrets
     (list
      (sops-secret
       (key '("credentials"))
       (file (local-file "../../secrets/groups/media/smb.yaml"))
       (path "/run/secrets/media-smb-credentials")
       (permissions #o400))
      (sops-secret
       (key '("auth-key"))
       (file (local-file "../../secrets/shared/tailscale.yaml"))
       (path "/run/secrets/tailscale-auth-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service media-accounts-service-type)
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (auth-key-file "/run/secrets/tailscale-auth-key")
                      (forward-ports '((22 . 22) (8989 . 8989) (7878 . 7878))))))
      (service sonarr-service-type
               (sonarr-configuration))
      (service radarr-service-type
               (radarr-configuration))
      (service alloy-service-type
               (alloy-configuration
                (hostname "arr.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")
                                 (cons "/var/log/sonarr.log" "sonarr")
                                 (cons "/var/log/radarr.log" "radarr"))))))))))

arr-os
