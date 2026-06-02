;; downloads.scm — NZBGet (Usenet) + Transmission (torrents) on a Proxmox VM.
;; NZBGet on port 6789, Transmission on port 9091, both via Tailscale.
;;
;; Bootstrap note: the CIFS mount and SOPS secrets require the VM's age key
;; to be enrolled first.  See the deployment plan for the two-pass sequence.

(define-module (peteches systems downloads)
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
  #:use-module (peteches system-services nzbget)
  #:use-module (peteches system-services transmission)
  #:use-module (peteches system-services restic)
  #:use-module (peteches system-services tailscale)
  #:use-module (sops secrets)
  #:export (downloads-os))

(define-public downloads-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "downloads.peteches.co.uk"
     #:ipv4-address "192.168.51.196/23"
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
     #:restic-config
     (restic-vm-backup-configuration
      (vm-name "downloads")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/nzbget" "/var/lib/transmission"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
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
       (permissions #o400))
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/downloads/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/downloads/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service media-accounts-service-type)
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (auth-key-file "/run/secrets/tailscale-auth-key"))))
      (service nzbget-service-type
               (nzbget-configuration))
      (service transmission-service-type
               (transmission-configuration))
      (service alloy-service-type
               (alloy-configuration
                (hostname "downloads.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")
                                 (cons "/var/log/nzbget.log" "nzbget")
                                 (cons "/var/log/transmission.log" "transmission"))))))))))

downloads-os
