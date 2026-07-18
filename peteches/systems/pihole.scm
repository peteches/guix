;; pihole.scm — Pi-hole DNS ad-blocking server on a Proxmox QEMU/KVM VM.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.
;;
;; This VM is %vm-nameservers in (peteches systems vm-base) — every other VM
;; resolves through it, so a broken deploy here breaks DNS fleet-wide.  That
;; is why deploys address hosts by IP.  Upstream resolution is unbound
;; (dns-upstreams is empty), with *.ts.net forwarded to Tailscale's
;; MagicDNS at 100.100.100.100.
;;
;; `custom-hosts' is the LAN's forward DNS.  Add an entry here for each new
;; VM (see CLAUDE.md "Adding a New VM"); it duplicates the IP list in
;; peteches/machines.scm and peteches/home/modules/ssh.scm, with
;; proxmox-vms.org as the authoritative inventory.
;;
;; IPv6 suffixes are assigned by hand and tracked nowhere but the
;; #:ipv6-address lines in peteches/systems/*.scm — proxmox-vms.org records
;; IPv4 only.  Before assigning one, check the current allocation:
;;
;;   command grep -rn '#:ipv6-address' peteches/systems/
;;
;; In use: ::1 gateway, ::100 prometheus, ::101 grafana, ::102 loki,
;; ::103 git, ::104 jellyfin, ::105 pihole, ::106 caddy, ::107 rustdesk,
;; ::108 vault, ::109 critical-grind-campaign, ::110 critical-grind-outline.
;; Free: ::111+.
;; (prowlarr, arr, downloads, concourse-*, plane are IPv4-only.)

(define-module (peteches systems pihole)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services pihole)
  #:use-module (peteches services restic)
  #:use-module (peteches services tailscale)
  #:use-module (sops secrets)
  #:export (pihole-os))

(define-public pihole-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "pihole.peteches.co.uk"
     #:ipv4-address "192.168.51.189/23"
     #:ipv6-address "2a10:d582:ef59::105/64"
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
     #:restic-config
     (restic-vm-backup-configuration
      (vm-name "pihole")
      (synology-host "nas.peteches.co.uk")
      (schedule "40 2 * * *")
      (backup-paths '("/var/lib/pihole"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/pihole/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/pihole/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400))
      (sops-secret
       (key '("ssh-private-key"))
       (file (local-file "../../secrets/hosts/pihole/guix-build.yaml"))
       (path "/run/secrets/guix-offload-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches"))))
      (service alloy-service-type
               (alloy-configuration
                (hostname "pihole.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
				 (cons "/var/log/pihole/unbound.log" "unbound")
                                 (cons "/var/log/pihole/FTL.log" "pihole")
                                 (cons "/var/log/pihole/pihole.log" "pihole")
                                 (cons "/var/log/pihole/webserver.log" "pihole")
                                 (cons "/var/log/pihole/exporter.log" "pihole")))))
      (service pihole-service-type
               (pihole-configuration
                (interface "eth0")
                (listening-mode "ALL")
                (dns-upstreams '()) ; using unbound
                (with-unbound? #t)
                (unbound
                 (pihole-unbound-configuration
                  (forward-zones
                   '(("spaniel-cordylus.ts.net." . "100.100.100.100")))))
		(adlists '("https://raw.githubusercontent.com/r0xd4n3t/pihole-adblock-lists/main/pihole_adlists.txt"))
                (with-exporter? #t)
                (custom-hosts
                 (list
		  (pihole-custom-host (address "192.168.50.244")
				      (hostname "nas.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.1")
                                      (hostname "proxmox1.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.187")
                                      (hostname "prometheus.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.188")
                                      (hostname "grafana.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.189")
                                      (hostname "pihole.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.190")
                                      (hostname "loki.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.191")
                                      (hostname "git.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.192")
                                      (hostname "jellyfin.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.193")
                                      (hostname "caddy.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.194")
                                      (hostname "prowlarr.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.195")
                                      (hostname "arr.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.196")
                                      (hostname "downloads.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.197")
                                      (hostname "rustdesk.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.198")
                                      (hostname "concourse-db.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.199")
                                      (hostname "concourse-web01.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.200")
                                      (hostname "concourse-worker01.peteches.co.uk"))
                  (pihole-custom-host (address "192.168.51.201")
                                      (hostname "vault.peteches.co.uk"))
		  (pihole-custom-host (address "192.168.51.202")
				      (hostname "critical-grind-campaign.peteches.co.uk"))
		  (pihole-custom-host (address "192.168.51.203")
				      (hostname "critical-grind-outline.peteches.co.uk"))
		  (pihole-custom-host (address "192.168.51.204")
				      (hostname "plane.peteches.co.uk")))))))))))

pihole-os
