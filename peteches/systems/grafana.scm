;; grafana.scm — Grafana dashboard server on a Proxmox QEMU/KVM VM.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.

(define-module (peteches systems grafana)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches system-services alloy)
  #:use-module (peteches system-services grafana)
  #:use-module (peteches system-services restic)
  #:use-module (peteches system-services tailscale)
  #:use-module (sops secrets)
  #:export (grafana-os))

(define-public grafana-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "grafana.peteches.co.uk"
     #:ipv4-address "192.168.51.188/23"
     #:ipv6-address "2a10:d582:ef59::101/64"
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
      (vm-name "grafana")
      (synology-host "nas.peteches.co.uk")
      (schedule "20 2 * * *")
      (backup-paths '("/var/lib/grafana"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/grafana/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/grafana/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service alloy-service-type
               (alloy-configuration
                (hostname "grafana.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/grafana/grafana.log" "grafana")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")))))
      (service grafana-service-type
               (grafana-configuration
                (admin-password "CHANGEME")
                (datasources
                 (list
                  (grafana-datasource
                   (name "Prometheus")
                   (type "prometheus")
                   (uid "prometheus")
                   (url "http://192.168.51.187:9090")
                   (is-default? #t))
                  (grafana-datasource
                   (name "Loki")
                   (type "loki")
                   (uid "loki")
                   (url "http://192.168.51.190:3100"))))
                (dashboards
                 (list
		  (grafana-dashboard
		   (name "synology-overview")
		   (json-file (local-file
			       "../grafana-dashboards/synology-overview.json")))
		  (grafana-dashboard
		   (name "synology-details")
		   (json-file (local-file
			       "../grafana-dashboards/synology-details.json")))
                  (grafana-dashboard
                   (name "node-exporter")
                   (json-file (local-file
                               "../grafana-dashboards/node-exporter.json")))
                  (grafana-dashboard
                   (name "pihole")
                   (json-file (local-file
                               "../grafana-dashboards/pihole.json")))
		  (grafana-dashboard
		   (name "proxmox")
		   (json-file (local-file
			       "../grafana-dashboards/proxmox.json")))))))
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")))))))))

grafana-os
