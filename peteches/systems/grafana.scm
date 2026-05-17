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
  #:use-module (peteches system-services grafana)
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
     #:extra-services
     (list
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
                   (is-default? #t))))
                (dashboards
                 (list
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
			       "../grafana-dashboards/proxmox.json"))))))))))))

grafana-os
