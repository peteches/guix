;; prometheus.scm — Prometheus monitoring server on a Proxmox QEMU/KVM VM.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.

(define-module (peteches systems prometheus)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches systems monitored-hosts)
  #:use-module (peteches system-services alloy)
  #:use-module (peteches system-services prometheus)
  #:use-module (peteches system-services restic)
  #:use-module (peteches system-services tailscale)
  #:export (prometheus-os))

(define-public prometheus-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "prometheus.peteches.co.uk"
     #:ipv4-address "192.168.51.187/23"
     #:ipv6-address "2a10:d582:ef59::100/64"
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
      (vm-name "prometheus")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/prometheus")))
     #:extra-services
     (list
      (service alloy-service-type
               (alloy-configuration
                (hostname "prometheus.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/prometheus.log" "prometheus")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")))))
      (service prometheus-service-type
               (prometheus-configuration
                (scrape-configs
                 (list
                  (prometheus-scrape-config
                   (job-name "prometheus")
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("localhost:9090"))))))
                  (prometheus-scrape-config
                   (job-name "node")
                   (static-configs
                    (map (lambda (host)
                           (prometheus-static-config
                            (targets (list (cdr host)))
                            (labels `(("instance" . ,(car host))))))
                         %monitored-hosts)))
                  (prometheus-scrape-config
                   (job-name "grafana")
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.51.188:3000"))
                           (labels '(("instance" . "grafana")))))))
                  (prometheus-scrape-config
                   (job-name "pihole")
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.51.189:9617"))
                           (labels '(("instance" . "pihole")))))))
                  (prometheus-scrape-config
                   (job-name "loki")
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.51.190:3100"))
                           (labels '(("instance" . "loki")))))))
                  (prometheus-scrape-config
                   (job-name "alloy")
                   (static-configs
                    (list
                     (prometheus-static-config
                      (targets '("192.168.51.187:12345"))
                      (labels '(("instance" . "prometheus"))))
                     (prometheus-static-config
                      (targets '("192.168.51.188:12345"))
                      (labels '(("instance" . "grafana"))))
                     (prometheus-static-config
                      (targets '("192.168.51.190:12345"))
                      (labels '(("instance" . "loki"))))
                     (prometheus-static-config
                      (targets '("192.168.51.189:12345"))
                      (labels '(("instance" . "pihole")))))))
                  (prometheus-scrape-config
                   (job-name "proxmox")
                   (metrics-path "/pve")
                   (params '(("module" . ("default"))))
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.51.1")))))
                   (relabel-configs
                    (list
                     (prometheus-relabel-config
                      (source-labels '("__address__"))
                      (target-label "__param_target"))
                     (prometheus-relabel-config
                      (source-labels '("__param_target"))
                      (target-label "instance"))
                     (prometheus-relabel-config
                      (target-label "__address__")
                      (replacement "192.168.51.1:9221")))))))))
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (forward-ports '((22 . 22) (9090 . 9090)))))))))))

prometheus-os
