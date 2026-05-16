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
  #:use-module (peteches system-services prometheus)
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
     #:extra-services
     (list
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
                   (job-name "pihole")
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.51.189:9617"))
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
                      (replacement "192.168.51.1:9221"))))))))))))))

prometheus-os
