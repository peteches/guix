;; prometheus.scm — Prometheus monitoring server on a Proxmox QEMU/KVM VM.
;; BIOS bootloader, single virtio root partition, DHCP networking.

(define-module (peteches systems prometheus)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches system-services prometheus)
  #:export (prometheus-os))

(define-public prometheus-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "prometheus.peteches.co.uk"
     #:bootloader
     (bootloader-configuration
      (bootloader grub-bootloader)
      (targets '("/dev/vda"))
      (keyboard-layout (keyboard-layout "us")))
     #:file-systems
     (list
      (file-system
        (mount-point "/")
        (device "/dev/vda1")
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
                   (job-name "proxmox")
                   (metrics-path "/pve")
                   (params '(("module" . ("default"))))
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.50.220")))))
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
                      (replacement "192.168.50.220:9221"))))))))))))))

prometheus-os
