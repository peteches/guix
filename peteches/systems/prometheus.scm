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
  #:use-module (sops secrets)
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
      (backup-paths '("/var/lib/prometheus"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/prometheus/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/prometheus/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400)))
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
                      (labels '(("instance" . "pihole"))))
                     (prometheus-static-config
                      (targets '("192.168.51.193:12345"))
                      (labels '(("instance" . "caddy"))))
                     (prometheus-static-config
                      (targets '("192.168.51.194:12345"))
                      (labels '(("instance" . "prowlarr"))))
                     (prometheus-static-config
                      (targets '("192.168.51.195:12345"))
                      (labels '(("instance" . "arr"))))
                     (prometheus-static-config
                      (targets '("192.168.51.196:12345"))
                      (labels '(("instance" . "downloads"))))
                     (prometheus-static-config
                      (targets '("192.168.51.197:12345"))
                      (labels '(("instance" . "rustdesk")))))))
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
                      (replacement "192.168.51.1:9221")))))
                  (prometheus-scrape-config
                   (job-name "prowlarr")
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.51.194:9696"))
                           (labels '(("instance" . "prowlarr")))))))
                  (prometheus-scrape-config
                   (job-name "sonarr")
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.51.195:8989"))
                           (labels '(("instance" . "sonarr")))))))
                  (prometheus-scrape-config
                   (job-name "radarr")
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.51.195:7878"))
                           (labels '(("instance" . "radarr")))))))
                  (prometheus-scrape-config
                   (job-name "nzbget")
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.51.196:6789"))
                           (labels '(("instance" . "nzbget")))))))
                  (prometheus-scrape-config
                   (job-name "transmission")
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("192.168.51.196:9091"))
                           (labels '(("instance" . "transmission")))))))
                  (prometheus-scrape-config
                   (job-name "snmp")
                   (metrics-path "/snmp")
                   (params '(("module" . ("synology" "ucd_la_table" "ucd_system_stats" "ucd_memory" "hrStorage" "hrSystem" "system" "if_mib")) ("auth" . ("synology"))))
                   (static-configs
                    (list (prometheus-static-config
                           (targets '("nas.peteches.co.uk")))))
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
                      (replacement "192.168.50.244:9116")))))))))
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")))))))))

prometheus-os
