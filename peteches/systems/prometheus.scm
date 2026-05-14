;; prometheus.scm — Prometheus monitoring server on a Proxmox QEMU/KVM VM.
;; BIOS bootloader, single virtio root partition, DHCP networking.

(define-module (peteches systems prometheus)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system shadow)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches packages prometheus)
  #:use-module (peteches system-services firewall)
  #:export (prometheus-os))

(define %prometheus-group
  (user-group
   (name "prometheus")
   (system? #t)))

(define %prometheus-account
  (user-account
   (name "prometheus")
   (group "prometheus")
   (system? #t)
   (comment "Prometheus monitoring daemon")
   (home-directory "/var/lib/prometheus")
   (shell (file-append shadow "/sbin/nologin"))))

(define %prometheus-config
  (plain-file "prometheus.yml"
              "\
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: \"prometheus\"
    static_configs:
      - targets:
          - \"localhost:9090\"
  - job_name: \"proxmox\"
    static_configs:
      - targets:
          - \"192.168.50.187:9221\"
"))

(define %prometheus-shepherd-service
  (shepherd-service
   (provision '(prometheus))
   (documentation "Prometheus monitoring server")
   (requirement '(networking file-systems))
   (start #~(make-forkexec-constructor
              (list #$(file-append prometheus "/bin/prometheus")
                    "--config.file=/etc/prometheus/prometheus.yml"
                    "--storage.tsdb.path=/var/lib/prometheus"
                    "--web.listen-address=0.0.0.0:9090")
              #:user "prometheus"
              #:group "prometheus"
              #:log-file "/var/log/prometheus.log"))
   (stop #~(make-kill-destructor))))

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
    #:extra-packages
    (list prometheus)
    #:extra-services
    (list
     ;; Register prometheus user and group
     (simple-service 'prometheus-account account-service-type
                     (list %prometheus-group %prometheus-account))
     ;; Create data directory owned by prometheus
     (simple-service 'prometheus-data-dir activation-service-type
                     #~(begin
                         (use-modules (guix build utils))
                         (mkdir-p "/var/lib/prometheus")
                         (let* ((pw  (getpwnam "prometheus"))
                                (uid (passwd:uid pw))
                                (gid (passwd:gid pw)))
                           (chown "/var/lib/prometheus" uid gid))))
     ;; Install /etc/prometheus/prometheus.yml
     (simple-service 'prometheus-config etc-service-type
                     (list `("prometheus/prometheus.yml" ,%prometheus-config)))
     ;; Shepherd service
     (simple-service 'prometheus-shepherd shepherd-root-service-type
                     (list %prometheus-shepherd-service))
     ;; Restart prometheus after reconfigure
     (simple-service 'prometheus-restart activation-service-type
                     #~(when (file-exists? "/run/shepherd/socket")
                         (system* #$(file-append shepherd "/bin/herd")
                                  "restart" "prometheus")))
     ;; Open port 9090 in the firewall
     (simple-service 'prometheus-firewall firewall-service-type
                     (nftables-rules
                      (input (list
                              "tcp dport 9090 accept comment \"prometheus\"")))))))))

prometheus-os
