;; concourse-db.scm — PostgreSQL database VM for Concourse CI.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.
;; Allows the concourse user to connect from concourse-web01 (192.168.51.199).

(define-module (peteches systems concourse-db)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages databases)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services restic)
  #:use-module (peteches services tailscale)
  #:use-module (sops secrets)
  #:export (concourse-db-os))

(define-public concourse-db-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "concourse-db.peteches.co.uk"
     #:ipv4-address "192.168.51.198/23"
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
      (vm-name "concourse-db")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/postgresql"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/concourse-db/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/concourse-db/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400))
      (sops-secret
       (key '("ssh-private-key"))
       (file (local-file "../../secrets/hosts/concourse-db/guix-build.yaml"))
       (path "/run/secrets/guix-offload-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service postgresql-service-type
               (postgresql-configuration
                (postgresql postgresql-14)
                (config-file
                 (postgresql-config-file
                  (log-destination "stderr")
                  (hba-file
                   (plain-file "pg_hba.conf"
                               "\
local\tall\tall\t\t\ttrust
host\tconcourse\tconcourse\t192.168.51.199/32\ttrust\n"))
                  (extra-config
                   '(("listen_addresses" "*")
                     ("log_directory"    "/var/log/postgresql")))))))
      (service postgresql-role-service-type
               (postgresql-role-configuration
                (roles
                 (list (postgresql-role
                        (name "concourse")
                        (create-database? #t))))))
      ;; Open PostgreSQL port to web01 only
      (simple-service 'concourse-db-firewall firewall-service-type
                      (nftables-rules
                       (input (list "ip saddr 192.168.51.199 tcp dport 5432 accept comment \"postgresql-concourse\""))))
      (service alloy-service-type
               (alloy-configuration
                (hostname "concourse-db.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/postgresql/*.log" "postgresql")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")))))
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")))))))))

concourse-db-os
