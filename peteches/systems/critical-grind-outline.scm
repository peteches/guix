;; critical-grind-outline.scm — Outline wiki server for the Critical Grind campaign.
;; Runs outlinewiki/outline via oci-service-type, with local PostgreSQL and Redis.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.

(define-module (peteches systems critical-grind-outline)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages databases)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gnu services containers)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services outline)
  #:use-module (peteches services restic)
  #:use-module (sops secrets)
  #:export (critical-grind-outline-os))

(define %outline-hba
  (plain-file "pg_hba.conf"
              "local   all     all                     trust\n\
host    outline outline  10.88.0.0/16   scram-sha-256\n\
host    outline outline  127.0.0.1/32   scram-sha-256\n"))

(define-public critical-grind-outline-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "critical-grind-outline.peteches.co.uk"
     #:ipv4-address "192.168.51.203/23"
     #:ipv6-address "2a10:d582:ef59::110/64"
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
        (mount-point "/sys/fs/cgroup")
        (device "cgroup2")
        (type "cgroup2")
        (flags '(no-exec no-suid no-dev))
        (check? #f)
        (create-mount-point? #t)))
     #:restic-config
     (restic-vm-backup-configuration
      (vm-name "critical-grind-outline")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/outline" "/var/lib/postgresql"))
      (password-file "/run/secrets/restic-backup-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("restic-password"))
       (file (local-file "../../secrets/hosts/critical-grind-outline/restic.yaml"))
       (path "/run/secrets/restic-backup-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/critical-grind-outline/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400))
      (sops-secret
       (key '("secret-key"))
       (file (local-file "../../secrets/hosts/critical-grind-outline/outline.yaml"))
       (path "/run/secrets/outline-secret-key"))
      (sops-secret
       (key '("utils-secret"))
       (file (local-file "../../secrets/hosts/critical-grind-outline/outline.yaml"))
       (path "/run/secrets/outline-utils-secret"))
      (sops-secret
       (key '("database-url"))
       (file (local-file "../../secrets/hosts/critical-grind-outline/outline.yaml"))
       (path "/run/secrets/outline-database-url"))
      (sops-secret
       (key '("db-password"))
       (file (local-file "../../secrets/hosts/critical-grind-outline/outline.yaml"))
       (path "/run/secrets/outline-db-password")
       (group "postgres")
       (permissions #o440))
      (sops-secret
       (key '("smtp-password"))
       (file (local-file "../../secrets/hosts/critical-grind-outline/outline.yaml"))
       (path "/run/secrets/outline-smtp-password"))
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/critical-grind-outline/redis.yaml"))
       (path "/run/secrets/redis-password"))
      (sops-secret
       (key '("url-env"))
       (file (local-file "../../secrets/hosts/critical-grind-outline/redis.yaml"))
       (path "/run/secrets/redis-url-env")))
     #:extra-services
     (list
      ;; Declare the cgroup group required by oci-service-type with Podman + root user.
      (simple-service 'cgroup-group
                      account-service-type
                      (list (user-group
                             (name "cgroup")
                             (system? #t))))

      (service oci-service-type
               (oci-configuration
                (runtime 'podman)
                (user "root")))

      ;; Mount cgroup2 so Podman/crun can manage container cgroups.
      ;; Replaces the elogind-service-type dependency (which breaks PAM during deploy)
      ;; with a minimal one-shot service that just mounts the filesystem.
      ;; cgroups2-limits, rootless-podman-shared-root-fs, dbus-system remain stubs
      ;; since Podman 5 + crun only strictly require the cgroup2 mount.
      (simple-service 'cgroup-stubs
                      shepherd-root-service-type
                      (list
                       (shepherd-service
                        (provision '(cgroups2-fs-owner))
                        (one-shot? #t)
                        (start #~(lambda _ #t))
                        (documentation "Stub."))
                       (shepherd-service
                        (provision '(cgroups2-limits))
                        (one-shot? #t)
                        (start #~(lambda _ #t))
                        (documentation "Stub."))
                       (shepherd-service
                        (provision '(rootless-podman-shared-root-fs))
                        (one-shot? #t)
                        (start #~(lambda _ #t))
                        (documentation "Stub."))
                       (shepherd-service
                        (provision '(dbus-system))
                        (one-shot? #t)
                        (start #~(lambda _ #t))
                        (documentation "Stub."))))

      ;; Podman requires a trust policy for image pulls.
      (simple-service 'containers-policy
                      etc-service-type
                      (list (list "containers/policy.json"
                                  (plain-file "containers-policy.json"
                                              "{\"default\":[{\"type\":\"insecureAcceptAnything\"}]}"))))


      ;; Set Redis requirepass from SOPS secret after redis + sops-secrets are up.
      ;; Outline retries its Redis connection so it will succeed once this completes.
      (simple-service 'redis-auth
                      shepherd-root-service-type
                      (list (shepherd-service
                             (provision '(redis-auth))
                             (requirement '(redis sops-secrets))
                             (one-shot? #t)
                             (start #~(lambda _
                                        (let* ((port (open-input-file "/run/secrets/redis-password"))
                                               (pw (let loop ((c (read-char port)) (chars '()))
                                                     (if (eof-object? c)
                                                         (string-trim-right (list->string (reverse chars)))
                                                         (loop (read-char port) (cons c chars)))))
                                               (_ (close-port port)))
                                          (zero? (system* #$(file-append redis "/bin/redis-cli")
                                                          "CONFIG" "SET" "requirepass" pw)))))
                             (documentation "Set Redis auth password from SOPS secret."))))

      ;; Allow Podman bridge (10.88.0.0/16) to reach PostgreSQL and Redis on the host,
      ;; and forward traffic into/out of the Podman bridge so host-port mappings work.
      (simple-service 'container-db-firewall
                      firewall-service-type
                      (nftables-rules
                       (input (list "ip saddr 10.88.0.0/16 tcp dport 5432 accept comment \"postgres-container\""
                                    "ip saddr 10.88.0.0/16 tcp dport 6379 accept comment \"redis-container\""))
                       (forward (list "ct state { established, related } accept"
                                      "iifname \"podman0\" accept"
                                      "oifname \"podman0\" accept"))))

      ;; PostgreSQL — TCP enabled so the Outline Docker container can reach it
      (service postgresql-service-type
               (postgresql-configuration
                (postgresql postgresql)
                (config-file
                 (postgresql-config-file
                  (hba-file %outline-hba)
                  (extra-config
                   '(("listen_addresses" "0.0.0.0")))))))

      ;; Create 'outline' role + database on first boot
      (service postgresql-role-service-type
               (postgresql-role-configuration
                (roles (list (postgresql-role
                              (name "outline")
                              (password-file "/run/secrets/outline-db-password")
                              (permissions '(login))
                              (create-database? #t))))))

      ;; Redis — bind on all interfaces for Docker container access
      (service redis-service-type
               (redis-configuration
                (bind "0.0.0.0")))

      ;; Outline wiki — OCI container via oci-service-type extension
      ;; NOTE: On first boot, if the outline Shepherd service starts before
      ;; postgres-roles completes, run: herd restart outline
      (service outline-service-type
               (outline-configuration
                (url "http://critical-grind-outline.peteches.co.uk:3000")
                (port 3000)
                (data-dir "/var/lib/outline")
                (secret-key-file   "/run/secrets/outline-secret-key")
                (utils-secret-file "/run/secrets/outline-utils-secret")
                (database-url-file "/run/secrets/outline-database-url")
                (redis-url-file "/run/secrets/redis-url-env")
                (smtp-host     "smtp.fastmail.com")
                (smtp-port     587)
                (smtp-username "criticalgrind@peteches.co.uk")
                (smtp-password-file "/run/secrets/outline-smtp-password")
                (smtp-from-email  "criticalgrind@peteches.co.uk")
                (smtp-reply-email "criticalgrind@peteches.co.uk")
                (extra-environment '("PGSSLMODE=disable"
                                    "FORCE_HTTPS=false"))))

      (service alloy-service-type
               (alloy-configuration
                (hostname "critical-grind-outline.peteches.co.uk")
                (log-files (list (cons "/var/log/messages"  "syslog")
                                 (cons "/var/log/outline.log" "outline")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")))))))))
)
critical-grind-outline-os
