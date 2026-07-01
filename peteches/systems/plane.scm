;; peteches/systems/plane.scm — Plane project management server on a Proxmox QEMU/KVM VM.
;;
;; PostgreSQL and Redis run on the host (Guix services).
;; RabbitMQ runs as an OCI container on the "plane" Podman network.
;; MinIO is provided externally at minio.ts.peteches.co.uk (via Caddy + Tailscale).
;;
;; Plane containers reach host services via host.containers.internal:
;;   DATABASE_URL=postgresql://plane:<pw>@host.containers.internal:5432/plane
;;   REDIS_URL=redis://host.containers.internal:6379
;;   AMQP_URL=amqp://<user>:<pw>@rabbitmq:5672/<vhost>   ← on "plane" network, by hostname
;;
;; Required keys in /run/secrets/plane.env:
;;   SECRET_KEY              — Django secret key (openssl rand -hex 32)
;;   DATABASE_URL            — postgresql://plane:<pw>@host.containers.internal:5432/plane
;;   REDIS_URL               — redis://host.containers.internal:6379
;;   AMQP_URL                — amqp://<user>:<pw>@rabbitmq:5672/<vhost>
;;   AWS_ACCESS_KEY_ID       — MinIO access key
;;   AWS_SECRET_ACCESS_KEY   — MinIO secret key
;;   RABBITMQ_DEFAULT_USER   — RabbitMQ bootstrap user (used by the rabbitmq container)
;;   RABBITMQ_DEFAULT_PASS   — RabbitMQ bootstrap password
;;   RABBITMQ_DEFAULT_VHOST  — RabbitMQ default vhost (e.g. "/")
;;
;; Required key in /run/secrets/plane-db-password:
;;   (raw password, same as the password component of DATABASE_URL above)
;;
;; Bootstrap note: the SOPS secrets require the VM's age key to be enrolled first.
;; EFI bootloader, virtio root on vda2 with ESP on vda1, static networking.

(define-module (peteches systems plane)
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
  #:use-module (peteches services plane)
  #:use-module (peteches services restic)
  #:use-module (peteches services tailscale)
  #:use-module (sops secrets)
  #:export (plane-os))

(define %plane-pg-hba
  (plain-file "pg_hba.conf"
              "local   all     all                     trust\n\
host    plane   plane   10.91.0.0/24    scram-sha-256\n\
host    plane   plane   127.0.0.1/32    trust\n"))

(define-public plane-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "plane.peteches.co.uk"
     #:ipv4-address "192.168.51.204/23"
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
      ;; cgroup2 mount required by Podman/crun for container management
      (file-system
        (mount-point "/sys/fs/cgroup")
        (device "cgroup2")
        (type "cgroup2")
        (flags '(no-exec no-suid no-dev))
        (check? #f)
        (create-mount-point? #t)))
     #:restic-config
     (restic-vm-backup-configuration
      (vm-name "plane")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/postgresql"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("env-file"))
       (file (local-file "../../secrets/hosts/plane/plane.yaml"))
       (path "/run/secrets/plane.env")
       (permissions #o400))
      (sops-secret
       (key '("db-password"))
       (file (local-file "../../secrets/hosts/plane/plane.yaml"))
       (path "/run/secrets/plane-db-password")
       (group "postgres")
       (permissions #o440))
      (sops-secret
       (key '("password"))
       (file (local-file "../../secrets/hosts/plane/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/plane/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400))
      (sops-secret
       (key '("auth-key"))
       (file (local-file "../../secrets/shared/tailscale.yaml"))
       (path "/run/secrets/tailscale-auth-key")
       (permissions #o400)))
     #:extra-services
     (list
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (auth-key-file "/run/secrets/tailscale-auth-key"))))

      ;; Required by oci-service-type with Podman running as root
      (simple-service 'cgroup-group
                      account-service-type
                      (list (user-group
                             (name "cgroup")
                             (system? #t))))

      (service oci-service-type
               (oci-configuration
                (runtime 'podman)
                (user "root")))

      ;; Stub shepherd services required by oci-service-type that would otherwise
      ;; depend on elogind (which conflicts with Shepherd-managed PAM during deploy)
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

      ;; Podman requires a trust policy for image pulls
      (simple-service 'containers-policy
                      etc-service-type
                      (list (list "containers/policy.json"
                                  (plain-file "containers-policy.json"
                                              "{\"default\":[{\"type\":\"insecureAcceptAnything\"}]}"))
                            ;; linux-libre has no overlayfs or FUSE support on this VM;
                            ;; vfs is the only working storage driver.
                            (list "containers/storage.conf"
                                  (plain-file "containers-storage.conf"
                                              "[storage]\ndriver = \"vfs\"\n"))
                            ;; /etc/containers/ is a read-only Guix store symlink so Podman
                            ;; cannot create a networks/ subdir there.  Redirect to a
                            ;; writable location so `podman network create' persists across
                            ;; reboots.
                            (list "containers/containers.conf"
                                  (plain-file "containers-containers.conf"
                                              "[network]\nnetwork_config_dir = \"/var/lib/containers/networks\"\n"))))

      ;; RabbitMQ on the "plane" Podman network.
      ;; Plane worker/beat containers resolve it by hostname "rabbitmq" via Podman DNS.
      ;; AMQP_URL in secrets env-file must reference this hostname:
      ;;   AMQP_URL=amqp://<user>:<pass>@rabbitmq:5672/<vhost>
      ;; Credentials are bootstrapped from the env-file at first start:
      ;;   RABBITMQ_DEFAULT_USER, RABBITMQ_DEFAULT_PASS, RABBITMQ_DEFAULT_VHOST
      (simple-service 'plane-rabbitmq
                      oci-service-type
                      (oci-extension
                       (containers
                        (list (oci-container-configuration
                               (image "docker.io/rabbitmq:3-management")
                               (provision "plane-rabbitmq")
                               (requirement '(networking file-systems sops-secrets))
                               (log-file "/var/log/plane/rabbitmq.log")
                               (network "plane")
                               (extra-arguments
                                (list "--user=root"
                                      "--env-file=/run/secrets/plane.env"
                                      "--network-alias=rabbitmq")))))))

      ;; Allow the plane Podman network (10.91.0.0/24, fixed subnet) to reach
      ;; PostgreSQL and Redis on the host, and allow aardvark-dns queries.
      ;; Forward rules use subnet matching because netavark names bridges
      ;; dynamically (podman0, podman1, …), not after the network.
      ;; The plane Podman network is pinned to 10.91.0.0/24 (set in plane-activation).
      ;; All rules match on source/dest subnet rather than bridge interface name
      ;; because netavark assigns bridge names dynamically (podman0, podman1, …).
      (simple-service 'plane-container-firewall
                      firewall-service-type
                      (nftables-rules
                       (input
                        (list "ip saddr 10.91.0.0/24 tcp dport 5432 accept comment \"postgres-plane\""
                              "ip saddr 10.91.0.0/24 tcp dport 6379 accept comment \"redis-plane\""
                              ;; aardvark-dns listens on 10.91.0.1:53 (the plane bridge gateway).
                              ;; The input policy is drop, so these rules are required or all
                              ;; container DNS lookups time out.
                              "ip saddr 10.91.0.0/24 udp dport 53 accept comment \"aardvark-dns-plane\""
                              "ip saddr 10.91.0.0/24 tcp dport 53 accept comment \"aardvark-dns-plane\""))
                       (forward
                        (list "ct state { established, related } accept"
                              "ip saddr 10.91.0.0/24 accept comment \"plane-containers-out\""
                              "ip daddr 10.91.0.0/24 accept comment \"plane-containers-in\""))))

      ;; PostgreSQL — listens on all interfaces so containers can connect via
      ;; host.containers.internal.  Access is controlled by pg_hba.conf above.
      (service postgresql-service-type
               (postgresql-configuration
                (postgresql postgresql)
                (config-file
                 (postgresql-config-file
                  (hba-file %plane-pg-hba)
                  (extra-config
                   '(("listen_addresses" "0.0.0.0")
                     ("log_directory"    "/var/log/postgresql")))))))

      (service postgresql-role-service-type
               (postgresql-role-configuration
                (roles
                 (list (postgresql-role
                        (name "plane")
                        (password-file "/run/secrets/plane-db-password")
                        (permissions '(login))
                        (create-database? #t))))))

      ;; redis-configuration's config-file field is maybe-string (string path only),
      ;; so deliver the file via etc-service-type and reference it by path.
      (simple-service 'redis-config
                      etc-service-type
                      (list (list "redis/redis.conf"
                                  (plain-file "redis.conf"
                                              "bind 0.0.0.0\nport 6379\ndir /var/lib/redis\ndaemonize no\nprotected-mode no\n"))))

      ;; Redis — listens on all interfaces; access limited by firewall to 10.91.0.0/24.
      ;; protected-mode must be off because there is no password and containers
      ;; connect from 10.91.0.0/24 (non-loopback), which Redis protected-mode rejects.
      (service redis-service-type
               (redis-configuration
                (config-file "/etc/redis/redis.conf")))

      ;; Plane project management — OCI containers via plane-service-type.
      ;; MINIO_ENDPOINT_SSL=1 appended last in extra-environment overrides the
      ;; use-minio?=#t default of =0, since minio.ts.peteches.co.uk uses HTTPS.
      (service plane-service-type
               (plane-configuration
                (web-url "https://plane.ts.peteches.co.uk")
                (port 80)
                (secrets-env-file "/run/secrets/plane.env")
                (s3-endpoint-url "https://minio.ts.peteches.co.uk")
                (s3-bucket-name "plane")
                (s3-region "us-east-1")
                (use-minio? #f)
                (extra-environment '("MINIO_ENDPOINT_SSL=1"))))

      (service alloy-service-type
               (alloy-configuration
                (hostname "plane.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/postgresql/*.log" "postgresql")
                                 (cons "/var/log/plane/*.log" "plane")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")))))))))
)
plane-os
