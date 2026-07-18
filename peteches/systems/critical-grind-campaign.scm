;; critical-grind-campaign.scm — Critical Grind campaign system
;; (Go/Gin + HTMX + PostgreSQL).
;;
;; The package and the Shepherd service are NOT defined here.  They live in the
;; application's own repository, which is a Guix channel:
;;
;;     https://git.ts.peteches.co.uk/git/critical-grind-campaign.git
;;     modules (critical-grind packages campaign) / (critical-grind services campaign)
;;
;; pinned in peteches/channels/*.scm like every other channel.  Shipping a new
;; version of the app is therefore a channel commit bump here, not an edit to
;; this file.  See that repository's guix/README.org for the build details.
;;
;; DEVIATIONS FROM THE HOUSE VM STYLE — both deliberate, do not "fix" them:
;;
;;   * bootloader is `grub-efi-bootloader', not `grub-efi-removable-bootloader'.
;;   * file systems are matched by UUID, not by the "GNU-ESP" label.
;;
;; This VM was not built from the standard image; it was adopted from an
;; existing install, and the facts below were read off the running machine
;; rather than assumed.  Re-check before a reconfigure:
;;
;;     ssh peteches@192.168.51.202 lsblk -o NAME,FSTYPE,MOUNTPOINT,UUID
;;
;;   /dev/vda1  vfat  40M  -> /boot/efi   UUID A030-96FA
;;   /dev/vda2  ext4  25G  -> /           UUID 38af4c98-8758-b1c9-009b-584238af4c98
;;   no swap partition or swap file
;;
;; SOPS.  The VM's age key is enrolled (age-keys/critical-grind-campaign.pub,
;; plus a creation rule in .sops.yaml), so secrets can be encrypted to it.
;;
;; Never reference a secrets/ file before it exists.  A local-file pointing at
;; a missing path does not fail politely -- it breaks the build for the WHOLE
;; fleet, because machines.scm imports this module and every deploy loads that.
;; Create the encrypted file first, then add the sops-secret.
;;
;; TODO, both needing further files under secrets/hosts/critical-grind-campaign/
;; (see docs/secrets-management.org):
;;
;;   * SESSION_SECRET as a sops-secret rather than the hand-provisioned
;;     /etc/critical-grind/env described below.
;;   * #:with-nug-offload? can go back to its #t default once guix-build.yaml
;;     exists AND the VM's guix-offload public key is in nug.scm's
;;     authorized-keys.  Enabling it with only one half fails silently, so it
;;     is off for now.

(define-module (peteches systems critical-grind-campaign)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages databases)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system uuid)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services restic)
  #:use-module (peteches services tailscale)
  #:use-module (sops secrets)
  #:use-module (critical-grind services campaign)
  #:export (critical-grind-campaign-os))

;; No TCP listener at all: the app connects over the UNIX socket and
;; authenticates by peer, so the database is unreachable from the network by
;; construction and there is no password anywhere to leak.  The role name must
;; match the service's system user for peer auth to work.
(define %critical-grind-pg-hba
  (plain-file "pg_hba.conf"
              "local all all peer\n"))

(define-public critical-grind-campaign-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "critical-grind-campaign.peteches.co.uk"
     #:ipv4-address "192.168.51.202/23"
     #:ipv6-address "2a10:d582:ef59::109/64"
     ;; Not the removable-media bootloader — see the header note.
     #:bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout (keyboard-layout "gb")))
     #:file-systems
     (list
      (file-system
        (mount-point "/boot/efi")
        (device (uuid "A030-96FA" 'fat32))
        (type "vfat"))
      (file-system
        (mount-point "/")
        (device (uuid "38af4c98-8758-b1c9-009b-584238af4c98" 'ext4))
        (type "ext4")))
     ;; psql and pg_dump for maintenance; the app itself needs nothing extra.
     #:extra-packages (list postgresql)
     ;; /var/lib/postgresql is the whole of the application's state.  Uploaded
     ;; terrain images are NOT on disk -- AdminUploadTerrainImage reads them
     ;; into memory and stores the bytes in the database -- and the HTML
     ;; templates and static assets are compiled into the binary via embed.FS.
     ;; /etc/critical-grind carries the hand-provisioned SESSION_SECRET; losing
     ;; it only invalidates live sessions, but it is unmanaged by Guix and so
     ;; exists nowhere else.
     #:restic-config
     (restic-vm-backup-configuration
      (vm-name "critical-grind-campaign")
      (synology-host "nas.peteches.co.uk")
      (backup-paths '("/var/lib/postgresql" "/etc/critical-grind"))
      (password-file "/run/secrets/restic-password")
      (ssh-key-file "/run/secrets/restic-ssh-key"))
     #:sops-secrets
     (list
      (sops-secret
       (key '("restic-password"))
       (file (local-file "../../secrets/hosts/critical-grind-campaign/restic.yaml"))
       (path "/run/secrets/restic-password"))
      (sops-secret
       (key '("ssh-key"))
       (file (local-file "../../secrets/hosts/critical-grind-campaign/restic.yaml"))
       (path "/run/secrets/restic-ssh-key")
       (permissions #o400)))
     ;; See the TODO in the header.
     #:with-nug-offload? #f
     #:extra-services
     (list
      ;; Caddy reaches every backend in this fleet over Tailscale, not the
      ;; LAN, so the reverse proxy in peteches/systems/caddy.scm depends on
      ;; this.  The node has to be authenticated by hand once after first
      ;; boot -- `sudo tailscale up' on the VM -- exactly as for the other
      ;; Tailscale VMs; there is no auth key in sops.
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches"))))

      (service critical-grind-service-type
               (critical-grind-configuration
                (port 8080)
                (log-level "info")
                ;; NOT managed by Guix: everything in /gnu/store is
                ;; world-readable, so SESSION_SECRET is read from this file at
                ;; service start.  Provision it by hand — the criticalgrind
                ;; user only exists after the first successful reconfigure:
                ;;
                ;;   sudo mkdir -p /etc/critical-grind
                ;;   printf 'SESSION_SECRET=%s\n' "$(openssl rand -base64 48)" \
                ;;     | sudo tee /etc/critical-grind/env >/dev/null
                ;;   sudo chown criticalgrind:criticalgrind /etc/critical-grind/env
                ;;   sudo chmod 0400 /etc/critical-grind/env
                ;;   sudo herd restart critical-grind
                ;;
                ;; The service exits with a clear error if it is missing.
                (environment-file "/etc/critical-grind/env")))

      (service postgresql-service-type
               (postgresql-configuration
                (postgresql postgresql)
                (config-file
                 (postgresql-config-file
                  (hba-file %critical-grind-pg-hba)
                  (extra-config
                   '(("listen_addresses" "")
                     ("log_destination" "syslog")))))))

      ;; Creates role `criticalgrind' and a database of the same name.
      (service postgresql-role-service-type
               (postgresql-role-configuration
                (roles
                 (list (postgresql-role
                        (name "criticalgrind")
                        (create-database? #t))))))

      ;; The Go process runs unprivileged and serves plain HTTP directly on
      ;; :8080 — no reverse proxy, so nothing needs CAP_NET_BIND_SERVICE and
      ;; port 80 stays closed.  PostgreSQL is deliberately absent: it has no
      ;; TCP listener to open a port for.
      (simple-service 'critical-grind-firewall
                      firewall-service-type
                      (nftables-rules
                       (input
                        (list "tcp dport 8080 accept comment \"critical-grind\""))))

      (service alloy-service-type
               (alloy-configuration
                (hostname "critical-grind-campaign.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")
                                 (cons "/var/log/critical-grind.log" "critical-grind-campaign"))))))))))

critical-grind-campaign-os
