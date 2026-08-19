;; claude-workstation.scm — headless VM hosting Claude Code for three accounts.
;;
;; One VM, three login users, one per identity:
;;   peteches      — the personal account (the baseline %vm-peteches-user
;;                   that every VM already gets from vm-base.scm)
;;   criticalgrind — the Critical Grind work account (added here)
;;   ygo           — Peter McCabe's ygo.ai identity (added here)
;;
;; Each user runs their OWN `guix home reconfigure' against a config in
;; peteches/home/configs/:
;;   claude-workstation-peteches.scm
;;   claude-workstation-criticalgrind.scm
;;   claude-workstation-ygo.scm
;; All three instantiate the shared constructor in
;; (peteches home modules claude-workstation): it installs claude-code,
;; symlinks ~/.claude from configs/claude/defaults, registers that account's
;; MCP servers, and pre-clones repos into ~/area_51/<repo>.
;;
;; WHY separate OS users rather than the container-session wrapper: each user
;; gets its own ~/.claude.json (auth + per-project state), so the two accounts
;; never race on that file and run in parallel for free — the exact thing the
;; container wrapper worked hard to fake on one identity.
;;
;; FIRST-BOOT TODO (none of these can be done before the VM exists):
;;   * age-keys/claude-workstation.pub + a .sops.yaml creation rule, then wire
;;     any sops-secrets (e.g. the Plane/Outline API keys for criticalgrind).
;;   * create + encrypt secrets/hosts/claude-workstation/slack.yaml (ygo's
;;     Slack MCP xoxp- token) -- see the #:sops-secrets entry below for the
;;     exact `sops -e -i' invocation.
;; See CLAUDE.md "Adding a New VM" for the remaining fleet-integration files
;; (ssh.scm, machines.scm, scripts/deploy.scm, pihole.scm, monitored-hosts,
;; prometheus.scm, proxmox-vms.org, infra/terraform).

(define-module (peteches systems claude-workstation)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services ssh)
  #:use-module (gnu services base)
  #:use-module (gnu services containers)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module ((gnu packages databases) #:select (postgresql-16))
  #:use-module (peteches packages postgres-extensions)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services firewall)
  #:use-module (peteches services tailscale)
  #:use-module (peteches services wireguard-socks5)
  #:use-module (sops secrets)
  #:use-module (gnu services guix)
  #:use-module (peteches home configs claude-workstation-peteches)
  #:use-module (peteches home configs claude-workstation-criticalgrind)
  #:use-module (peteches home configs claude-workstation-ygo)
  #:export (claude-workstation-os))

;; This VM has no elogind/PAM session management (vm-base.scm starts from
;; bare %base-services, not %desktop-services), so nothing ever creates
;; /run/user/<uid> -- and /run itself isn't even tmpfs here, only /dev/shm
;; is. (peteches home modules claude-workstation) points each account's
;; XDG_RUNTIME_DIR at ~/.cache/xdg-runtime instead, since that's always
;; writable, but that directory lives on the root ext4 filesystem and
;; therefore survives reboots -- which silently breaks Guix Home's
;; on-first-login bootstrap (the script that starts shepherd-for-home,
;; supervising anvil and herdr): it's guarded by a flag file inside that
;; same directory, meant to fire once per boot, that without a real tmpfs
;; there only ever fires once for the account's entire lifetime. A small
;; per-account tmpfs mounted at that exact path restores real "cleared
;; every boot" semantics without pulling in elogind's full PAM/D-Bus
;; session stack. UIDs are static for these three accounts (verified via
;; `id`): peteches=1000, criticalgrind=1001, ygo=1002.
(define (xdg-runtime-tmpfs user uid)
  (file-system
   (mount-point (string-append "/home/" user "/.cache/xdg-runtime"))
   (device "none")
   (type "tmpfs")
   (options (string-append "size=64M,mode=0700,uid=" (number->string uid)
                            ",gid=998"))
   (check? #f)))

;; Console-recovery user for the criticalgrind account.  Same shape as
;; %vm-peteches-user (vm-base.scm): day-to-day access is SSH key-only, so this
;; pre-hashed password exists only for the Proxmox console.  It reuses the
;; already-committed peteches hash so there is a known console login; give it
;; its own hash later if the accounts should differ.
(define %criticalgrind-user
  (user-account
   (name "criticalgrind")
   (comment "Critical Grind (Claude account)")
   (group "users")
   (home-directory "/home/criticalgrind")
   (supplementary-groups '("wheel" "netdev"))
   (password "$6$yk5pnJr/ECPPOvGv$/HoWZNE7fWDslHHIVHAcaxk0AyhnthoHGhs3RrXaXqvVL8W5UI9OUVHndx4RfSqnWnnPw/.q2KhkfrPRKkw.11")))

;; Console-recovery user for the ygo account.  Same shape as %criticalgrind-user
;; and reuses the same pre-hashed console password -- day-to-day access is
;; SSH key-only.
(define %ygo-user
  (user-account
   (name "ygo")
   (comment "Peter McCabe (ygo.ai account)")
   (group "users")
   (home-directory "/home/ygo")
   (supplementary-groups '("wheel" "netdev"))
   (password "$6$yk5pnJr/ECPPOvGv$/HoWZNE7fWDslHHIVHAcaxk0AyhnthoHGhs3RrXaXqvVL8W5UI9OUVHndx4RfSqnWnnPw/.q2KhkfrPRKkw.11")))

;; Matches the old ygo-dev-postgres container's POSTGRES_HOST_AUTH_METHOD=
;; trust: loopback-only dev database, so no password required either via
;; the unix socket or over 127.0.0.1/::1 TCP.
(define %ygo-dev-pg-hba
  (plain-file "pg_hba.conf"
              "local   all     all                     trust\n\
host    all     all     127.0.0.1/32            trust\n\
host    all     all     ::1/128                 trust\n"))

(define-public claude-workstation-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "claude-workstation.peteches.co.uk"
     #:ipv4-address "192.168.51.205/23"
     #:ipv6-address "2a10:d582:ef59::111/64"
     #:with-nug-offload? #t
     #:automation-key-extra-users '("criticalgrind" "ygo")
     #:users-extra (list %criticalgrind-user %ygo-user)
     ;; Tailscale unattended join.  The auth-key is a SHARED sops secret
     ;; (secrets/shared/tailscale.yaml), decrypted at boot with the VM's own
     ;; age key.  The VM pipeline must add this host's age key as a recipient
     ;; of that file (re-encrypt) before the join can succeed; until then
     ;; tailscaled starts but cannot authenticate.
     #:sops-secrets
     (list
      (sops-secret
       (key '("auth-key"))
       (file (local-file "../../secrets/shared/tailscale.yaml"))
       (path "/run/secrets/tailscale-auth-key")
       (permissions #o400))
      ;; Full wg-quick config (private key, peer public key, endpoint,
      ;; tunnel address) as ONE opaque secret value -- see (peteches
      ;; services wireguard-socks5) and docs/secrets-management.org for the
      ;; expected wg0.conf template. wg0 runs inside its own network
      ;; namespace, so the template no longer needs Table/PostUp/PreDown
      ;; lines -- a plain default route via wg0 already sends everything in
      ;; that namespace over the tunnel. The path MUST be named
      ;; "wg0.conf" (basename "wg0") to match the "wg0" interface name used
      ;; below, since wg-quick derives the interface name from the config
      ;; file's basename.
      (sops-secret
       (key '("wg0-conf"))
       (file (local-file "../../secrets/hosts/claude-workstation/wireguard.yaml"))
       (path "/run/secrets/wg0.conf")
       (permissions #o400))
      ;; criticalgrind's Plane/Outline MCP keys, decrypted here with this
      ;; VM's own age key. secrets/hosts/claude-workstation/critical-grind.env
      ;; also carries the operator's PGP key as a recipient, purely so it
      ;; can be edited from a desktop with gpg-agent -- see
      ;; docs/secrets-management.org.
      (sops-secret
       (key '("PLANE_API_KEY"))
       (file (local-file "../../secrets/hosts/claude-workstation/critical-grind.env"))
       (user "criticalgrind")
       (group "users")
       (permissions #o400)
       (path "/run/secrets/plane-api-key"))
      (sops-secret
       (key '("OUTLINE_API_KEY"))
       (file (local-file "../../secrets/hosts/claude-workstation/critical-grind.env"))
       (user "criticalgrind")
       (group "users")
       (permissions #o400)
       (path "/run/secrets/outline-api-key"))
      ;; ygo's Slack MCP token. secrets/hosts/claude-workstation/slack.yaml
      ;; does NOT exist yet -- create it before the next `guix system
      ;; reconfigure`/redeploy of this VM:
      ;;   printf 'xoxp-token: xoxp-...\n' > secrets/hosts/claude-workstation/slack.yaml
      ;;   sops -e -i secrets/hosts/claude-workstation/slack.yaml
      ;; It is picked up automatically by the existing
      ;; `secrets/hosts/claude-workstation/.*\.yaml$` creation_rule in
      ;; .sops.yaml (both this VM's age key and the operator's PGP key as
      ;; recipients) -- no .sops.yaml change needed. See
      ;; docs/secrets-management.org.
      (sops-secret
       (key '("xoxp-token"))
       (file (local-file "../../secrets/hosts/claude-workstation/slack.yaml"))
       (user "ygo")
       (group "users")
       (permissions #o400)
       (path "/run/secrets/slack-mcp-xoxp-token"))
      ;; ygo's 1Password CLI (op) auth: a service-account token, not an
      ;; interactive session -- no expiry, no master-password prompt, just
      ;; an env var op reads per invocation (see (peteches packages
      ;; onepassword-cli)). secrets/hosts/claude-workstation/onepassword.yaml
      ;; does NOT exist yet -- create it before the next `guix system
      ;; reconfigure`/redeploy of this VM (generate the token itself from
      ;; the 1Password web console: Settings -> Developer -> Service
      ;; Accounts):
      ;;   printf 'token: ops_...\n' > secrets/hosts/claude-workstation/onepassword.yaml
      ;;   sops -e -i secrets/hosts/claude-workstation/onepassword.yaml
      ;; Picked up automatically by the existing
      ;; `secrets/hosts/claude-workstation/.*\.yaml$` creation_rule in
      ;; .sops.yaml -- no .sops.yaml change needed. See
      ;; docs/secrets-management.org.
      (sops-secret
       (key '("token"))
       (file (local-file "../../secrets/hosts/claude-workstation/onepassword.yaml"))
       (user "ygo")
       (group "users")
       (permissions #o400)
       (path "/run/secrets/op-service-account-token"))
      ;; Private half of the peteches automation SSH keypair -- see
      ;; %automation-authorized-key-secret in vm-base.scm for the public
      ;; half, decrypted on every VM. Only claude-workstation ever
      ;; decrypts the private half: peteches uses it (via
      ;; %peteches-automation-ssh-key in (peteches home modules
      ;; claude-workstation)) to reach the criticalgrind/ygo accounts
      ;; locally, and any other VM's peteches account, without borrowing a
      ;; desktop's admin key.
      (sops-secret
       (key '("private-key"))
       (file (local-file "../../secrets/hosts/claude-workstation/peteches-automation-ssh.yaml"))
       (user "peteches")
       (group "users")
       (permissions #o400)
       (path "/run/secrets/peteches-automation-ssh-key"))
      ;; Private half of the guix-offload keypair, encrypted for this VM's
      ;; own age key -- see the matching public half added to
      ;; guix-offload-authorized-keys in peteches/systems/nug.scm. Same
      ;; shape as every other VM's guix-build.yaml (see arr.scm, caddy.scm,
      ;; etc.); nug is reachable over Tailscale now, so this VM can offload
      ;; too.
      (sops-secret
       (key '("ssh-private-key"))
       (file (local-file "../../secrets/hosts/claude-workstation/guix-build.yaml"))
       (path "/run/secrets/guix-offload-key")
       (permissions #o400)))
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
      ;; cgroup2 mount required by Podman/crun for the dev database
      ;; containers below (see plane.scm / critical-grind-outline.scm for
      ;; the same requirement).
      (file-system
        (mount-point "/sys/fs/cgroup")
        (device "cgroup2")
        (type "cgroup2")
        (flags '(no-exec no-suid no-dev))
        (check? #f)
        (create-mount-point? #t))
      (xdg-runtime-tmpfs "peteches" 1000)
      (xdg-runtime-tmpfs "criticalgrind" 1001)
      (xdg-runtime-tmpfs "ygo" 1002))
     #:extra-services
     (list
      (service guix-home-service-type
       `(("peteches" ,claude-workstation-peteches-home)
         ("criticalgrind" ,claude-workstation-criticalgrind-home)
         ("ygo" ,claude-workstation-ygo-home)))
      ;; Authorize the same admin keys (nug + nyarlothotep) for the
      ;; criticalgrind and ygo users, so `ssh criticalgrind@…' / `ssh ygo@…'
      ;; work key-only just like the peteches account.  openssh-service-type
      ;; coalesces this with the peteches entry vm-base already sets.
      (simple-service 'criticalgrind-authorized-keys
                      openssh-service-type
                      `(("criticalgrind" ,@%vm-peteches-authorized-keys)))
      (simple-service 'ygo-authorized-keys
                      openssh-service-type
                      `(("ygo" ,@%vm-peteches-authorized-keys)))
      ;; Claude reaches Anthropic outbound; the base firewall permits
      ;; established/related + output and opens ssh inbound.  Tailscale needs
      ;; no inbound rule here: ssh over the tailnet still lands on tcp/22
      ;; (already open) and tailscaled's own traffic is outbound.
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (auth-key-file "/run/secrets/tailscale-auth-key"))))
      ;; Split-tunnel WireGuard behind a local SOCKS5 proxy (127.0.0.1:1080).
      ;; The system default route is untouched -- only processes that dial
      ;; the SOCKS5 proxy get routed over the tunnel. See
      ;; (peteches services wireguard-socks5).
      (service wireguard-socks5-service-type
               (wireguard-socks5-configuration
                (config-file "/run/secrets/wg0.conf")))

      ;; Dev databases for the ygo account: rootful Podman OCI containers
      ;; (same shape as plane.scm / critical-grind-outline.scm), bound to
      ;; loopback only since this is local dev tooling, not a network
      ;; service. POSTGRES_HOST_AUTH_METHOD=trust / no Redis password: safe
      ;; only because nothing but 127.0.0.1 can reach these ports (see the
      ;; firewall rules below), so no SOPS secret is warranted here.
      ;;
      ;; Required by oci-service-type with Podman running as root.
      (simple-service 'cgroup-group
                      account-service-type
                      (list (user-group
                             (name "cgroup")
                             (system? #t))))

      (service oci-service-type
               (oci-configuration
                (runtime 'podman)
                (user "root")))

      ;; Stub shepherd services required by oci-service-type that would
      ;; otherwise depend on elogind (which conflicts with Shepherd-managed
      ;; PAM during deploy) -- see plane.scm / critical-grind-outline.scm.
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
                                              "{\"default\":[{\"type\":\"insecureAcceptAnything\"}]}"))
                            ;; linux-libre has no overlayfs or FUSE support on
                            ;; this VM; vfs is the only working storage driver
                            ;; (see plane.scm).
                            (list "containers/storage.conf"
                                  (plain-file "containers-storage.conf"
                                              "[storage]\ndriver = \"vfs\"\n"))))

      ;; Host directories backing each remaining container's data volume,
      ;; created and owned by the uid/gid each official image runs its
      ;; daemon as (999:999 for both timescaledb and redis's upstream
      ;; images). Postgres itself moved to a native postgresql-service-type
      ;; instance below (needs postgis + pg_cron, neither of which come as
      ;; a ready-made Docker image combo) -- it manages its own
      ;; /var/lib/postgresql/data.
      (simple-service 'ygo-dev-db-dirs
                      activation-service-type
                      #~(begin
                          (use-modules (guix build utils))
                          (for-each
                           (lambda (dir)
                             (mkdir-p dir)
                             (chown dir 999 999))
                           '("/var/lib/ygo-dev-db/timescaledb"
                             "/var/lib/ygo-dev-db/redis"))))

      ;; Loopback-only DNAT still traverses the FORWARD chain once Podman
      ;; rewrites the destination onto the podman0 bridge, so this is needed
      ;; even though nothing off-VM can reach these ports (see
      ;; critical-grind-outline.scm for the identical rule).
      (simple-service 'ygo-dev-db-firewall
                      firewall-service-type
                      (nftables-rules
                       (forward (list "ct state { established, related } accept"
                                      "iifname \"podman0\" accept"
                                      "oifname \"podman0\" accept"))))

      ;; Native PostgreSQL 16 instance for ygo's schema.sql/content-schema.sql
      ;; (port 5432) -- needs postgis and pg_cron as loadable extensions,
      ;; which no upstream Docker image bundles together, so this replaces
      ;; the old docker.io/library/postgres:16 container. Pinned to
      ;; postgresql-16 (not Guix's default postgresql-14) to match that old
      ;; container and the still-Docker-based ygo-dev-timescaledb sibling
      ;; below (timescale/timescaledb:latest-pg16) -- postgis-16 and
      ;; pg-cron (peteches packages postgres-extensions) are both built
      ;; against postgresql-16 specifically for this reason: Postgres
      ;; extensions are ABI-locked to the major version they're compiled
      ;; against. fuzzystrmatch, ltree and pg_trgm are contrib modules
      ;; Guix's postgresql package already builds and installs, so they
      ;; need no extension-packages entry of their own.
      ;;
      ;; pg_cron needs shared_preload_libraries set (it registers a
      ;; background worker at server start, not just a loadable .so) --
      ;; cron.database_name defaults to "postgres"; add it to extra-config
      ;; below if the app's cron jobs live in a different database.
      ;;
      ;; %ygo-dev-pg-hba mirrors the old container's
      ;; POSTGRES_HOST_AUTH_METHOD=trust; listen_addresses defaults to
      ;; localhost, matching the container's 127.0.0.1-only port mapping,
      ;; so no firewall rule is needed here (unlike the podman0 DNAT rule
      ;; the two remaining containers still need).
      (service postgresql-service-type
               (postgresql-configuration
                (postgresql postgresql-16)
                (extension-packages (list postgis-16 pg-cron))
                (config-file
                 (postgresql-config-file
                  (hba-file %ygo-dev-pg-hba)
                  (extra-config
                   '(("shared_preload_libraries" "pg_cron")))))))

      (simple-service 'ygo-dev-timescaledb
                      oci-service-type
                      (oci-extension
                       (containers
                        (list (oci-container-configuration
                               (image "docker.io/timescale/timescaledb:latest-pg16")
                               (provision "ygo-dev-timescaledb")
                               (requirement '(networking file-systems))
                               (log-file "/var/log/ygo-dev-timescaledb.log")
                               (environment '(("POSTGRES_HOST_AUTH_METHOD" . "trust")))
                               ;; Host 5433 -> container 5432: TimescaleDB is a
                               ;; Postgres image and listens on 5432 internally.
                               (ports (list "127.0.0.1:5433:5432"))
                               (volumes (list "/var/lib/ygo-dev-db/timescaledb:/var/lib/postgresql/data")))))))

      (simple-service 'ygo-dev-redis
                      oci-service-type
                      (oci-extension
                       (containers
                        (list (oci-container-configuration
                               (image "docker.io/library/redis:7")
                               (provision "ygo-dev-redis")
                               (requirement '(networking file-systems))
                               (log-file "/var/log/ygo-dev-redis.log")
                               (ports (list "127.0.0.1:6379:6379"))
                               (volumes (list "/var/lib/ygo-dev-db/redis:/data")))))))

      (service alloy-service-type
               (alloy-configuration
                (hostname "claude-workstation.peteches.co.uk")
                ;; The native ygo-dev-postgres instance has no log-files
                ;; entry of its own -- postgresql-config-file's
                ;; log_destination defaults to "syslog", so its output
                ;; already lands in /var/log/messages above.
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/ygo-dev-timescaledb.log" "ygo-dev-timescaledb")
                                 (cons "/var/log/ygo-dev-redis.log" "ygo-dev-redis"))))))))))

claude-workstation-os
