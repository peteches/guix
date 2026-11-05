;; claude-workstation.scm -- a headless VM that is now a pure jump box.
;;
;; HISTORY -> PRESENT.  This VM used to be Claude Code's dedicated
;; workstation: one VM, three login users (peteches, criticalgrind, ygo), each
;; with its own home-environment config in peteches/home/configs/ built by the
;; shared constructor in (peteches home modules claude-workstation); a native
;; Postgres 17 + postgis + pg_cron instance and two Podman dev-DB containers
;; that the Claude MCP servers talked to; `pi-coding-agent' in the system
;; profile.  Claude Code is not needed anywhere any more -- the harness is
;; `pi', on the desktops for interactive work and on `agents' for the headless
;; account (see peteches/systems/agents.scm) -- so all of that is gone.  What
;; is left is the one job this box was always good at, plus what keeps it
;; reachable:
;;
;;   * wireguard-socks5-service-type -- split-tunnel WireGuard (wg0, inside
;;     its own netns) feeding microsocks on 10.200.0.2:1080.  THE REASON THIS
;;     VM EXISTS.  The desktops have no kernel route to `ygo-server',
;;     `ygoswiss1' or `ygoswiss2', so their ssh ProxyCommands relay through
;;     here: `ssh claude-workstation -- proxychains4 -q -f
;;     ~/.config/proxychains.conf nc %h %p' ((peteches home modules ssh)).
;;     That is also why proxychains-ng + netcat stay installed -- see the
;;     #:extra-packages comment below for where they live and why sshd's
;;     non-interactive shell can still find them.  auto-start? #t brings the
;;     netns, tunnel and proxy up on boot; see the `read-line' gotcha comment
;;     inside the proxy's start thunk in peteches/services/wireguard-socks5.scm
;;     for why it was being left "stopped (failing)" after the channel bump.
;;   * tailscale-service-type -- this box's own tailnet presence, so the
;;     operator's `Host *.ts' aliases keep resolving.
;;   * alloy-service-type -- pushes messages.log, node-exporter.log, ntpd.log
;;     and alloy.log to loki (192.168.51.190:3100).  Its metrics port 12345 is
;;     what prometheus scrapes -- the hand-written `alloy' job in
;;     peteches/systems/prometheus.scm lists 192.168.51.205:12345 -- and
;;     alloy-service-type opens that port in the firewall itself, so there is
;;     no `simple-service ... firewall-service-type' extension here.
;;   * ONE home environment: `peteches', from
;;     peteches/home/configs/claude-workstation-peteches.scm (built by
;;     (peteches home modules vm-jumpbox)) -- git, ssh, proxychains, a pinned
;;     ~/.config/guix/channels.scm, nothing else.  It needs no separate
;;     `guix home reconfigure': this #:extra-services wires the
;;     home-environment value straight into the system, so `guix system
;;     reconfigure'/`guix deploy' activates it as the guix-home-peteches
;;     shepherd service in the same run -- editing a home config for this VM
;;     just needs a redeploy (scripts/deploy.scm; see AGENTS.md).  Its
;;     ~/.ssh/config now carries NO IdentityFile of its own: the private half
;;     of the peteches automation keypair that used to sit behind a `Host *'
;;     entry here moved to `agents' on 2026-11-05 (see
;;     peteches/systems/agents.scm), so this box has no outbound fleet
;;     identity, cannot ssh into any other VM as peteches, and is no longer a
;;     deploy coordinator -- deploys run from the desktops or from `agents'.
;;     Every ssh session this box serves arrives FROM a desktop (the
;;     ProxyCommand hop above), so it has no reason to ssh out at all.
;;   * TWO OS accounts: `peteches' (that environment) and `ygo', now only a
;;     login account -- key + console password, no home environment, nothing
;;     running as it.
;;
;; WHY separate OS users: each user gets its own ~/.claude.json (auth +
;; per-project state), so the accounts never race on that file and run in
;; parallel for free -- the exact thing the container-session wrapper used to
;; work hard to fake on a single identity.  That argument no longer applies on
;; this box (there is no Claude Code left on it), `criticalgrind' is gone with
;; Critical Grind's own VM, and `ygo' stays only because Peter McCabe still
;; logs into it.
;;
;; Fleet-integration files for this VM (ssh.scm, machines.scm,
;; scripts/deploy.scm, pihole.scm, monitored-hosts.scm, prometheus.scm,
;; proxmox-vms.org, infra/terraform, age-keys/) are already wired -- see
;; AGENTS.md "Adding a New VM" for what those mean for a NEW host.

(define-module (peteches systems claude-workstation)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system shadow)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services tailscale)
  #:use-module (peteches services wireguard-socks5)
  #:use-module (sops secrets)
  #:use-module (gnu services guix)
  #:use-module (peteches home configs claude-workstation-peteches)
  #:export (claude-workstation-os))

;; Login account for Peter McCabe's ygo.ai identity.  It keeps the same
;; pre-hashed console password as %vm-peteches-user (vm-base.scm) -- a
;; known console login for recovery -- but has no guix-home environment,
;; no toolchain and nothing running as it; day-to-day access is SSH
;; key-only (`ssh claude-workstation-ygo' from a desktop).
(define %ygo-user
  (user-account
   (name "ygo")
   (comment "Peter McCabe (ygo.ai account)")
   (group "users")
   (home-directory "/home/ygo")
   (supplementary-groups '("wheel" "netdev"))
   (password "$6$yk5pnJr/ECPPOvGv$/HoWZNE7fWDslHHIVHAcaxk0AyhnthoHGhs3RrXaXqvVL8W5UI9OUVHndx4RfSqnWnnPw/.q2KhkfrPRKkw.11")))

(define-public claude-workstation-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "claude-workstation.peteches.co.uk"
     #:ipv4-address "192.168.51.205/23"
     #:ipv6-address "2a10:d582:ef59::111/64"
     #:with-nug-offload? #t
     ;; No #:extra-packages.  `pi-coding-agent' used to sit here, which put
     ;; `pi' on this box's SYSTEM PATH -- the harness runs where the user is
     ;; (the desktops, and `agents' for the headless account), so a box whose
     ;; only job is relaying other people's proxied ssh does not need it.
     ;; proxychains-ng + netcat, which that job does need, come in through
     ;; peteches' HOME profile instead: Guix System's /etc/profile puts
     ;; ~/.guix-home/profile on PATH even for the non-interactive shell sshd
     ;; execs, so the desktops' ProxyCommands still resolve (verified on the
     ;; live box).
     ;;
     ;; No #:automation-key-extra-users (default '()): there is no private
     ;; automation key left on this box for any account to read -- the
     ;; criticalgrind/ygo entries this used to pass went with those accounts.
     ;; What still matters here is the OTHER half of the pair: this box must
     ;; keep ACCEPTING the fleet-wide automation key so its new holder
     ;; (`agents') can log in as peteches, and that is exactly what vm-base's
     ;; %automation-authorized-key-secret -- the public half, decrypted on
     ;; EVERY VM including this one -- plus the sshd Match block do.  So
     ;; #:with-automation-key? stays at its #t default, and this box's own
     ;; peteches login keeps working for the desktops' admin keys
     ;; (%vm-peteches-authorized-keys).
     #:users-extra (list %ygo-user)
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
      ;; Four sops secrets that used to be listed here are gone: the two
      ;; Plane/Outline keys (critical-grind.env -- Critical Grind now runs on
      ;; its own VM, critical-grind-campaign, 192.168.51.202), slack.yaml and
      ;; onepassword.yaml (ygo's Slack / 1Password MCP tokens) and deepgram.yaml
      ;; (the pi-dictate `rec' wrapper that consumed it is retired; voice
      ;; dictation stays on the desktops, where the DMS dictate plugin uses
      ;; secrets/shared/deepgram.yaml -- see configs/dms/plugins/dictate/).
      ;; The four files are still in secrets/hosts/claude-workstation/ and
      ;; still decryptable with this box's age key; they are simply no longer
      ;; decrypted at boot because nothing reads them any more.
      ;; The peteches automation SSH keypair's PRIVATE half is no longer
      ;; decrypted on this box (2026-11-05): the file that carried it,
      ;; secrets/hosts/claude-workstation/peteches-automation-ssh.yaml, moved
      ;; to secrets/hosts/agents/ and was re-keyed for `agents' age key, and
      ;; peteches/systems/agents.scm now decrypts it to
      ;; /run/secrets/peteches-automation-ssh-key.  What this box still gets,
      ;; automatically from vm-base (%automation-authorized-key-secret), is the
      ;; PUBLIC half -- authorized for `peteches' by the sshd Match block, so
      ;; whoever holds the private half can still land here.
      ;; Private half of the guix-offload keypair, encrypted for this VM's
      ;; own age key -- see the matching public half added to
      ;; guix-offload-authorized-keys in peteches/systems/guix-build.scm. Same
      ;; shape as every other VM's guix-build.yaml (see arr.scm, caddy.scm,
      ;; etc.); guix-build is reachable over Tailscale now, so this VM can
      ;; offload too.
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
      ;; No cgroup2 mount any more (it only existed for the Podman dev-DB
      ;; containers, gone below) and no per-account XDG_RUNTIME_DIR tmpfs:
      ;; (peteches home modules vm-jumpbox) sets no XDG_RUNTIME_DIR at all,
      ;; so nothing here wants a runtime directory cleared on every boot.
      )
     #:extra-services
     (list
      (service guix-home-service-type
       ;; One home environment only -- the retired
       ;; claude-workstation-criticalgrind / claude-workstation-ygo configs
       ;; used to be the other two entries.
       `(("peteches" ,claude-workstation-peteches-home)))
      ;; `ygo' has no guix-home environment any more, and sshd's
      ;; AuthorizedKeysFile is ~/.ssh/authorized_keys
      ;; (%vm-authorized-keys-file in vm-base.scm) -- a ~/.ssh that
      ;; guix-home owns makes sshd skip that path outright, so key auth
      ;; for an account WITHOUT a home environment has to come through
      ;; openssh-service-type's authorized-keys field, which writes
      ;; /run/current-system/profile/etc/ssh/<user>/authorized_keys and
      ;; passes it in with -f.  Without this entry `ssh
      ;; claude-workstation-ygo' from the desktops -- the only way the
      ;; operator reaches that account -- dies with Permission denied
      ;; (publickey).  Same pattern as %agents-peteches-authorized-keys
      ;; in peteches/systems/agents.scm.
      (simple-service 'ygo-authorized-keys
                      openssh-service-type
                      `(("ygo" ,@%vm-peteches-authorized-keys)))
      ;; Nothing here needs a new inbound firewall rule: ssh over the
      ;; tailnet still lands on tcp/22 (already open by %vm-base-firewall),
      ;; tailscaled's own traffic is outbound, alloy's scrape port is
      ;; opened by alloy-service-type itself, and the SOCKS5 proxy is only
      ;; ever dialled from processes already running on this box -- it
      ;; lives inside netns-wg0ns and is reachable solely over the veth.
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches")
                      (auth-key-file "/run/secrets/tailscale-auth-key"))))
      ;; Split-tunnel WireGuard behind a local SOCKS5 proxy (127.0.0.1:1080).
      ;; The system default route is untouched -- only processes that dial
      ;; the SOCKS5 proxy get routed over the tunnel. See
      ;; (peteches services wireguard-socks5). auto-start? #t brings up all
      ;; three shepherd services (netns, wg0, microsocks) on boot, in the
      ;; innermost-last order wsc-shepherd-services already lists them in, so
      ;; proxychains-ng -- on this box, in peteches' home profile -- has a
      ;; live proxy to dial without a manual `herd start' first.  The dialers
      ;; are the desktops' ProxyCommands for the tunnel-only hosts (see
      ;; (peteches home modules ssh)), and the .claude/skills/
      ;; wireguard-socks5 skill documents the same three-service order for
      ;; anyone reaching this box by hand.
      (service wireguard-socks5-service-type
               (wireguard-socks5-configuration
                (config-file "/run/secrets/wg0.conf")
                (auto-start? #t)))

      ;; What used to live from here down was ygo's Claude MCP backend set:
      ;; a rootful-Podman OCI stack (the cgroup group and stub services,
      ;; policy.json + storage.conf, the timescaledb and redis containers,
      ;; their host directories and their podman0 forward rule) plus a
      ;; native postgresql-17 instance with postgis + pg_cron.  Nothing
      ;; talks to those databases any more -- they existed for Claude's
      ;; MCP servers on this box -- so the whole block is gone, and with
      ;; it the cgroup2 mount above.  Dropping the Postgres data service
      ;; here is also what keeps this system deployable at all:
      ;; `guix deploy' refuses to switch Postgres majors while the old
      ;; data service is still running, and nothing on this box needs a
      ;; database.
      (service alloy-service-type
               (alloy-configuration
                (hostname "claude-workstation.peteches.co.uk")
                ;; Every per-service log entry beyond these four went with
                ;; the service it belonged to -- the two dev-DB containers
                ;; that used to sit below, and the native Postgres that used
                ;; to sit above them (postgresql-config-file's
                ;; log_destination defaulted to "syslog", so Postgres output
                ;; still lands in /var/log/messages if anything ever needs
                ;; it again).
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy"))))))))))

claude-workstation-os
