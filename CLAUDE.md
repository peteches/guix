# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Secret Handling — MANDATORY

**Never print, echo, or display secret or key material in any shell command output.**
This conversation is recorded by Anthropic. Anything that appears in a tool result is
part of that record and must be treated as permanently exposed.

This applies to **all** key material without exception:
- Private keys (SSH, age, GPG)
- Passwords and passphrases
- **Public keys** — even public keys reveal which VM/identity owns them and can be used
  for correlation attacks against SOPS-encrypted files or SSH `authorized_keys`
- Age public keys — expose which recipients can decrypt which secrets
- SSH host keys — fingerprints help attackers impersonate hosts
- IP addresses of newly provisioned machines before they are publicly registered
- Any value read out of a SOPS-encrypted file

**Rules:**
1. Secrets must live exclusively in shell variables or `/dev/shm` files. Never in `/tmp`.
2. Shell commands must **not** `echo`, `cat`, `print`, or otherwise output key material to
   stdout or stderr. Status messages like `echo "Key saved."` are fine; `echo "$KEY"` is not.
3. When a value is needed across steps (e.g. age key, host key), store it in a `/dev/shm`
   file and read it back silently — do not echo it as confirmation.
4. Use `shred -u` to destroy any `/dev/shm` files as soon as they are no longer needed.
5. If a secret has appeared in conversation output, treat it as compromised and rotate it.

## What This Repository Is

A personal [GNU Guix](https://guix.gnu.org/) system configuration repository. All files are written in Guile Scheme. It defines operating systems and home environments for multiple machines. The root Guile module namespace is `(peteches ...)`.

Custom packages, home services, and system services have been migrated to a separate
private guix channel (`peteches`). The channel URL is pinned in `peteches/channels/base.scm`.

## Common Commands

All `guix` commands that reference local modules need `-L .` (or `--load-path=.`) to be run from the repo root.

```bash
# Validate a system config (dry-run)
guix system build -L . --dry-run peteches/systems/<host>.scm

# Build a QCOW2 disk image for a VM
guix system image -L . -t qcow2 peteches/systems/<vm>.scm

# Pull channels (updates all pinned channels including peteches channel).
# Use manual.scm — it is the full plain list. Despite its name,
# channels-nug.scm contains ONLY the peteches channel and would leave
# guix itself unpinned. See "Channels" below.
guix pull -C peteches/channels/manual.scm

# Deploy to all VMs (or filter with --hosts)
scripts/deploy.scm
scripts/deploy.scm --hosts 192.168.51.187          # single VM by IP
scripts/deploy.scm -h "host-name=prometheus"       # by hostname pattern
scripts/deploy.scm -h "prometheus,loki" --dry-run  # multiple patterns

# Check channels
guix describe
```

## Validating changes

Read this before trusting a "it loads fine" claim — several of the obvious
checks give **false positives**.

**1. Bare `guile` cannot read these files.** Anything with a gexp (`#~`, `#$`)
fails with `Unknown # object: "#~"`. That is a reader limitation, *not* a
problem with the file. Use `guix repl` instead, which has the gexp reader:

```bash
guix repl -- /path/to/script.scm       # runs a Guile script with gexps available
```

**2. Parsing is not loading.** A parse check catches unbalanced parens but
happily accepts a file with two `(define-module ...)` forms. Both checks are
needed.

**3. Load-check ONE MODULE PER PROCESS.** When a module fails to load, Guile
can leave a stub registered under its name; a later `resolve-interface` on that
name in the *same process* then succeeds spuriously. Batching load checks in
one `guix repl` therefore reports green for modules that are actually broken:

```bash
# WRONG — later modules can report OK because of stubs from earlier failures
guix repl -L . -- /dev/stdin <<'EOF'
(for-each resolve-interface '((peteches systems vm-base) (peteches machines)))
EOF

# RIGHT — fresh process per module
for m in "(peteches systems vm-base)" "(peteches machines)"; do
  printf '%-36s ' "$m"
  guix repl -L . -L "$CHANNEL" -- /dev/stdin <<EOF >/dev/null 2>&1 && echo OK || echo FAIL
(resolve-interface '$m)
EOF
done
```

**4. Channel modules need `-L`, or the pulled guix.** Most modules here import
`(peteches services ...)` from the external channel, which in turn imports
`(nongnu ...)`, `(sops ...)`, `(guix-science-nonfree ...)`. Point `-L` at a
channel checkout for the `peteches` half:

```bash
guix repl -L . -L ~/area_51/codeberg.org/peteches/guix-channel_main -- …
```

The third-party channels are only available via the **pulled** guix
(`~/.config/guix/current`), which needs container nesting — see below.

**5. Real builds need the guix daemon.** `guix build` / `guix system build`
cannot even compute a derivation without a store connection, so `--dry-run`
does not help.

```bash
guix system build -L . --dry-run peteches/systems/<host>.scm
guix build -L . -L "$CHANNEL" peteches/packages/<pkg>.scm:<name>
```

### Container nesting (required for builds)

A plain `guix shell --container` has **no `/var/guix`** and only the manifest
closure in `/gnu/store` (~200 items), so guix cannot work inside it. The
`claude-container` wrapper therefore supports `--nesting` (alias `--guix`),
which passes `guix shell -W` — mapping the full store read-only, the daemon
socket, and guix's caches — and exposes `~/.config/guix/current` so channel
modules resolve.

It is **off by default** (a reachable daemon can build and install arbitrary
derivations on the host). The `guix` session opts in permanently via an empty
`nesting` file in its session dir (`~/.claude-sessions/guix/nesting`); use
`--nesting` ad hoc, or `--no-nesting` to force it off.

Nesting also makes store-symlinked host config resolve — notably
`~/.config/git/config`, which otherwise dangles, taking `user.name`/`user.email`
and `commit.gpgSign` with it and producing unsigned commits.

Check whether you have it:

```bash
ls /var/guix/daemon-socket/socket   # present => builds work
guix describe                       # lists channels => pulled guix in use
```

The `manual.scm` / `channels-nug.scm` channel files each carry a
`define-module` header (matching their path) but end in a bare `(list …)`, so
they double as plain channel lists for `guix pull -C` while still loading
cleanly when `guix home`/`guix system` scan every module under `-L .` (a plain
list with no module header fails that scan with `no code for module …`). They
use Guix record macros, so bare `guile` still cannot load them — validate with
a read-only parse instead:
```bash
guile -c '(call-with-input-file "peteches/channels/manual.scm"
            (lambda (p) (let loop () (unless (eof-object? (read p)) (loop)))))'
```

## Architecture

### Where code actually lives

Three sources are easy to confuse when reading a `#:use-module` line:

| Module prefix | Where it lives |
|---|---|
| `(gnu ...)`, `(guix ...)` | upstream Guix |
| `(peteches services ...)`, `(peteches home services ...)`, `(peteches packages ...)` | the **external `peteches` channel** (codeberg.org/peteches/guix-channel), pinned in `peteches/channels/base.scm` — **not in this repo** |
| `(critical-grind packages ...)`, `(critical-grind services ...)` | the **`critical-grind` channel** — the application repo at `git@git.peteches.co.uk:critical-grind-campaign`, pinned in `peteches/channels/base.scm` — **not in this repo** |
| `(peteches systems ...)`, `(peteches home modules ...)`, `(peteches home configs ...)`, `(peteches channels ...)`, `(containers ...)` | this repo |

So `alloy-service-type`, `restic-vm-backup-service-type`, `firewall-service-type`,
`tailscale-service-type`, `home-git-service-type` and friends are **defined in the
channel**. This repo only supplies configuration *values* for them. Changing a
service type means editing that channel and re-pinning its commit here.

`peteches/packages/desktop-scripts.scm` is the one exception — a package defined
in this repo, because its source (`configs/bin/`) is here.

### Two OS constructors

| Constructor | Module | Used by |
|---|---|---|
| `make-vm-os` | `peteches/systems/vm-base.scm` | every headless Proxmox VM |
| `make-base-os` | `peteches/systems/base.scm` | the two desktops (nug, nyarlothotep) |

Each system file ends with a **bare expression** naming its OS record
(e.g. `loki-os`). `guix system build FILE` uses that last value; `guix deploy`
imports the module and reads the exported variable. Omit it and `guix system
build` breaks. VMs are deployed via `scripts/deploy.scm`; the desktops are
reconfigured locally and are deliberately absent from `peteches/machines.scm`.

Each module's header comment documents its keyword arguments — read
`vm-base.scm` before adding or changing a VM.

### Directory Layout

| Path | Purpose |
|---|---|
| `peteches/systems/` | OS configurations per host |
| `peteches/home/configs/` | Host-specific home-environment files (`nug.scm`, `nyarlothotep.scm`) |
| `peteches/home/modules/` | Shared home config fragments — `base.scm` plus focused modules (ssh, gpg, theming, ai, etc.) |
| `peteches/monitoring/` | Loki gexp helper — **dead code**, not exported, not called |
| `peteches/channels/` | Channel lock files (four of them — see "Channels") |
| `peteches/packages/` | `desktop-scripts.scm` — the only package defined in this repo |
| `peteches/repository.scm` | `repo-directory` / `source-path` — resolve repo assets via `%load-path` |
| `peteches/utils.scm` | **Legacy**, unused; `gather-manifest-packages` reads a `manifests/` dir that no longer exists |
| `peteches/deploy.scm` | **Legacy** `guix deploy` manifest, superseded by `machines.scm` — do not use |
| `peteches/machines.scm` | Named `machine` records + `%all-machines` list |
| `peteches/grafana-dashboards/` | Grafana dashboard JSON definitions |
| `configs/` | Non-Scheme assets (emacs, hypr, matugen, nyxt, wofi, alacritty, bin, claude, dms) referenced via `repo-directory` |
| `age-keys/` | SOPS age public keys, one per VM |
| `secrets/` | SOPS-encrypted secrets per host/group/shared — see `docs/secrets-management.org` |
| `infra/` | Terraform infrastructure-as-code for Proxmox VM provisioning |
| `ci/` | Concourse CI pipeline and task definitions |
| `containers/` | Container definitions |
| `scripts/deploy.scm` | `guix deploy` wrapper with `--hosts` filtering — **the supported deploy entry point** |
| `proxmox-vms.org` | VM inventory and IP reference table — **authoritative** for IPs |
| `docs/` | Architecture documentation |
| `.claude/skills/` | Claude Code automation skills (`update-channels`) |

### Complete File Map

#### `peteches/systems/`

| File | Purpose |
|---|---|
| `base.scm` | `make-base-os` — desktop/laptop base (Hyprland, greetd, libvirt) |
| `vm-base.scm` | `make-vm-os` — headless Proxmox VM base (SSH, nftables, node-exporter) |
| `common.scm` | Shared OS-level services and environment |
| `network-mounts.scm` | Network filesystem mount definitions |
| `monitored-hosts.scm` | Registry of Prometheus node-exporter scrape targets |
| `bootstrap.scm` | Minimal bootstrap OS for initial VM provisioning |
| `nug.scm` | nug desktop system |
| `nyarlothotep.scm` | nyarlothotep desktop system |
| `pihole.scm` | Pi-hole DNS/ad-blocking VM (192.168.51.189) |
| `prometheus.scm` | Prometheus monitoring VM (192.168.51.187, port 9090) |
| `grafana.scm` | Grafana dashboard VM (192.168.51.188, port 3000) |
| `loki.scm` | Loki log aggregation VM (192.168.51.190, port 3100) |
| `git.scm` | Git server VM — Gitolite SSH, cgit via Tailscale (192.168.51.191) |
| `jellyfin.scm` | Jellyfin media server VM (192.168.51.192, port 8096) |
| `caddy.scm` | Caddy reverse proxy VM (192.168.51.193, ports 80/443) |
| `prowlarr.scm` | Prowlarr indexer management VM (192.168.51.194, port 9696) |
| `arr.scm` | Arr media stack VM — Sonarr (:8989), Radarr (:7878) (192.168.51.195) |
| `downloads.scm` | Downloads VM — NZBGet (:6789), Transmission (:9091) (192.168.51.196) |
| `rustdesk.scm` | RustDesk remote desktop relay/rendezvous VM (192.168.51.197) |
| `concourse-db.scm` | Concourse CI database VM — PostgreSQL (192.168.51.198) |
| `concourse-web01.scm` | Concourse CI web frontend — ATC (:8080), TSA (:2222) (192.168.51.199) |
| `concourse-worker01.scm` | Concourse CI worker VM (192.168.51.200) |
| `vault.scm` | HashiCorp Vault secrets management VM (192.168.51.201, port 8200) — does not auto-unseal |
| `critical-grind-campaign.scm` | Critical Grind campaign system VM — Go/Gin app on :8080 + local PostgreSQL (192.168.51.202). Package and service come from the `critical-grind` channel |
| `critical-grind-outline.scm` | Outline wiki VM — Podman container + local PostgreSQL/Redis (192.168.51.203, :3000) |
| `plane.scm` | Plane project management VM — Podman containers + PostgreSQL/Redis/RabbitMQ (192.168.51.204, :80) |
| `nug-substitute-key.pub` | SSH public key for nug's local Guix substitute server |

#### `peteches/home/configs/`

Host-specific files only — each exports a complete `home-environment` record.

| File | Purpose |
|---|---|
| `nug.scm` | nug home environment — `(peteches home configs nug)` |
| `nyarlothotep.scm` | nyarlothotep home environment — `(peteches home configs nyarlothotep)` |

#### `peteches/home/modules/`

Shared fragments — imported by host configs and composed into `base-packages` / `base-services`.

| File | Purpose |
|---|---|
| `base.scm` | `(peteches home modules base)` — orchestrator; exports `base-packages` and `base-services` |
| `ssh.scm` | `(peteches home modules ssh)` — `base-ssh-service` with all SSH host entries |
| `gpg.scm` | `(peteches home modules gpg)` — `base-gpg-service` with pinentry dispatch script |
| `syncthing.scm` | `(peteches home modules syncthing)` — `base-syncthing-service` (org folder + devices) |
| `theming.scm` | `(peteches home modules theming)` — `base-theming-services` list (cursor, wallpaper, matugen, DMS plugin) |
| `ai.scm` | `(peteches home modules ai)` — `base-ai-service`: ECA config **only**. Claude Code's MCP servers live in `claude.scm`. Its anvil path is stale/broken |
| `claude.scm` | `(peteches home modules claude)` — defines `home-claude-service-type`: symlinks `configs/claude/defaults` into `~/.claude/` and registers MCP servers via `claude mcp add` |
| `mako.scm` | `(peteches home modules mako)` — `base-mako-config` and `base-mako-service` |
| `firefox.scm` | `(peteches home modules firefox)` — Firefox profiles. `base-firefox-global-prefs`/`-extensions` are **unused**, so uBlock/DarkReader/PassFF are not installed |
| `git.scm` | `(peteches home modules git)` — `peteches-gpg-for-git` package and `git-config` |
| `scoreplay.scm` | `(peteches home modules scoreplay)` — `%scoreplay-ssh-hosts`. **Unused**: `ssh.scm` never splices it in, so these hosts are absent from `~/.ssh/config` |
| `mpv.scm` | `(peteches home modules mpv)` — `%mpv-profiles` |
| `ssh-authorized-keys` | SSH authorized keys (co-located with `ssh.scm` for `local-file`) |
| `firefox-extensions/` | Firefox .xpi extensions (uBlock, DarkReader, PassFF, AWS SSO) |
| `git-hooks/` | Git hook scripts (pre-commit) |
| `git-ignore.txt` | Git global ignore patterns |

#### `peteches/monitoring/`

| File | Purpose |
|---|---|
| `loki.scm` | `loki-event-gexp` — **dead code**: not exported and never called. Routine log shipping is done by the Alloy service on each VM, not this |

#### `peteches/channels/`

Four files carrying duplicated pinned commits, kept in sync **by hand**.
Prefer the `/update-channels` skill over editing pins directly.

| File | Purpose |
|---|---|
| `base.scm` | `%base-channels` — the reference. Module. Pinned: sops-guix, guix-science, guix-science-nonfree, nonguix, guix, peteches, critical-grind |
| `nug.scm` | Module exporting `%nug-channels` = `%base-channels` + guix-hpc-non-free |
| `manual.scm` | **Full** plain channels list (all 7) for `guix pull -C` / symlinking to `~/.config/guix/channels.scm` |
| `channels-nug.scm` | Plain list with **only** the `peteches` channel — *not* a mirror of the above, despite the name. Pulling with it leaves guix unpinned |

#### Other notable files

| File | Purpose |
|---|---|
| `peteches/machines.scm` | Named `machine` records + `%all-machines` list |
| `peteches/repository.scm` | `repo-directory` / `source-path` — resolve repo assets through `%load-path` |
| `peteches/utils.scm` | **Legacy/unused**: `gather-manifest-packages` (reads a nonexistent `manifests/` dir, hard-codes an absolute path), `apply-template-file` |
| `peteches/deploy.scm` | **Legacy** `guix deploy` manifest listing only 5 of 17 machines. Superseded by `machines.scm` + `scripts/deploy.scm`. `docs/backups.org` and `proxmox-vms.org` still reference it — that guidance is stale |
| `peteches/packages/desktop-scripts.scm` | `peteches-desktop-scripts` — packages `configs/bin/`. Scripts must be listed explicitly in `#:install-plan` |
| `scripts/deploy.scm` | `guix deploy` wrapper — parses `--hosts` patterns, filters `%all-machines`, passes result via `-e`. Keeps its own `%machine-names` alist that must be updated alongside `machines.scm` |
| `scripts/sync-restic-keys.sh` | Syncs restic backup keys to all VMs |
| `proxmox-vms.org` | VM inventory: IPs, VMIDs, purpose — authoritative IP reference |
| `containers/claude.scm` | `claude-container` (runs claude-code in `guix shell --container`), `emacs-anvil`, `comfyui-mcp` packages |
| `containers/guix-builder.scm` | Docker image for Concourse CI tasks — `registry.ts.peteches.co.uk/guix-builder`, referenced by `ci/tasks/*.yml` |
| `containers/postgres.scm` | Throwaway PostgreSQL test container — not deployed, nothing imports it |
| `peteches/grafana-dashboards/node-exporter.json` | Node Exporter Grafana dashboard |
| `peteches/grafana-dashboards/pihole.json` | Pi-hole Grafana dashboard |
| `peteches/grafana-dashboards/proxmox.json` | Proxmox Grafana dashboard |
| `peteches/grafana-dashboards/synology-details.json` | Synology NAS detailed metrics dashboard |
| `peteches/grafana-dashboards/synology-overview.json` | Synology NAS overview dashboard |
| `docs/backups.org` | Backup strategy documentation |
| `docs/secrets-management.org` | SOPS + age keys workflow |
| `docs/infrastructure.org` | Terraform + Concourse CI overview |
| `.claude/skills/update-channels/` | Claude Code skill for updating pinned channel commits across all four channel files |

### System Configurations (`peteches/systems/`)

Two base constructors exist for different machine classes:

**`base.scm`** exports `make-base-os` — desktop/laptop systems with Hyprland, greetd, libvirt, virbr0, Tor, fingerprint, etc. Used by `nug.scm`, `nyarlothotep.scm`.

Key `make-base-os` flags:
- `laptop?` — enables TLP and thermald
- `intel-cpu?` (default `#t`) — adds intel-microcode and IOMMU kernel args
- `with-nvidia?` — adds NVIDIA driver, firmware, kernel modules, Wayland env vars
- `with-docker?` — enables containerd + Docker services
- `with-bluetooth?`, `with-printing?` — optional services
- `with-nonguix?` — registers nonguix substitute server

`without-gdm` strips GDM from `%desktop-services` and configures the local Guix substitute server (`nug.peteches.co.uk:3000`). All systems use gtkgreet inside cage as the greeter, launching a Hyprland session.

**`vm-base.scm`** exports `make-vm-os` — headless Proxmox QEMU/KVM VMs. No desktop services. Starts from `%base-services`, adds SSH, networking, NTP, nftables firewall, QEMU guest agent, and Prometheus node-exporter.

Key `make-vm-os` parameters:
- `host-name`, `bootloader`, `file-systems` — required
- `kernel` (default `linux-libre`), `firmware`, `mapped-devices` — optional
- `ipv4-address` — set static IP; omit to use dhcpcd
- `ipv6-address` — set static IPv6
- `nameservers` — override DNS resolvers
- `users-extra`, `extra-services`, `extra-packages` — composition points
- `restic-config` — attach restic backup service
- `sops-secrets` — list of SOPS secret files to decrypt at boot
- `with-nonguix?` — registers nonguix substitute server
- `with-nvidia?` — nonguix NVIDIA driver + CUDA (used by `jellyfin.scm` for NVENC)
- `with-nug-offload?` (default `#t`) — enable build offload to nug. **Requires both** a `guix-offload-key` sops-secret on the VM *and* its `guix-offload` public key in `nug.scm`'s authorized-keys. Several VMs have only one half; offload then fails and falls back to local builds

VM system files should `(define-public <name>-os ...)` and end with `<name>-os` as the final expression so both `guix system build FILE` and `guix deploy` (via module import) work.

All VMs use `/dev/vda` as the root disk. The LAN is a **/23** (`192.168.50.0/23`), so VM addresses are written `192.168.51.x/23` — the gateway is `192.168.50.1`. See `proxmox-vms.org` for IPs and VMIDs, and `docs/infrastructure.org` for the Terraform provisioning workflow.

The base firewall (`%vm-base-firewall`) has a **drop** input policy and opens only ssh (22), node-exporter (9100) and ICMP. A service that starts but is unreachable is usually a missing `simple-service … firewall-service-type` extension — see `git.scm` or `rustdesk.scm` for the pattern.

### Home Configurations (`peteches/home/`)

`peteches/home/modules/base.scm` is the orchestrator: it exports `base-packages`
and `base-services`, composing feature modules from the external `peteches` guix
channel (imported as `(peteches home services ...)`) — emacs, git, hyprland, aws,
nyxt, wofi, and more. The focused modules alongside it (`ssh.scm`, `gpg.scm`,
`theming.scm`, `mako.scm`, `ai.scm`, `claude.scm`, …) supply configuration values.

`peteches/home/configs/nug.scm` and `nyarlothotep.scm` append host-specific
extras and each evaluate to a bare `home-environment` record.

Non-Scheme assets under `configs/` are located via `repo-directory` /
`source-path` from `(peteches repository)`, which searches `%load-path` —
never by relative path, which would break under `-L .` and in worktrees.

### Channels (`peteches/channels/`)

`base.scm` exports `%base-channels` — pinned: `sops-guix`, `guix-science`,
`guix-science-nonfree`, `nonguix`, `guix` itself, the `peteches` channel
(which provides the custom packages, home services and system services this repo
consumes), and `critical-grind`. `nug.scm` adds `guix-hpc-non-free` on top.

`critical-grind` is the Critical Grind application repository, which is itself
a channel supplying `(critical-grind packages campaign)` and
`(critical-grind services campaign)`. It has **no channel introduction**, so
commits are not signature-verified and every pull warns. Releasing a new
version of the app is a commit bump in the four channel files — there is
nothing to build by hand.

It is fetched over **smart HTTP** (`git.ts.peteches.co.uk/git/…`, served by
`git-http-backend` on the git VM), not over gitolite's SSH. That is not a
stylistic choice: `guix/git.scm` authenticates git fetches with
`(%make-auth-ssh-agent)` and nothing else — it never reads `~/.ssh/config` or a
key file — so an `ssh://` channel URL requires the key loaded in an agent on
**every** machine that pulls, CI containers included, and fails with a
`remote rejected authentication` message that names neither ssh-agent nor the
key. Repositories must carry `git-daemon-export-ok` to be fetchable this way;
see `peteches/systems/git.scm`.

**Pins are duplicated across four files** (`base.scm`, `nug.scm`, `manual.scm`,
`channels-nug.scm`) with nothing enforcing agreement. Update them together —
use the `/update-channels` skill.

For `guix pull -C` or symlinking to `~/.config/guix/channels.scm`, use
**`manual.scm`** (the full plain list). `channels-nug.scm` holds only the
`peteches` channel despite its name.

### Adding a New VM

When creating a new VM system config (`peteches/systems/<name>.scm`), always update these files in the same change:

1. **`peteches/systems/pihole.scm`** — add a `pihole-custom-host` entry to the `custom-hosts` list mapping the VM's IP to its hostname (`<name>.peteches.co.uk`).

2. **`peteches/home/modules/ssh.scm`** — add an `openssh-host` entry inside the `home-openssh-service-type` hosts list in `base-ssh-service`. At minimum a direct LAN entry:
   ```scheme
   (openssh-host
    (name "<name>")
    (host-name "<ip-address>")
    (user "peteches")
    (identity-file "~/.ssh/id_ed25519"))
   ```
   If the VM runs Tailscale, also add a `.ts` alias entry:
   ```scheme
   (openssh-host
    (name "<name>.ts")
    (host-name "<name>.spaniel-cordylus.ts.net"))
   ```

3. **`peteches/machines.scm`** — add a `define-public <name>-machine` entry and include it in `%all-machines`. The SSH `host-key` can only be filled in after the VM's first boot (`ssh-keyscan <ip>`); use a `TODO` placeholder until then.

4. **`scripts/deploy.scm`** — add the machine to the `%machine-names` alist. It maps machine records back to variable names for the `-e` expression; omitting it makes `--hosts` filtering fail with `Unknown machine`.

5. **`peteches/systems/monitored-hosts.scm`** — add the VM's node-exporter endpoint so Prometheus scrapes it automatically. The `node` scrape job in prometheus.scm reads this list dynamically; no change to prometheus.scm is needed for node-exporter coverage.

6. **`peteches/systems/prometheus.scm`** — two separate things:
   - Add the VM's Alloy endpoint (`<ip>:12345`) to the hand-written `alloy` job. This is **not** generated from `monitored-hosts.scm` and is the usual omission.
   - If the VM's service exposes metrics on a service-specific port (beyond node-exporter's 9100), add a dedicated `prometheus-scrape-config`. Examples: Loki on `:3100`, Grafana on `:3000`.

7. **`proxmox-vms.org`** — add a row to the VM table in the Overview section.

8. **`infra/terraform/main.tf`** — add a `module "<name>"` block using the `proxmox-vm` module. See `docs/infrastructure.org` for the full provisioning workflow (Terraform creates the VM; Guix deploys the OS).

9. **`age-keys/<name>.pub`** — after first boot, retrieve the VM's age public key and commit it. Add the corresponding entry to `.sops.yaml`. See `docs/secrets-management.org`.

10. **Build offload** — `make-vm-os` enables it by default. Either add a `guix-offload-key` sops-secret (from `secrets/hosts/<name>/guix-build.yaml`) **and** the VM's `guix-offload` public key to `nug.scm`'s authorized-keys, or pass `#:with-nug-offload? #f`. Half-wiring it fails silently.

Pick a free IPv6 suffix too, if the VM needs one. The `2a10:d582:ef59::`
addresses are assigned by hand and tracked **only** in the `#:ipv6-address`
lines of `peteches/systems/*.scm` — `proxmox-vms.org` records IPv4 only, so
nothing checks for collisions. Check the current allocation first:

```bash
command grep -rn '#:ipv6-address' peteches/systems/
```

### Utilities (`peteches/utils.scm`) — legacy

Both exports are unreferenced and should be treated as dead code:

- `gather-manifest-packages` — reads manifest `.scm` files from a `manifests/` directory that **no longer exists**, and hard-codes the absolute path `/home/peteches/area_51/guix`, so it cannot work from a worktree.
- `apply-template-file` — substitutes `${KEY}` placeholders from an alist. Unused.

For resolving repo-relative paths, use `(peteches repository)` instead.

## Editing Lisp / Scheme / Guile Code

This repository uses the **Anvil** MCP server (`anvil` in the MCP server list) which bridges Claude Code to a live Emacs instance. Emacs has `paredit-mode` active for all `.el`, `.scm`, and `.lisp` buffers.

**Always prefer Anvil for structural edits** — raw text substitution of parenthetical code is error-prone. Use the `emacs-eval` tool instead:

### Workflow for editing a Scheme/Guile/Elisp file

1. **Visit the file** in Emacs:
   ```elisp
   (find-file "/path/to/file.scm")
   ```

2. **Navigate to the target form** using search or `re-search-forward`, then use paredit commands to restructure safely:
   - `(paredit-wrap-round)` — wrap sexp in parens
   - `(paredit-forward-slurp-sexp)` — slurp next sibling into current list
   - `(paredit-forward-barf-sexp)` — barf last child out of current list
   - `(paredit-splice-sexp)` — splice (unwrap) current list
   - `(paredit-kill)` — kill sexp at point
   - `(kill-sexp)` / `(forward-sexp N)` — navigate/kill whole sexps

3. **Make inline edits** via `(insert ...)`, `(delete-region ...)`, or `(replace-string ...)` within the buffer context.

4. **Verify balance** after every significant change:
   ```elisp
   (with-current-buffer (get-file-buffer "/path/to/file.scm")
     (check-parens))
   ```
   `check-parens` signals an error if parens are unbalanced — treat any error as a blocker.

5. **Save** when clean:
   ```elisp
   (with-current-buffer (get-file-buffer "/path/to/file.scm")
     (save-buffer))
   ```

### When Anvil is unavailable

Only fall back to `Edit`/`Write` tools if:
- The `anvil` MCP server shows as disconnected, **and**
- Emacs cannot be reached via `emacsclient`

In that case, make the smallest possible textual change and double-check paren balance manually.
