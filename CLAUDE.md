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

# Pull channels (updates all pinned channels including peteches channel)
guix pull -C peteches/channels/channels-nug.scm

# Deploy to all VMs (or filter with --hosts)
scripts/deploy.scm
scripts/deploy.scm --hosts 192.168.51.187          # single VM by IP
scripts/deploy.scm -h "host-name=prometheus"       # by hostname pattern
scripts/deploy.scm -h "prometheus,loki" --dry-run  # multiple patterns

# Check channels
guix describe
```

To validate that a module loads without errors, load it in guile:
```bash
guile -L . -c '(use-modules (peteches systems vm-base))'
```

## Architecture

### Directory Layout

| Path | Purpose |
|---|---|
| `peteches/systems/` | OS configurations per host |
| `peteches/home-configs/` | Home environment configs per host/feature |
| `peteches/monitoring/` | Guix gexp helpers for monitoring (e.g. Loki event sender) |
| `peteches/channels/` | Channel lock files |
| `peteches/utils.scm` | Shared utilities |
| `peteches/machines.scm` | Named `machine` records + `%all-machines` list |
| `peteches/grafana-dashboards/` | Grafana dashboard JSON definitions |
| `age-keys/` | SOPS age public keys, one per VM |
| `secrets/` | SOPS-encrypted secrets per host/group/shared — see `docs/secrets-management.org` |
| `infra/` | Terraform infrastructure-as-code for Proxmox VM provisioning |
| `ci/` | Concourse CI pipeline and task definitions |
| `containers/` | Container definitions |
| `scripts/deploy.scm` | `guix deploy` wrapper with `--hosts` filtering |
| `channels.scm` | Plain channels list for `guix pull -C` (mirrors `channels-nug.scm`) |
| `proxmox-vms.org` | VM inventory and IP reference table |
| `docs/` | Architecture documentation |
| `skills/` | Claude Code automation skills |

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
| `vault.scm` | HashiCorp Vault secrets management VM (192.168.51.201, port 8200) |
| `nug-substitute-key.pub` | SSH public key for nug's local Guix substitute server |

#### `peteches/home-configs/`

| File | Purpose |
|---|---|
| `base.scm` | Main home environment — composes all feature home-services |
| `nug.scm` | nug-specific home additions |
| `nyarlothotep.scm` | nyarlothotep-specific home additions |
| `scoreplay.scm` | scoreplay-specific home additions |
| `mako.scm` | Mako notification daemon config fragment |
| `firefox.scm` | Firefox config fragment |
| `hyprland.scm` | Hyprland compositor config fragment |
| `git.scm` | Git config fragment |
| `waybar.scm` | Waybar status bar config fragment |
| `mpv.scm` | MPV video player config (upscaler, YouTube, low-power profiles) |
| `ssh-authorized-keys` | SSH authorized keys for the home environment |
| `firefox-extensions/` | Firefox .xpi extensions (uBlock, DarkReader, PassFF, AWS SSO) |
| `git-hooks/` | Git hook scripts (pre-commit) |

#### `peteches/monitoring/`

| File | Purpose |
|---|---|
| `loki.scm` | `loki-event-gexp` — gexp helper for sending log events to Loki from G-expressions |

#### `peteches/channels/`

| File | Purpose |
|---|---|
| `base.scm` | `%base-channels` — pinned: sops-guix, guix-science, nonguix, simendsjo, peteches |
| `nug.scm` | Guile module exporting `%nug-channels` (extends `%base-channels`) |
| `channels-nug.scm` | Plain channels list for `guix pull -C` — no `define-module`, safe to symlink to `~/.config/guix/channels.scm` |

#### Other notable files

| File | Purpose |
|---|---|
| `peteches/machines.scm` | Named `machine` records + `%all-machines` list |
| `peteches/utils.scm` | `gather-manifest-packages`, `apply-template-file` |
| `scripts/deploy.scm` | `guix deploy` wrapper — parses `--hosts` patterns, filters `%all-machines`, passes result via `-e` |
| `scripts/sync-restic-keys.sh` | Syncs restic backup keys to all VMs |
| `proxmox-vms.org` | VM inventory: IPs, VMIDs, purpose — authoritative IP reference |
| `channels.scm` | Root-level plain channels list (mirrors `peteches/channels/channels-nug.scm`) |
| `containers/postgres.scm` | PostgreSQL container definition |
| `peteches/grafana-dashboards/node-exporter.json` | Node Exporter Grafana dashboard |
| `peteches/grafana-dashboards/pihole.json` | Pi-hole Grafana dashboard |
| `peteches/grafana-dashboards/proxmox.json` | Proxmox Grafana dashboard |
| `peteches/grafana-dashboards/synology-details.json` | Synology NAS detailed metrics dashboard |
| `peteches/grafana-dashboards/synology-overview.json` | Synology NAS overview dashboard |
| `docs/backups.org` | Backup strategy documentation |
| `docs/secrets-management.org` | SOPS + age keys workflow |
| `docs/infrastructure.org` | Terraform + Concourse CI overview |
| `skills/guix-update/` | Claude Code skill for updating pinned channel commits |

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
- `with-nug-offload?` — enable build offload to nug

VM system files should `(define-public <name>-os ...)` and end with `<name>-os` as the final expression so both `guix system build FILE` and `guix deploy` (via module import) work.

All VMs use `/dev/vda` as the root disk. See `proxmox-vms.org` for IPs and VMIDs, and `docs/infrastructure.org` for the Terraform provisioning workflow.

### Home Configurations (`peteches/home-configs/`)

`base.scm` is the main home environment. It composes feature modules from the external `peteches` guix channel (imported as `(peteches home services ...)`) — covering emacs, git, hyprland, waybar, aws, ai tools, and more. Host-specific config fragments (mako.scm, firefox.scm, hyprland.scm, etc.) remain in this directory and are composed into host-specific configs like `nug.scm`.

### Channels (`peteches/channels/`)

`base.scm` exports `%base-channels` — a pinned list including: `sops-guix`, `guix-science`, `nonguix`, `simendsjo`, and the `peteches` channel (which provides custom packages, home services, and system services). Update commits here to upgrade channels.

Use `channels-nug.scm` (plain list, no `define-module`) with `guix pull -C` or symlink it to `~/.config/guix/channels.scm`.

### Adding a New VM

When creating a new VM system config (`peteches/systems/<name>.scm`), always update these files in the same change:

1. **`peteches/systems/pihole.scm`** — add a `pihole-custom-host` entry to the `custom-hosts` list mapping the VM's IP to its hostname (`<name>.peteches.co.uk`).

2. **`peteches/home-configs/base.scm`** — add an `openssh-host` entry inside the `home-openssh-service-type` hosts list. At minimum a direct LAN entry:
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

4. **`peteches/systems/monitored-hosts.scm`** — add the VM's node-exporter endpoint so Prometheus scrapes it automatically. The `node` scrape job in prometheus.scm reads this list dynamically; no change to prometheus.scm is needed for node-exporter coverage.

5. **`peteches/systems/prometheus.scm`** — if the VM's service exposes Prometheus metrics on a service-specific port (beyond node-exporter's 9100), add a dedicated `prometheus-scrape-config` entry. Examples: Loki on `:3100`, Grafana on `:3000`. Node-exporter coverage is already automatic via `monitored-hosts.scm`.

6. **`proxmox-vms.org`** — add a row to the VM table in the Overview section.

7. **`infra/terraform/main.tf`** — add a `module "<name>"` block using the `proxmox-vm` module. See `docs/infrastructure.org` for the full provisioning workflow (Terraform creates the VM; Guix deploys the OS).

8. **`age-keys/<name>.pub`** — after first boot, retrieve the VM's age public key and commit it. Add the corresponding entry to `.sops.yaml`. See `docs/secrets-management.org`.

### Utilities (`peteches/utils.scm`)

- `gather-manifest-packages` — reads manifest `.scm` files from `manifests/` directory and converts them to package+output pairs for inclusion in home/system package lists.
- `apply-template-file` — reads a file and substitutes `${KEY}` placeholders from an alist; used for generating config files from templates.

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
