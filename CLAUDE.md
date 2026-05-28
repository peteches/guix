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

A personal [GNU Guix](https://guix.gnu.org/) system configuration repository. All files are written in Guile Scheme. It defines operating systems, home environments, custom packages, and system services for multiple machines. The root Guile module namespace is `(peteches ...)`.

## Common Commands

All `guix` commands that reference local modules need `-L .` (or `--load-path=.`) to be run from the repo root.

```bash
# Build/validate a custom package
guix build -L . <package-name>
guix build -L . --dry-run <package-name>

# Lint a package module
guix lint -L . -m peteches/packages/<file>.scm

# Validate a system config (dry-run)
guix system build -L . --dry-run peteches/systems/<host>.scm

# Build a QCOW2 disk image for a VM
guix system image -L . -t qcow2 peteches/systems/<vm>.scm

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
guile -L . -c '(use-modules (peteches packages fonts))'
```

## Architecture

### Directory Layout

| Path | Purpose |
|---|---|
| `peteches/systems/` | OS configurations per host |
| `peteches/home-configs/` | Home environment configs per host/feature |
| `peteches/home-services/` | Reusable home service definitions |
| `peteches/packages/` | Custom package definitions |
| `peteches/system-services/` | Custom OS-level service definitions |
| `peteches/channels/` | Channel lock files |
| `peteches/utils.scm` | Shared utilities |
| `peteches/machines.scm` | Named `machine` records + `%all-machines` list |
| `peteches/grafana-dashboards/` | Grafana dashboard JSON definitions |
| `containers/` | Container definitions |
| `scripts/deploy.scm` | `guix deploy` wrapper with `--hosts` filtering |
| `proxmox-vms.org` | VM inventory and IP reference table |
| `docs/` | Architecture documentation |
| `skills/` | Claude Code automation skills |

### Complete File Map

#### `peteches/systems/`

| File | Purpose |
|---|---|
| `base.scm` | `make-base-os` — desktop/laptop base (Hyprland, greetd, libvirt) |
| `vm-base.scm` | `make-vm-os` — headless Proxmox VM base (SSH, dhcpcd, nftables) |
| `common.scm` | Shared OS-level services and environment |
| `network-mounts.scm` | Network filesystem mount definitions |
| `monitored-hosts.scm` | Registry of Prometheus node-exporter scrape targets |
| `pihole.scm` | Pi-hole DNS/ad-blocking VM (192.168.51.189) |
| `prometheus.scm` | Prometheus monitoring VM (192.168.51.187, port 9090) |
| `grafana.scm` | Grafana dashboard VM (192.168.51.188, port 3000) |
| `loki.scm` | Loki log aggregation VM (192.168.51.190, port 3100) |
| `nug.scm` | nug desktop system |
| `nyarlothotep.scm` | nyarlothotep desktop system |

#### `peteches/home-configs/`

| File | Purpose |
|---|---|
| `base.scm` | Main home environment — composes all feature home-services |
| `nug.scm` | nug-specific home additions |
| `nyarlothotep.scm` | nyarlothotep-specific home additions |
| `scoreplay.scm` | scoreplay-specific home additions |
| `firefox-extensions/` | Firefox .xpi extensions (uBlock, DarkReader, PassFF, AWS SSO) |
| `git-hooks/` | Git hook scripts (pre-commit) |

#### `peteches/home-services/`

| File | Purpose |
|---|---|
| `git.scm` | Git config service (gitignore, git-section helpers) |
| `firefox.scm` | Firefox home service with profile launcher |
| `hyprland.scm` | Hyprland Wayland compositor home service |
| `waybar.scm` | Waybar status bar home service |
| `mako.scm` | Mako notification daemon home service |
| `desktop.scm` | Desktop environment home service |
| `agixt.scm` | AGiXT AI framework home service |
| `ai.scm` | AI tools home service |
| `aws.scm` | AWS CLI home service |
| `ezlocalai.scm` | EZLocalAI LLM service |
| `koboldcpp.scm` | KoboldCPP language model service |
| `password-store.scm` | pass password manager home service |
| `nyxt.scm` | Nyxt browser home service |
| `emacs/` | Emacs module: `base.scm`, `exwm.scm`, `configs/` (init.el, early-init.el, etc.) |
| `scripts/` | AI, comfyui, and other helper scripts |
| `waybar-scripts/` | Waybar module scripts |
| `agixt-scripts/` | AGiXT launcher scripts |

#### `peteches/packages/`

Pattern: use `copy-build-system` for pre-built binaries (see `go-tools.scm`).

| File | Purpose |
|---|---|
| `prometheus.scm` | Prometheus pre-built binary |
| `grafana.scm` | Grafana dashboard server |
| `loki.scm` | Grafana Loki log aggregation |
| `alloy.scm` | Grafana Alloy (CGO Go binary, glibc wrapper) |
| `pihole.scm` | Pi-hole FTL DNS binary |
| `pihole-exporter.scm` | Pi-hole Prometheus exporter |
| `tailscale.scm` | Tailscale VPN |
| `rclone.scm` | Rclone cloud storage tool |
| `mermaid.scm` | Mermaid diagram generator |
| `go-tools.scm` | Go tooling packages |
| `go-deps.scm` | Go dependency packages |
| `go-enum.scm` | Go enum code generation |
| `aws.scm` | AWS CLI and tools |
| `emacs.scm` | Emacs package variants |
| `hyprland.scm` | Hyprland Wayland compositor |
| `nvidia-container-runtime.scm` | NVIDIA container runtime |
| `koboldcpp.scm` | KoboldCPP LLM runner |
| `fonts.scm` | Font collection |
| `gpg.scm` | GPG/GnuPG tools |
| `aider.scm` | Aider AI pair programming tool |
| `claude-code.scm` | Claude Code CLI |
| `mcp.scm` | MCP-related packages |
| `terraform.scm` | Terraform |
| `dank-material-shell.scm` | Material Shell theme |
| `gurps.scm` | GURPS game system tools |
| `lycheeslicer.scm` | LycheeSlice 3D printing slicer |
| `scripts.scm` | Helper scripts package |
| `shims.scm` | Shell shim utilities |

#### `peteches/system-services/`

| File | Purpose |
|---|---|
| `prometheus.scm` | Prometheus service type (YAML config generation) |
| `grafana.scm` | Grafana service type |
| `loki.scm` | Loki service type (schema config generation) |
| `pihole.scm` | Pi-hole service type (FTL process management) |
| `alloy.scm` | Grafana Alloy service type |
| `tailscale.scm` | Tailscale service type (multi-instance, per-instance Linux netns) |
| `firewall.scm` | Modular nftables firewall service (authoritative ruleset, extension hooks) |
| `restic.scm` | Restic VM backup service (SFTP to NAS) |
| `boltd.scm` | Boltd (Thunderbolt secure enclave) service |

#### `peteches/channels/`

| File | Purpose |
|---|---|
| `base.scm` | `%base-channels` — pinned guix, nonguix, guix-science, simendsjo |
| `nug.scm` | nug-specific channel overrides |

#### Other notable files

| File | Purpose |
|---|---|
| `peteches/machines.scm` | Named `machine` records + `%all-machines` list |
| `scripts/deploy.scm` | `guix deploy` wrapper — parses `--hosts` patterns, filters `%all-machines`, passes result via `-e` |
| `proxmox-vms.org` | VM inventory: IPs, VMIDs, purpose — authoritative IP reference |
| `containers/postgres.scm` | PostgreSQL container definition |
| `peteches/grafana-dashboards/node-exporter.json` | Node Exporter Grafana dashboard |
| `peteches/grafana-dashboards/pihole.json` | Pi-hole Grafana dashboard |
| `peteches/grafana-dashboards/proxmox.json` | Proxmox Grafana dashboard |
| `docs/backups.org` | Backup strategy documentation |
| `skills/guix-update/` | Claude Code skill for channel/package updates |

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

**`vm-base.scm`** exports `make-vm-os` — headless Proxmox QEMU/KVM VMs. No desktop services. Starts from `%base-services`, adds SSH, dhcpcd, NTP, and a minimal SSH-only nftables firewall. Uses `linux-libre` (no nonguix needed).

Key `make-vm-os` parameters:
- `host-name`, `bootloader`, `file-systems` — required
- `kernel` (default `linux-libre`), `firmware`, `mapped-devices` — optional
- `users-extra`, `extra-services`, `extra-packages` — composition points
- `with-nonguix?` — registers nonguix substitute server

VM system files should `(define-public <name>-os ...)` and end with `<name>-os` as the final expression so both `guix system build FILE` and `guix deploy` (via module import) work.

Current VM configs: `prometheus.scm` (port 9090), `grafana.scm` (port 3000), `loki.scm` (port 3100), `pihole.scm` (DNS). All use `/dev/vda` as the root disk. See `proxmox-vms.org` for IPs and VMIDs.

### Home Configurations (`peteches/home-configs/`)

`base.scm` is the main home environment; it composes feature modules from `peteches/home-services/` (e.g., emacs, git, hyprland, waybar, aws, ai tools) and system-specific fragments. Host-specific configs typically `#:use-module` base plus add machine-local services/packages.

### Channels (`peteches/channels/`)

`base.scm` exports `%base-channels` — a pinned list of: `guix` (codeberg), `nonguix` (gitlab), `guix-science`, and `simendsjo`. Update commits here to upgrade channels.

### Custom Packages (`peteches/packages/`)

Packages not in the upstream Guix channels. Most use `copy-build-system` for pre-built binaries (see `go-tools.scm` for the pattern). See the Complete File Map above for the full per-file listing.

Notable entry: `prometheus.scm` installs `prometheus`, `promtool`, and the `consoles/`/`console_libraries/` assets from a pre-built linux-amd64 binary.

### Custom System Services (`peteches/system-services/`)

Custom Guix service types for the VM fleet. Each pairs with a same-named package in `peteches/packages/`. See the Complete File Map above for the full listing.

Key patterns:
- Services generate YAML/JSON config files via `apply-template-file` or inline G-expressions.
- `firewall.scm` is authoritative: it replaces the default `%base-services` nftables setup and accepts extension hooks from other services.
- `tailscale.scm` supports multiple instances, each isolated in its own Linux network namespace.

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
    (host-name "<name>.tailb21dfe.ts.net"))
   ```

3. **`peteches/machines.scm`** — add a `define-public <name>-machine` entry and include it in `%all-machines`. The SSH `host-key` can only be filled in after the VM's first boot (`ssh-keyscan <ip>`); use a `TODO` placeholder until then.

4. **`peteches/systems/monitored-hosts.scm`** — add the VM's node-exporter endpoint so Prometheus scrapes it automatically. The `node` scrape job in prometheus.scm reads this list dynamically; no change to prometheus.scm is needed for node-exporter coverage.

5. **`peteches/systems/prometheus.scm`** — if the VM's service exposes Prometheus metrics on a service-specific port (beyond node-exporter's 9100), add a dedicated `prometheus-scrape-config` entry. Examples: Loki on `:3100`, Grafana on `:3000`. Node-exporter coverage is already automatic via `monitored-hosts.scm`.

6. **`proxmox-vms.org`** — add a row to the VM table in the Overview section.

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
