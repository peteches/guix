# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

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

# Deploy to a running VM over SSH
guix deploy -L . peteches/deploy.scm

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
| `containers/` | Container definitions |

### System Configurations (`peteches/systems/`)

Two base constructors exist for different machine classes:

**`base.scm`** exports `make-base-os` — desktop/laptop systems with Hyprland, greetd, libvirt, virbr0, Tor, fingerprint, etc. Used by `azathoth.peteches.co.uk.scm`, `bhiyaki.peteches.co.uk.scm`, `nug.scm`, `nyarlothotep.scm`.

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

Current VM configs: `prometheus.scm` — Prometheus monitoring server on `/dev/vda`, port 9090.

### Home Configurations (`peteches/home-configs/`)

`base.scm` is the main home environment; it composes feature modules from `peteches/home-services/` (e.g., emacs, git, hyprland, waybar, aws, ai tools) and system-specific fragments. Host-specific configs typically `#:use-module` base plus add machine-local services/packages.

### Channels (`peteches/channels/`)

`base.scm` exports `%base-channels` — a pinned list of: `guix` (codeberg), `nonguix` (gitlab), `guix-science`, and `simendsjo`. Update commits here to upgrade channels.

### Custom Packages (`peteches/packages/`)

Packages not in the upstream Guix channels. Most use `copy-build-system` for pre-built binaries (see `go-tools.scm` for the pattern). Notable entries:

- `prometheus.scm` — Prometheus 3.11.3, pre-built linux-amd64 binary; installs `prometheus`, `promtool`, and the `consoles/`/`console_libraries/` assets.

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

3. **`peteches/deploy.scm`** — add a `machine` entry. The SSH `host-key` can only be filled in after the VM's first boot (`ssh-keyscan <ip>`); use a `TODO` placeholder until then.

4. **`peteches/systems/monitored-hosts.scm`** — add the VM's node-exporter endpoint so Prometheus scrapes it automatically. The `node` scrape job in prometheus.scm reads this list dynamically; no change to prometheus.scm is needed for node-exporter coverage.

5. **`peteches/systems/prometheus.scm`** — if the VM's service exposes Prometheus metrics on a service-specific port (beyond node-exporter's 9100), add a dedicated `prometheus-scrape-config` entry. Examples: Loki on `:3100`, Grafana on `:3000`. Node-exporter coverage is already automatic via `monitored-hosts.scm`.

6. **`proxmox-vms.org`** — add a row to the VM table in the Overview section.

### Utilities (`peteches/utils.scm`)

- `gather-manifest-packages` — reads manifest `.scm` files from `manifests/` directory and converts them to package+output pairs for inclusion in home/system package lists.
- `apply-template-file` — reads a file and substitutes `${KEY}` placeholders from an alist; used for generating config files from templates.
