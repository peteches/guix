# peteches Guix System Configurations

Personal [GNU Guix](https://guix.gnu.org/) system-configuration repository, written in
Guile Scheme under the root module namespace `(peteches ...)`. It defines the operating
systems for ~21 headless Proxmox VMs (monitoring stack, media stack, CI, reverse proxy,
Claude Code workstation, GPU workloads) and 2 desktops (Hyprland / gtkgreet), plus
per-host `guix home` environments.

> **Start here:** read [`peteches/systems/vm-base.scm`](peteches/systems/vm-base.scm)
> first. Its header documents the house style (bare final expression), every
> `make-vm-os` keyword, the VM baseline, and the firewall / offload / age-key
> conventions. Every other system config is a thin instance of it. Then
> [`peteches/machines.scm`](peteches/machines.scm) + [`scripts/deploy.scm`](scripts/deploy.scm)
> for the deploy path, and [`peteches/channels/base.scm`](peteches/channels/base.scm) for
> the channel model.

---

## Repository layout

| Path | Purpose |
|---|---|
| `peteches/systems/` | Per-host OS configs (~30 files) + the two base constructors + shared system modules |
| `peteches/home/configs/` | Host-specific `home-environment` records (dagon, nyarlothotep, 3 claude-workstation account configs) |
| `peteches/home/modules/` | Home config *values* + shared constructors (`base.scm` orchestrator, ssh, gpg, claude, pi, theming, …) |
| `peteches/home/services/` | Reusable home service *types* (aws, desktop, emacs, firefox, git, hyprland, mako, mpv, nyxt, wofi, …) |
| `peteches/services/` | Reusable system service *types* (~32: alloy, caddy, comfyui, concourse, firewall, grafana, loki, pihole, prometheus, restic, tailscale, vault, vllm, …) |
| `peteches/packages/` | Package definitions (~90 files, incl. large node/python dep-closure files: mermaid-deps 2.6 MB, seerr-deps 1.9 MB) |
| `peteches/channels/` | Channel lock files (5 — see [Channels](#channels)) |
| `peteches/machines.scm` | `%all-machines` — 21 `machine` records pairing OS configs with SSH details; single source of truth for deploys |
| `peteches/repository.scm` | `source-path` / `repo-directory` — resolve repo assets via `%load-path` |
| `peteches/build/mesa-utils.scm` | Build-system helper (`patch-wrap-file`) |
| `peteches/grafana-dashboards/` | Grafana dashboard JSON (node-exporter, pihole, proxmox, synology) |
| `scripts/` | `deploy.scm` (the supported deploy entry point), `bump-channel.sh`, `build-vm-image`, `generation-gc`, `proxmox-clone-bootstrap`, `sync-restic-keys.sh` |
| `ci/` | Concourse CI pipelines + tasks (image-build, terraform-apply, critical-grind deploy/bump/lint) |
| `infra/terraform/` | Terraform IaC — provisions Proxmox VMs (one `module "…"` per VM) |
| `secrets/` | SOPS-encrypted secrets: `hosts/<vm>/`, `groups/`, `shared/` |
| `age-keys/` | SOPS age public keys, one per VM (21) |
| `configs/` | Non-Scheme assets referenced via `repo-directory` (emacs, hypr, claude, bin, alacritty, dms, matugen, nyxt, pi, vllm, wofi) |
| `containers/` | `guix-builder` Docker image for CI tasks; throwaway postgres test container |
| `docs/` | `fleet-deployment.org` (deploy rules — source of truth), `secrets-management.org`, `infrastructure.org`, `backups.org`, `dependency-review-todo.org` |
| `.claude/` | Claude Code skills (`deploy-vms`, `update-channels`, `review-channels`, `review-dependencies`, `review-packages`) |
| `proxmox-vms.org` | Authoritative VM inventory (IPs, VMIDs, specs) |
| `.sops.yaml` | SOPS creation rules (per-VM age recipients) |

**Legacy / dead code** (deletion candidates — see [Flags](#conventions--flags)):
`peteches/deploy.scm`, `peteches/utils.scm`, `peteches/monitoring/loki.scm`, and
`common-home-services` in `peteches/systems/common.scm`.

---

## The two OS constructors

Every system config is a thin wrapper around one of two constructors.

### `make-vm-os` — headless Proxmox VMs

`peteches/systems/vm-base.scm` → `make-vm-os`, `%vm-peteches-user`,
`%vm-peteches-authorized-keys`.

Baseline: openssh (key-only), ntpd, qemu-guest-agent, nftables firewall
(`%vm-base-firewall`: ssh + 9100 + icmp only, drop policy), cifs-client, node-exporter.

Key keywords (full list in the file header):

- `#:host-name` `#:bootloader` `#:file-systems` — **required**
- `#:ipv4-address` (CIDR with `/23` — LAN is `192.168.50.0/23`, gw `192.168.50.1`), `#:ipv6-address`
- `#:restic-config` (backups over SFTP to Synology), `#:sops-secrets` (SOPS+age → `/run/secrets/` at boot)
- `#:extra-services` `#:extra-packages` `#:users-extra`
- `#:with-nonguix?`, `#:with-nvidia?` `#:nvidia-driver-version`
- `#:with-nug-offload?` (default `#t` → build offload to the guix-build VM; misnamed, see [Flags](#conventions--flags))
- `#:with-automation-key?`, `#:with-swap?` (RAM-sized `/swapfile` via activation), `#:with-generation-gc?` (weekly mcron job)

### `make-base-os` — desktops / laptops

`peteches/systems/base.scm` → `make-base-os`, `%peteches-user`, `%common-services`,
`without-gdm`, `nonguix-substitute-service`.

Baseline: gtkgreet-in-cage greeter, Hyprland, libvirt, Tor, Tailscale, boltd.

Keywords: `#:laptop?` `#:intel-cpu?` `#:with-nvidia?` `#:with-docker?`
`#:with-printing?` `#:with-bluetooth?` `#:with-nonguix?` `#:offload-builds?`.

Used by exactly 2 hosts: **dagon** (NVIDIA desktop) and **nyarlothotep** (AMD laptop).

### House style

```scheme
(define-public <name>-os
  (operating-system (inherit (make-vm-os …))))
<name>-os   ; ← bare final expression: `guix system build` uses the last value,
            ;    `guix deploy` imports the module; omit it and the build breaks
```

### Shared system modules

- `common.scm` — `%nug-build-machine` (gexp build-machine repointed at the guix-build VM);
  `%authorize-coordinator-key` (trust nyarlothotep + claude-workstation to push store
  items; registers the guix-publish substitute on the guix-build VM).
- `monitored-hosts.scm` — `%monitored-hosts` (Prometheus scrape registry, auto-regenerated on deploy).
- `network-mounts.scm` — manual CIFS mount for desktops.
- `bootstrap.scm` — legacy Proxmox template.

---

## Deployment

### `machines.scm`

21 `define-public <name>-machine` records, each a `machine` with
`operating-system <name>-os` + `machine-ssh-configuration`. Hostnames are **Tailscale
MagicDNS names** (`<host>.spaniel-cordylus.ts.net`); user is always `peteches`
(passwordless sudo). `%deploy-identity` prefers
`/run/secrets/peteches-automation-ssh-key` (claude-workstation) else
`~/.ssh/id_ed25519`. `%all-machines` is the list. The desktops are deliberately absent
(reconfigured locally).

### `scripts/deploy.scm`

Wraps `guix deploy -L <repo-root> -e <expr>` with `--hosts` / `-h` pattern filtering
(regex on machine name / host-name / host-key / user; OR logic). It keeps its own
`%machine-names` alist that **must be updated in lockstep** with `machines.scm`, or
filtering errors with "Unknown machine".

### Deploy flow

```
scripts/deploy.scm -h <patterns>
  → filters %all-machines
  → guix deploy -L . -e "(list …)"
  → per host: build (offloaded to the guix-build VM via %nug-build-machine,
               substitutes from guix-publish :3000),
             send store items (coordinator-key authorized),
             reconfigure
```

Success = generation bump + service health. See
[`docs/fleet-deployment.org`](docs/fleet-deployment.org) for the blast-radius table: a
shared-module or channel-pin change affects the **whole fleet**; deploy promptly, don't
bundle unrelated changes, roll back per host.

---

## Channels

Five lock files, pins duplicated, nothing enforces agreement — the `/update-channels`
skill is the preferred route.

| File | Pins | Notes |
|---|---|---|
| `channels/base.scm` | `%base-channels`: sops-guix, guix-science, guix-science-nonfree, nonguix, guix | **THE REFERENCE.** All have channel introductions. Trailing bare `%base-channels` doubles as a plain list for `guix pull -C`. |
| `channels/dagon.scm` | `%dagon-channels` = base + guix-hpc-non-free | HPC channel has **no introduction** (unverified commits). |
| `channels/manual.scm` | mirrors dagon (6 channels) | Full plain list for `guix pull -C` / `~/.config/guix/channels.scm` symlink. |
| `channels/critical-grind.scm` | critical-grind only | **Deliberately separate**: private repo over SSH; guix authenticates git fetches with ssh-agent only, so only machines loading the deploy key pull it. No introduction. Bare list. |
| `channels/deploy-critical-grind.scm` | base + critical-grind (6) | For operator machines doing interactive work on the campaign VM; duplicates rather than imports. |

`scripts/bump-channel.sh <sha>` updates the critical-grind pin in both files that carry
it (bumping only one silently shipped a stale commit for generations — a documented gap).

---

## How the pieces fit together

- **System config composition** — a VM file = `make-vm-os` + machine-specific
  `file-systems` (vda2 ext4 root, vda1 "GNU-ESP" label), `sops-secrets`,
  `restic-config`, and `extra-services` (service types from `peteches/services/` +
  firewall extensions to open ports). Desktops = `make-base-os` + encrypted-root
  `mapped-devices`.
- **Secrets** — per-VM age key in `age-keys/<vm>.pub` + creation rule in `.sops.yaml`;
  `sops-secret` records in system configs decrypt at boot. Never reference a `secrets/`
  file before it exists — a missing `local-file` breaks the whole fleet's builds
  (`machines.scm` imports every system module).
- **Provisioning** — Terraform (`infra/terraform/`) creates the Proxmox VM shell;
  Concourse CI (`ci/`) builds the qcow2 image (`build-vm-image.yml` — age key baked
  into `/etc/age/keys.txt`), uploads it to MinIO, and Terraform apply imports it. The
  two non-pipeline paths (bootstrap.scm, nyarlothotep) still add
  `sops-key-generator-service-type` themselves.
- **Networking** — static `/23` addresses in system configs; DNS = the pihole VM
  (circular — deploys address VMs by IP / tailnet name, never LAN name); every VM runs
  Tailscale; nscd's `hosts` cache is deliberately dropped (NXDOMAIN caching breaks
  pihole custom-hosts edits).
- **Home stack** — `home/modules/base.scm` exports `base-packages` + `base-services`,
  composing focused modules and home service types; host configs in `home/configs/`
  append extras and evaluate to a bare `home-environment`.
  `home/modules/claude-workstation.scm` is a separate headless constructor (claude-code
  + MCP servers + Anvil emacs daemon + pre-cloned repos) instantiated per account —
  those three home configs are wired into the claude-workstation **system** via
  `guix-home-service-type`, so a system redeploy activates them.

---

## Conventions & flags

- **`peteches/deploy.scm`** — LEGACY, superseded by `machines.scm` + `scripts/deploy.scm`;
  lists only 5 of 21 machines. `docs/backups.org` and `proxmox-vms.org` still reference
  it — stale guidance; deletion candidate.
- **`peteches/utils.scm`** — both exports (`gather-manifest-packages`,
  `apply-template-file`) unreferenced; reads a nonexistent `manifests/` dir; hard-codes
  an absolute path.
- **`peteches/monitoring/loki.scm`** — dead code (not exported, not called). Routine log
  shipping is Grafana Alloy on each VM.
- **`common-home-services` in `peteches/systems/common.scm`** — leftover; nothing imports
  it; the live home config is in `home/modules/base.scm` and the two have drifted.
- **Broken export in `peteches/systems/base.scm`** — exports
  `greetd-gtkgreet-service` (singular) but only `greetd-gtkgreet-services` (plural) is
  defined; importing the singular fails (Guile only warns at compile time).
- **Misnamed bindings** — `%nug-build-machine` / `with-nug-offload?` point at the
  guix-build VM (nug was reinstalled as the proxmox3 host); kept for minimal diff churn.
  `parallel-builds 20` was copied from nug's 32-core box and OOM-killed on guix-build's
  8 cores (now 6).
- **`CLAUDE.md` is partially stale** vs actual repo state — it says the critical-grind
  channel is fetched over smart HTTP (actual: private GitHub over SSH), that
  `machines.scm` uses LAN IPs (actual: Tailscale names), and that there are 3 channel
  files (actual: 5); its file maps omit several newer VM configs.
- **`critical-grind-campaign.scm` deviates from house style deliberately** (grub-efi-bootloader
  not `-removable`; UUID-matched filesystems) — adopted from an existing install; do
  not "fix".
