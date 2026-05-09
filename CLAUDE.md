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

`base.scm` exports `make-base-os`, the central constructor for all systems. Host-specific files (`azathoth.peteches.co.uk.scm`, `bhiyaki.peteches.co.uk.scm`, `nug.scm`, `nyarlothotep.scm`) call it with hardware-specific arguments.

Key `make-base-os` flags:
- `laptop?` — enables TLP and thermald
- `intel-cpu?` (default `#t`) — adds intel-microcode and IOMMU kernel args
- `with-nvidia?` — adds NVIDIA driver, firmware, kernel modules, Wayland env vars
- `with-docker?` — enables containerd + Docker services
- `with-bluetooth?`, `with-printing?` — optional services
- `with-nonguix?` — registers nonguix substitute server

`without-gdm` strips GDM from `%desktop-services` and configures the local Guix substitute server (`nug.peteches.co.uk:3000`). All systems use gtkgreet inside cage as the greeter, launching a Hyprland session.

### Home Configurations (`peteches/home-configs/`)

`base.scm` is the main home environment; it composes feature modules from `peteches/home-services/` (e.g., emacs, git, hyprland, waybar, aws, ai tools) and system-specific fragments. Host-specific configs typically `#:use-module` base plus add machine-local services/packages.

### Channels (`peteches/channels/`)

`base.scm` exports `%base-channels` — a pinned list of: `guix` (codeberg), `nonguix` (gitlab), `guix-science`, and `simendsjo`. Update commits here to upgrade channels.

### Utilities (`peteches/utils.scm`)

- `gather-manifest-packages` — reads manifest `.scm` files from `manifests/` directory and converts them to package+output pairs for inclusion in home/system package lists.
- `apply-template-file` — reads a file and substitutes `${KEY}` placeholders from an alist; used for generating config files from templates.
