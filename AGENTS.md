# AGENTS.md

This file provides guidance to AI coding agents (pi, and any similar harness) when working with code in this repository.

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

Custom packages, home services, and system services used to live in a separate private
Codeberg channel repo (`peteches/guix-channel`). Codeberg changed its ToS to prohibit
AI-generated code, and since this repo is now developed with AI coding agents, that channel
was folded back into this repo — see `peteches/packages/`, `peteches/home/services/`
and `peteches/services/` below. They are ordinary local modules loaded via `-L .`, with
no channel pin and no re-pinning step to change a service type.

## Design Points

**Wrappers are store packages, never `~/.local/bin` files.**
NEVER create a package that installs a wrapper script into `~/.local/bin`
(e.g. via a `home-files-service-type` entry) to shadow a wrapped binary.
Instead, any wrapper should be a new package — e.g. `peteches-pi-wrapper` —
that **inherits from the wrapped package** and **installs its wrapper in the
store** (the wrapper is the package's own `bin` output). Then add the wrapper
package to the profiles (system `#:extra-packages`, home packages, …)
**instead of the wrapped package**, so the store `bin` on PATH *is* the
wrapper.

## Common Commands

All `guix` commands that reference local modules need `-L .` (or `--load-path=.`) to be run from the repo root.

```bash
# Validate a system config (dry-run)
guix system build -L . --dry-run peteches/systems/<host>.scm

# Build a QCOW2 disk image for a VM
guix system image -L . -t qcow2 peteches/systems/<vm>.scm

# Pull channels (updates all pinned channels).
# Pins live in channels.scm since dde86b3; manual.scm re-exports it.
# WARNING: `guix pull -C' currently FAILS on both files — channel files are
# evaluated under %safe-channel-bindings, which lacks use-modules/define-module
# (open issue; see the "Channels" section of docs/file-map.md). Until fixed,
# materialise a bare (list …) channel file for the pull.
guix pull -C peteches/channels/manual.scm

# Deploy to all VMs (or filter with --hosts)
scripts/deploy.scm
scripts/deploy.scm --hosts prometheus.spaniel-cordylus.ts.net  # single VM by tailnet name
scripts/deploy.scm -h "host-name=prometheus"       # by hostname pattern
scripts/deploy.scm -h "prometheus,loki" --dry-run  # multiple patterns

# Check channels
guix describe
```

## Fleet Deployment

**Whenever asked for a fleet update, a fleet deploy, or "deploy the
changes" across more than one VM, follow `docs/fleet-deployment.org` — do
not improvise.** It is the single source of truth for:

- which hosts a given change's blast radius requires deploying to (a
  shared-module or channel-pin change means the *whole* fleet, not just the
  host you were editing — this is how the fleet previously drifted, with
  some VMs weeks stale relative to others)
- always using the `deploy-vms` skill rather than raw `scripts/deploy.scm`
  or `guix deploy` invocations
- how to verify success per host (generation bump + service health check,
  including the known shepherd-reload ACL-gap failure mode)
- how to roll back a single host or a fleet-wide change
- the fleet consistency audit recipe (per-host `guix system describe` over
  SSH) to run after a fleet deploy, or whenever asked to check when VMs
  were last deployed/reconfigured

Raw `scripts/deploy.scm` invocations (as in "Common Commands" above) remain
fine for a quick single-host deploy during iterative development; the
`docs/fleet-deployment.org` rules apply once more than one host, or a
shared-module/channel-wide change, is involved.

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

**4. Some modules still need `-L` pointed at real checkouts of *other*
channels.** `(peteches services ...)`, `(peteches home services ...)` and
`(peteches packages ...)` are local now (just `-L .`), but several of them
import `(nongnu ...)`, `(sops ...)`, `(guix-science-nonfree ...)` from the
dependency channels this repo still pulls in (see `peteches/channels/channels.scm`
for URLs/commits). None of these need to be *pulled* — a plain shallow clone
at the pinned commit is enough, passed via repeated `-L`:

```bash
guix repl -L . -L /path/to/nonguix \
          -L /path/to/guix-science -L /path/to/guix-science-nonfree \
          -L /path/to/sops-guix/modules -- …
```

Missing one just means the specific module chain that needs it fails to
resolve (e.g. `(nonguix packages nvidia)` without a `nonguix` checkout) —
everything else still validates fine. The `critical-grind` channel
(`(critical-grind packages campaign)`, `(critical-grind services campaign)`)
is the one remaining channel that is genuinely external — see the "Channels"
section of `docs/file-map.md`.

**5. Real builds need the guix daemon.** `guix build` / `guix system build`
cannot even compute a derivation without a store connection, so `--dry-run`
does not help.

```bash
guix system build -L . --dry-run peteches/systems/<host>.scm
guix build -L . -L "$CHANNEL" peteches/packages/<pkg>.scm:<name>
```

Check whether you have it:

```bash
ls /var/guix/daemon-socket/socket   # present => builds work
guix describe                       # lists channels => pulled guix in use
```

Channel pins are single-sourced in `peteches/channels/channels.scm` since
dde86b3 (`%dagon-channels`); `manual.scm` is a thin re-export of it intended
for `guix pull -C`, and `critical-grind.scm` pins the external app channel.
Note: `guix pull -C` on these re-export files currently fails — channel files
are read under `%safe-channel-bindings`, which lacks `use-modules` /
`define-module` (open issue). The files use Guix record macros, so bare
`guile` cannot load them anyway — validate with a read-only parse instead:
```bash
guile -c '(call-with-input-file "peteches/channels/manual.scm"
            (lambda (p) (let loop () (unless (eof-object? (read p)) (loop)))))'
```

## Architecture

### Where code actually lives

Two sources are easy to confuse when reading a `#:use-module` line:

| Module prefix | Where it lives |
|---|---|
| `(gnu ...)`, `(guix ...)` | upstream Guix |
| `(critical-grind packages ...)`, `(critical-grind services ...)` | the **`critical-grind` channel** — the private application repo at `git@github.com:peteches/critical-grind-battlefronts.git`, pinned in its own dedicated file `peteches/channels/critical-grind.scm` (deliberately not in `channels.scm`) — **not in this repo** |
| `(peteches services ...)`, `(peteches home services ...)`, `(peteches packages ...)`, `(peteches systems ...)`, `(peteches home modules ...)`, `(peteches home configs ...)`, `(peteches channels ...)`, `(containers ...)` | this repo |

So `alloy-service-type`, `restic-vm-backup-service-type`, `firewall-service-type`,
`tailscale-service-type`, `home-git-service-type` and friends are defined in
`peteches/services/` and `peteches/home/services/` in this repo — service
*types* — while `peteches/systems/` and `peteches/home/modules/` +
`peteches/home/configs/` supply configuration *values* for them. Changing a
service type is just editing the file; there is no channel to re-pin.

These package/service directories used to live in a separate private Codeberg
channel repo (`peteches/guix-channel`), pulled as `(peteches ...)` via the
now-removed `peteches/channels/base.scm`. Codeberg changed its ToS to prohibit
AI-generated code; since this repo is developed with AI coding agents, the channel
was folded back in here instead of continuing to host it there. The only
channel that remains genuinely external is `critical-grind` (a separate
application repo, not a modularisation of this one).

### Two OS constructors

| Constructor | Module | Used by |
|---|---|---|
| `make-vm-os` | `peteches/systems/vm-base.scm` | every headless Proxmox VM |
| `make-base-os` | `peteches/systems/base.scm` | the two desktops (dagon, nyarlothotep) |

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
| `peteches/home/configs/` | Host-specific home-environment files (`dagon.scm`, `nyarlothotep.scm`) |
| `peteches/home/modules/` | Shared home config fragments — `base.scm` plus focused modules (ssh, gpg, theming, ai, etc.) — configuration *values* |
| `peteches/home/services/` | Reusable home service *types* (aws, git, hyprland, firefox, nyxt, wofi, mako, mpv, …) — folded in from the retired `peteches` channel |
| `peteches/services/` | Reusable system service *types* (alloy, restic, firewall, tailscale, grafana, pihole, …) — folded in from the retired `peteches` channel |
| `peteches/channels/` | Channel lock files (five of them — see the "Channels" section of `docs/file-map.md`) |
| `peteches/packages/` | Package definitions — folded in from the retired `peteches` channel, plus `desktop-scripts.scm`, `claude-completion.scm`, `docker-compose.scm`, `emacs-anvil.scm` which were already local |
| `peteches/repository.scm` | `repo-directory` / `source-path` — resolve repo assets via `%load-path` |
| `peteches/machines.scm` | Named `machine` records + `%all-machines` list — `host-name` is each VM's Tailscale MagicDNS name (`<host>.spaniel-cordylus.ts.net`), not its LAN IP |
| `peteches/grafana-dashboards/` | Grafana dashboard JSON definitions |
| `configs/` | Non-Scheme assets (emacs, hypr, matugen, nyxt, wofi, alacritty, bin, claude, dms) referenced via `repo-directory` |
| `age-keys/` | SOPS age public keys, one per VM |
| `secrets/` | SOPS-encrypted secrets per host/group/shared — see `docs/secrets-management.org` |
| `infra/` | Terraform infrastructure-as-code for Proxmox VM provisioning |
| `ci/` | Concourse CI pipeline and task definitions |
| `containers/` | Container definitions |
| `scripts/deploy.scm` | `guix deploy` wrapper with `--hosts` filtering — **the supported deploy entry point**. **Derivation trap:** `peteches/packages/generation-gc.scm` embeds the *whole* `scripts/` directory via `(local-file ... #:recursive? #t)` and ships in every `vm-base` VM's profile, so **any** edit under `scripts/` — even a comment — changes every VM's `system.drv` and is a fleet-wide deploy-owed change, not a local one (see `docs/file-map.md`'s "Derivation trap" section for the before/after check) |
| `proxmox-vms.org` | VM inventory and IP reference table — **authoritative** for IPs |
| `docs/` | Architecture documentation — includes `docs/file-map.md`, the detailed per-file reference split out of this document |
| `.pi/skills/` | Automation skills: `update-channels`, `deploy-vms` (fleet deploys per docs/fleet-deployment.org), `wireguard-socks5` (claude-workstation's split tunnel — moved here out of `configs/claude/ygo-only/skills/` when Claude Code left the box, since the tunnel it documents outlived the CLI), plus the `review-*` skills |
| `.pi/agents/` | Repo-local pi subagents: `guix-validator`, `secrets-sentinel`, `service-reviewer`, `vm-auditor` — see "Subagent workflow" |

### Detailed file map

The per-file reference (every `peteches/systems/`, `peteches/packages/`,
`peteches/services/`, `peteches/home/*` and `peteches/channels/` file with a
one-line purpose, plus the `make-base-os`/`make-vm-os` flag reference and the
channels narrative) lives in **`docs/file-map.md`**. Read it on demand — for
most "where does X live" questions a grep or a scout subagent is cheaper than
loading the whole map.

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

10. **Build offload** — `make-vm-os` enables it by default, and it has **three** halves that all have to be wired or it degrades silently. Pick one:
    - **Wire it:** (a) add a `guix-offload-key` `sops-secret` reading `secrets/hosts/<name>/guix-build.yaml`; (b) put the VM's `guix-offload` **ssh** public key in `guix-build.scm`'s `guix-offload-authorized-keys`; **and** (c) put the VM's `/etc/guix/signing-key.pub` in `guix-build.scm`'s `guix-offload-signing-keys`. Half-wiring fails silently in two *different* ways: missing (b) means ssh refuses and `guix` reports an offload failure, while missing (c) means the connection and the build request succeed and only the **archive push** is refused with `unauthorized public key` — which `guix` degrades to building locally, so offload looks fine and never happens. Read `docs/fleet-deployment.org`'s "The build-offload keypairs drift separately" section before touching either list, and take both keys **from the live box** (host-key-verified, hashed with the `#…#`-token method): a key copied from an old commit is how `claude-workstation`'s entry sat stale there for weeks.
    - **Opt out:** pass `#:with-nug-offload? #f`.

    `guix-build`'s own keypair is generated on the box and is in no file in this repo, and `secrets/hosts/<name>/guix-build.yaml` can only be re-keyed from a desktop if the operator is a recipient of *that* file — `secrets/hosts/claude-workstation/guix-build.yaml` is age-only with no `pgp:` group, so it is not (see `docs/secrets-management.org`).

Pick a free IPv6 suffix too, if the VM needs one. The `2a10:d582:ef59::`
addresses are assigned by hand and tracked **only** in the `#:ipv6-address`
lines of `peteches/systems/*.scm` — `proxmox-vms.org` records IPv4 only, so
nothing checks for collisions. Check the current allocation first:

```bash
command grep -rn '#:ipv6-address' peteches/systems/
```

### Resolving repo-relative paths

Use `(peteches repository)` — `repo-directory` / `source-path` search
`%load-path`, so they keep working under `-L .` and in worktrees. Never
reach for a relative path or an absolute one.


## Subagent workflow

Work fans out through the `subagent` tool. Available agents:

| Agent | Where defined | Use for |
|---|---|---|
| `worker` | global (`~/.pi/agent/agents/`) | multi-file edits, builds, fixes — the only agent that writes code |
| `scout` | global | read-only codebase recon (file maps, line ranges, key snippets) |
| `researcher` | global | web research (upstream docs, error messages, release notes) |
| `guix-validator` | `.pi/agents/` | load-checks changed `.scm` modules with the fresh-process-per-module recipe |
| `secrets-sentinel` | `.pi/agents/` | pre-commit / pre-command review against the secret-handling rules above |
| `service-reviewer` | `.pi/agents/` | firewall / channel-pin / monitoring wiring review after a service change |
| `vm-auditor` | `.pi/agents/` | the 10-step "Adding a New VM" checklist audit |

Rules that make the fan-out tight:

1. **Briefs are self-contained.** A subagent sees none of the orchestrator's
   conversation. Every task description must carry: the exact file paths
   involved, what changed and why, and the commands needed to verify. Aim for
   the detail level that lets the agent finish without asking a question —
   2–3 KB task briefs are normal here, not excessive.

2. **Validation gate.** Any task that edits `.scm` under `peteches/` is not
   complete until `guix-validator` has been spawned on the changed modules and
   its output is quoted in the final summary. "Looks right" is not a result;
   PASS/FAIL/ENV-GAP per module is. The orchestrator (or the worker's brief)
   is responsible for spawning it — do not skip it because the edit was small.

3. **Review gates.** Before `git commit`/`git push`, spawn `secrets-sentinel`
   on the diff. After any service change, spawn `service-reviewer`. After
   creating or modifying a VM system file, spawn `vm-auditor`. These are
   cheap, read-only, and catch exactly the failure classes this repo has
   already paid for once.

4. **Never poll subagents.** Spawning is fire-and-forget: the harness wakes
   the orchestrator with each result automatically. Do not tail session
   files, sleep-loop, or "check status" — spawn independent agents in
   parallel and end the turn, or do other work. To follow up with a finished
   agent, `subagent_message` with the same `name` resumes its session.

5. **Repo skills.** The repo's skills live in `.pi/skills/`, so
   `deploy-vms`, `update-channels`, `wireguard-socks5` and the review skills
   are loadable in pi (`/skill:deploy-vms …`). Fleet deploys and channel
   updates must still go through those skills per "Fleet Deployment" and
   "Channels" — a subagent brief for such work should name the skill's
   `SKILL.md` path explicitly.

## Editing Scheme files

Use precise, minimal textual edits (`edit` with the smallest unique anchor
text). Parens are the hazard, so after any structural change to a `.scm`
file run the read-only parse check from "Validating changes" (bare `guile`
`read` loop) before anything else — it catches unbalanced parens in seconds —
then load-check with `guix repl`, one module per fresh process. Never claim a
Scheme change is done on visual inspection alone.
