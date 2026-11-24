# Complete File Map & Module Reference

Detailed per-file reference for this repository, split out of `AGENTS.md` so
the root context file stays small. Consult on demand — a scout/grep can also
answer most "where does X live" questions without reading this whole file.

Root-level orientation (directory layout, validation recipes, the new-VM
checklist, deploy rules) lives in `AGENTS.md`.

## Complete File Map

### `peteches/systems/`

| File | Purpose |
|---|---|
| `base.scm` | `make-base-os` — desktop/laptop base (Hyprland, greetd, libvirt) |
| `vm-base.scm` | `make-vm-os` — headless Proxmox VM base (SSH, nftables, node-exporter) |
| `common.scm` | Shared OS-level services and environment |
| `network-mounts.scm` | Network filesystem mount definitions |
| `monitored-hosts.scm` | Registry of Prometheus node-exporter scrape targets |
| `bootstrap.scm` | Minimal bootstrap OS for initial VM provisioning |
| `dagon.scm` | dagon desktop system (nug's successor; nug was decommissioned) |
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
| `claude-workstation.scm` | Jump-box VM — split-tunnel WireGuard + microsocks SOCKS5 (the desktops relay `ygo-server`/`ygoswiss1`/`ygoswiss2` ssh through it), tailscale, alloy; ONE home environment (`peteches`) + a plain `ygo` login account. Claude Code, the `criticalgrind` account and the Postgres/Podman dev-DB stack are gone. It is **not** a deploy origin any more: the automation SSH key it used to decrypt moved to `agents` on 2026-11-05 and its signing key was dropped from `%authorize-coordinator-key` (it had gone stale with the 2026-10-27 MinIO-image rebuild) (192.168.51.205) |
| `comfyui.scm` | ComfyUI VM — RTX 4090 GPU passthrough with nonguix driver/CUDA; successor to nug's desktop-hosted comfyui. Also runs strata (Intel Arc B70), AllTalk TTS (XTTS on the 4090), the WhisperX transcriber (speaker-labelled transcripts, :8787) and the OpenVINO GenAI server (LFM2.5-24B-A2B int4 on the B70, loopback :8093) (192.168.51.206) |
| `guix-build.scm` | Guix substitute server + build-offload target VM — successor to nug's guix-publish/offload role; signs with `guix-build-substitute-key.pub` (192.168.51.207). Holds BOTH offload trust lists: `guix-offload-authorized-keys` (ssh logins) and `guix-offload-signing-keys` (archive pushes). Until 2026-11-05 the second held one stale key; it now carries every offloader's live key, and `rustdesk` + `nyarlothotep` are the known gap (in the ssh list, no archive key, both unreachable when it was audited) |
| `agents.scm` | "agents" VM — rules-lawyer Discord bot + SillyTavern + `pi` for the peteches account (home-environment from `peteches/home/configs/agents-peteches.scm`); LLM/stable-diffusion offloaded to other VMs; it also runs the **herdr server** — supervised by the
  system shepherd as `herdr-server-peteches` (`peteches/services/herdr.scm`) —
  so this is the box `herdr --remote` attaches to. Since 2026-11-05 it is also a
  **deploy origin**: it decrypts the private half of the fleet-wide peteches
  automation keypair (`secrets/hosts/agents/peteches-automation-ssh.yaml`) and
  its own signing key is listed in `%authorize-coordinator-key` — what is still
  missing for deploys to actually run from here is a `~/area_51/guix` checkout
  (192.168.51.208) |
| `guix-build-substitute-key.pub` | Guix-publish signing public key for guix-build's substitute server (nug's substitute-server successor) |

### `peteches/home/configs/`

Host-specific files only — each exports a complete `home-environment` record.

| File | Purpose |
|---|---|
| `dagon.scm` | dagon home environment — `(peteches home configs dagon)` |
| `nyarlothotep.scm` | nyarlothotep home environment — `(peteches home configs nyarlothotep)` |
| `claude-workstation-peteches.scm` | `(peteches home configs claude-workstation-peteches)` — jump-box account config built by `make-vm-jumpbox-home`; **netcat only**. The `Host *` automation identity and `known-hosts2` it used to pass moved to `agents-peteches.scm` on 2026-11-05, and the repo-checkout justification for them was already false — neither VM has a `~/area_51` checkout (checked live) |
| `agents-peteches.scm` | `(peteches home configs agents-peteches)` — `make-vm-jumpbox-home` + `pi-coding-agent` + `home-pi-service-type` (the shared `configs/pi/defaults`); no Claude, no anvil/MCP. herdr is the one daemon
  exception: it carries `herdr-mx` (the binary the remote probe's
  `command -v herdr` looks for) and links the pi<->herdr integration hook
  `configs/pi/herdr-agent-state.ts`, and it now installs the
  `pi-interactive-subagents` extension (the `subagent` tool -- the only
  consumer of `configs/pi/defaults/agents/*.md`, whose three overrides were
  inert on this box before). It also carries the **fleet deploy identity** since
  2026-11-05: `#:ssh-hosts` `Host *` → `IdentityFile
  /run/secrets/peteches-automation-ssh-key` (the automation keypair every VM
  accepts for `peteches`) plus `#:known-hosts2` for the github/codeberg/git
  fetches — i.e. exactly what claude-workstation's config used to pass |

### `peteches/home/modules/`

Shared fragments — imported by host configs and composed into `base-packages` / `base-services`.

| File | Purpose |
|---|---|
| `base.scm` | `(peteches home modules base)` — orchestrator; exports `base-packages` and `base-services` |
| `ssh.scm` | `(peteches home modules ssh)` — `base-ssh-service` with all SSH host entries |
| `gpg.scm` | `(peteches home modules gpg)` — `base-gpg-service` with pinentry dispatch script |
| `syncthing.scm` | `(peteches home modules syncthing)` — `base-syncthing-service` (org folder + devices) |
| `theming.scm` | `(peteches home modules theming)` — `base-theming-services` list (cursor, wallpaper, matugen, DMS plugin) |
| `ai.scm` | `(peteches home modules ai)` — `base-ai-service`: ECA config **only**. pi's MCP servers come from `base.scm`'s `%desktop-mcp-servers`. Its anvil path is stale/broken |
| `claude.scm` | `(peteches home modules claude)` — defines `home-claude-service-type`: symlinks `configs/claude/defaults` into `~/.claude/` and registers MCP servers. **Unwired** since Claude Code left every machine (`base-services` no longer instantiates it, `configs/claude/` is defined-but-unused); kept only for the `<home-claude-mcp-server>` record, which `base.scm`/`pi.scm` still build for pi's mcp.json |
| `vm-jumpbox.scm` | `(peteches home modules vm-jumpbox)` — `make-vm-jumpbox-home`, the shared headless home-environment constructor for VM accounts (git, ssh, proxychains-ng, pinned `channels.scm`, canonical git identity; `#:extra-packages`/`#:ssh-hosts`/`#:known-hosts2`/`#:extra-services` hooks). Used by `claude-workstation-peteches.scm` and `agents-peteches.scm` |
| `mako.scm` | `(peteches home modules mako)` — `base-mako-config` and `base-mako-service`. **Unused**: `base.scm` no longer composes it (dropped 2026-09-27 so DankMaterialShell is the sole owner of `org.freedesktop.Notifications`); `theming.scm` still emits matugen mako colours that nothing now consumes |
| `firefox.scm` | `(peteches home modules firefox)` — Firefox profiles. `base-firefox-global-prefs`/`-extensions` are **unused**, so uBlock/DarkReader/PassFF are not installed |
| `git.scm` | `(peteches home modules git)` — `peteches-gpg-for-git` package and `git-config` |
| `scoreplay.scm` | `(peteches home modules scoreplay)` — `%scoreplay-ssh-hosts`. **Unused**: `ssh.scm` never splices it in, so these hosts are absent from `~/.ssh/config` |
| `mpv.scm` | `(peteches home modules mpv)` — `%mpv-profiles` |
| `ssh-authorized-keys` | SSH authorized keys (co-located with `ssh.scm` for `local-file`) |
| `firefox-extensions/` | Firefox .xpi extensions (uBlock, DarkReader, PassFF, AWS SSO) |
| `git-hooks/` | Git hook scripts (pre-commit) |
| `git-ignore.txt` | Git global ignore patterns |

### `peteches/channels/`

Five files. The first three carry the routine channel set's pinned commits,
duplicated and kept in sync **by hand**; the last two are a separate pair for
the private `critical-grind` channel. Prefer the `/update-channels` skill
over editing pins directly.

| File | Purpose |
|---|---|
| `base.scm` | `%base-channels` — the reference. Module. Pinned: sops-guix, guix-science, guix-science-nonfree, nonguix, guix |
| `dagon.scm` | Module exporting `%dagon-channels` = `%base-channels` + guix-hpc-non-free |
| `manual.scm` | **Full** plain channels list (all 6) for `guix pull -C` / symlinking to `~/.config/guix/channels.scm` |
| `critical-grind.scm` | The `critical-grind` channel's own dedicated entry — a private GitHub repo fetched over SSH, deliberately **not** in `base.scm`/`manual.scm`; only machines that load the read-only deploy key into an agent should pull it |
| `deploy-critical-grind.scm` | Duplicates `base.scm`'s entries plus `critical-grind.scm`'s, for an operator machine's `guix pull -C` with the deploy key in an ssh-agent |

### `peteches/packages/`

Package definitions. All but the last four rows below were folded in from the
retired `peteches` channel; the last four were already local. Node/Python
dependency-closure files (`claude-agent-acp.scm`, `mermaid.scm`,
`seerr-deps.scm`, `go-deps.scm`, `python-deps.scm`) list only their
culminating package(s) — most of their content is transitive deps.

| File | Provides |
|---|---|
| `aider.scm` | `aider` — AI pair-programming CLI |
| `alloy.scm` | `alloy` — Grafana Alloy telemetry collector |
| `aws.scm` | `aws-session-manager-plugin` |
| `beeper.scm` | `beeper-bin` — Beeper chat client |
| `caddy.scm` | `caddy` |
| `claude-agent-acp.scm` | `node-agentclientprotocol-claude-agent-acp-*` + ~50 node deps — Claude Agent ACP bridge (editor integration) |
| `claude-code.scm` | `claude-code` — the Claude Code CLI |
| `comfyui-mcp.scm` | `node-comfyui-mcp` + sharp/canvas/sqlite node deps |
| `concourse.scm` | `concourse`, `fly` |
| `emacs.scm` | `emacs-slack`, `emacs-mcp`, `emacs-linear` |
| `fonts.scm` | `font-jost` |
| `go-deps.scm` | Go module deps for GitHub API tooling (10 packages) |
| `go-enum.scm` | `go-enum` — Go enum code generator |
| `go-tools.scm` | `go-golangci-lint` |
| `gpg.scm` | `peteches-pinentry-switch` — pinentry dispatch script |
| `grafana.scm` | `grafana` |
| `gurps.scm` | `gurpscharactersheet` |
| `hyprland.scm` | `hyprland-glvnd` |
| `jellyfin.scm` | `jellyfin-ffmpeg`, `jellyfin` |
| `koboldcpp.scm` | `koboldcpp-bin`, `koboldcpp-nocuda-bin` |
| `loki.scm` | `loki` |
| `lycheeslicer.scm` | `lycheeslicer-7.6.2` — 3D print slicer |
| `mcp.scm` | MCP servers: `plane-mcp-server`, `mcp-grafana`, `mcp-outline`, `go-github-com-sonirico-mcp-shell` |
| `mermaid.scm` | `node-mermaid-js-mermaid-cli` + ~787 node deps |
| `nvidia-container-runtime.scm` | `nvidia-modprobe`, `libnvidia-container`, `nvidia-container-toolkit` |
| `openvino.scm` | `openvino`, `openvino-genai` — Intel OpenVINO runtime + GenAI C++/python serving stack, built from source with every FetchContent/submodule dependency vendored as its own source package (see the module comment) |
| `nyxt.scm` | `sbcl-nyxt-webkit`, `nyxt-webkit`, `nyxt-electron` |
| `nzbget.scm` | `nzbget` |
| `pihole-exporter.scm` | `pihole-exporter` |
| `pi-interactive-subagents.scm` | `pi-interactive-subagents` — peteches fork of amosblomqvist/pi-interactive-subagents (itself tmux-only) with herdr pane support added; provides pi's `subagent` tool and is the *only* consumer of `configs/pi/defaults/agents/*.md`, since pi core has no `agents/` directory. Wired on the desktops through `base.scm` and on agents through `agents-peteches.scm`; extensions install as store symlinks into `~/.pi/agent/extensions/`, never as PATH packages |
| `pihole.scm` | `pihole-ftl`, `pihole-scripts`, `pihole-web` |
| `prometheus.scm` | `prometheus` |
| `prowlarr.scm` | `prowlarr` |
| `proxmox-scripts.scm` | `proxmox-scripts` — packages `scripts/` (`proxmox-clone-bootstrap`) via `local-file "../../scripts"` |
| `python-deps.scm` | Python MCP/Plane-SDK deps: `python-mcp-1.26`, `python-fastmcp`, `python-plane-sdk`, +12 more |
| `radarr.scm` | `radarr` |
| `rclone.scm` | `rclone` |
| `rustdesk.scm` | `rustdesk-server`, `rustdesk` |
| `scripts.scm` | `shell-scripts` |
| `seerr-deps.scm` | Node/Tailwind deps for Seerr (~1112 packages) |
| `seerr.scm` | `seerr` |
| `shims.scm` | `python-six-bootstrap` — compat shim, points at the current `six` |
| `sonarr.scm` | `sonarr` |
| `strata.scm` | `strata`, `strata-sycl-engine`, `strata-vision`, `strata-llama-cpp-src` — the Strata MoE engine — plus the Intel Arc GPU driver stack, all five wrapped from Intel's own GitHub `.deb` releases through one shared `deb-libraries-builder`: `intel-level-zero-gpu` (the Level Zero driver), `intel-igc-core` (`libigc.so.2`), `libigdgmm` (`libigdgmm.so.12`), and the OpenCL pair `intel-igc-opencl` (`libigdfcl.so.2`) + `intel-opencl-icd` (`libigdrcl.so`, with its ICD descriptor rewritten from the .deb's Debian path to the store path). **The OpenCL pair is for OpenVINO, not for strata** — strata uses its own bundled SYCL/L0 stack. Read the long comment above those definitions before removing them: oneDNN's Level Zero backend enumerates device extensions through OpenCL, so without them OpenVINO's GPU plugin enumerates the card and then dies at `ProgramBuilder … could not create an engine`. Versions must move together — `libigdfcl` and `libigc` are two halves of one IGC release |
| `tailscale.scm` | `tailscale` |
| `terraform.scm` | `terragrunt` |
| `vault.scm` | `vault` |
| `desktop-scripts.scm` | `peteches-desktop-scripts` — packages `configs/bin/`, including `transcribe` (CLI for the fleet WhisperX service; output goes to `-o`, else `$TRANSCRIBE_OUT_DIR`, else stdout, because the server stores nothing). Propagates `curl`, which the script needs and profiles do not necessarily carry. Scripts must be listed explicitly in `#:install-plan`. Already local (source lives in this repo) |
| `claude-completion.scm` | `claude-completion` — packages `configs/claude/claude-completion.bash` as `share/bash-completion/completions/claude`. **Unconsumed** — `base-packages` dropped it when Claude Code came off the desktops. Already local |
| `emacs-anvil.scm` | `emacs-anvil` — packages anvil.el + `anvil-stdio.sh`; consumed only by `(peteches home modules base)` now (its other consumer, `(peteches home modules claude-workstation)`, was deleted). Already local |
| `docker-compose.scm` | `docker-compose` plugin packaging. Already local |

### `peteches/services/`

System service *types*, folded in from the retired `peteches` channel.

| File | Provides |
|---|---|
| `alltalk.scm` | `alltalk-service-type` — AllTalk TTS v2 server (XTTS voice synthesis) via uv in an FHS sandbox; the GPU TTS backend SillyTavern's native AllTalk provider talks to (comfyui VM, :7851 API + :7852 Gradio UI) |
| `alloy.scm` | `alloy-service-type` |
| `boltd.scm` | `boltd-service-type` — Thunderbolt authorization daemon |
| `caddy.scm` | `caddy-service-type` |
| `cifs.scm` | `cifs-client-service-type` |
| `colibri.scm` | `colibri-service-type` — Colibrì MoE inference engine (defined, not currently wired into any system) |
| `comfyui.scm` | `comfyui-service-type` |
| `concourse.scm` | `concourse-web-service-type`, `concourse-worker-service-type` |
| `firewall.scm` | `firewall-service-type` |
| `grafana.scm` | `grafana-service-type` |
| `herdr.scm` | `herdr-server-service-type` + `herdr-server-configuration` — headless herdr agent-server supervised by the SYSTEM shepherd (no login shell, no user shepherd needed); PATH is built in the store, `command -v herdr` coverage comes from the HOME profile, not `~/.local/bin` |
| `jellyfin.scm` | `jellyfin-service-type` |
| `koboldcpp.scm` | `koboldcpp-service-type` — KoboldCpp text-generation server on the CUDA GPU (comfyui's RTX 4090; system-level port of the home service) |
| `lidarr.scm` | `lidarr-service-type` — Lidarr music automation (arr VM) |
| `loki.scm` | `loki-service-type` |
| `media-accounts.scm` | `media-accounts-service-type` |
| `media-normalise.scm` | `media-normalise-service-type` — nightly media-library normalisation (jellyfin VM) |
| `nvidia.scm` | `nvidia-vm-service-type` — repo-local NVIDIA VM service type (driver/kernel-module plumbing for `with-nvidia?` systems) |
| `nzbget.scm` | `nzbget-service-type` |
| `openvino.scm` | `openvino-service-type` + `openvino-server-configuration` — OpenVINO GenAI model server (FastAPI/uvicorn via the co-located `openvino-server.py`; `openvino-test.py` alongside it is the out-of-band inference probe used for the first run); accepts one configuration or a list of them; loopback-only and `auto-start? #f` by default. The `device` field picks CPU or GPU, and **GPU on the Arc Pro B70 works** (measured: LFM2.5-24B-A2B int4 at ~164 tok/s vs ~6-8 tok/s on CPU). Two non-obvious requirements for the GPU path: the service environment must name both packages' `python/` trees on PYTHONPATH *and* both `runtime/lib/intel64` trees on LD_LIBRARY_PATH (upstream's SDK layout means the profile merge picks up neither), plus `OCL_ICD_VENDORS`, because shepherd passes only the listed variables; and the system profile must carry `intel-opencl-icd` + `intel-igc-opencl` (see `(peteches packages strata)` — oneDNN's Level Zero backend enumerates device extensions through OpenCL, so a Level Zero-only install fails at `ProgramBuilder`/`could not create an engine`) |
| `outline.scm` | `outline-service-type` |
| `pihole.scm` | `pihole-service-type` |
| `plane.scm` | `plane-service-type` |
| `prometheus.scm` | `prometheus-service-type` |
| `prowlarr.scm` | `prowlarr-service-type` |
| `radarr.scm` | `radarr-service-type` |
| `restic.scm` | `restic-vm-backup-service-type` — restic backups over SFTP to Synology |
| `rules-lawyer.scm` | `rules-lawyer-service-type` — the rules-lawyer Discord bot (agents VM); talks to strata through caddy's tailnet proxy |
| `rustdesk.scm` | `rustdesk-service-type` |
| `seerr.scm` | `seerr-service-type` |
| `sillytavern.scm` | `sillytavern-service-type` — SillyTavern LLM frontend (agents VM, :8001 via caddy) |
| `sonarr.scm` | `sonarr-service-type` |
| `sops-key-generator.scm` | `sops-key-generator-service-type` — one-shot age keypair generation on first boot |
| `strata.scm` | `strata-service-type` + `strata-configuration` — Strata MoE inference engine (Intel Arc SYCL, comfyui VM): native-pack models (`%native-models`), conversation parking, `sycl-engine` field serving the giridhart-fork engine binary directly (the #606 non-finite-logits fix), `firewall-sources` tailnet restriction |
| `tailscale.scm` | `tailscale-service-type` |
| `transcriber.scm` | `transcriber-service-type` + `transcriber-configuration` — WhisperX (faster-whisper ASR + wav2vec2 alignment) on the 4090 with **SortFormer diarization**, answering "who said what" where AllTalk's Transcribe tab cannot; FastAPI upload UI + `POST /transcribe` on :8787 (comfyui VM), lazy model load + idle unload so it does not squat VRAM. Diarization is ungated (no HF token, no terms acceptance — that was the pyannote requirement) and runs on CPU via `transcriber-diarizer.py`; the ONNX checkpoint downloads itself into `<cache-dir>/onnx` on first use. `summarize=true` asks strata for markdown minutes incl. agreed tasks + owners (a field in json output, a `---` section in txt; srt/vtt cannot carry prose). **The service writes no files** — comfyui is a processor, not storage, so persistence belongs to the client: `configs/bin/transcribe` writes to `-o`, else `$TRANSCRIBE_OUT_DIR`, else stdout. `transcriber-sync` one-shot builds the venv on first boot |
| `transcriber-diarizer.py` | numpy+onnxruntime port of NVIDIA SortFormer v2.1's streaming diarization state machine (mel front end, FIFO→speaker-cache compression, silence profile, hysteresis binarization, slot merging by embedding similarity) — the file `transcriber-server.py` loads via `#:diarizer-file`; no NeMo, no pyannote, no HF token |
| `transmission.scm` | `transmission-service-type` |
| `vault.scm` | `vault-service-type` |
| `vllm.scm` | `vllm-service-type` |
| `wireguard-socks5.scm` | `wireguard-socks5-service-type` — namespaced WireGuard + microsocks SOCKS5 split tunnel (claude-workstation; see `.pi/skills/wireguard-socks5/`) |

### `peteches/home/services/`

Home service *types*, folded in from the retired `peteches` channel.

| File | Provides |
|---|---|
| `aws.scm` | `home-aws-service-type` |
| `desktop.scm` | `home-desktop-service-type` |
| `emacs.scm` | `home-emacs-base-service-type` |
| `ezlocalai.scm` | `ezlocalai-direct-home-service-type` |
| `firefox.scm` | `firefox-service-type` — profiles, `firefox-profile-launcher.sh` co-located for `local-file` |
| `git.scm` | `home-git-service-type` |
| `hyprland.scm` | `home-hyprland-service-type` |
| `koboldcpp.scm` | `koboldcpp-service-type` (home variant) |
| `mako.scm` | `home-mako-service-type`. **Unused** — no longer composed into `base-services`; see the `home/modules/mako.scm` row |
| `mpv.scm` | `home-mpv-service-type` |
| `nyxt.scm` | `nyxt-service-type` |
| `password-store.scm` | `home-password-store-service-type` |
| `wofi.scm` | `wofi-service-type` |

### Other notable files

| File | Purpose |
|---|---|
| `peteches/machines.scm` | Named `machine` records + `%all-machines` list — `host-name` is each VM's Tailscale MagicDNS name (`<host>.spaniel-cordylus.ts.net`), not its LAN IP |
| `peteches/repository.scm` | `repo-directory` / `source-path` — resolve repo assets through `%load-path` |
| `scripts/deploy.scm` | `guix deploy` wrapper — parses `--hosts` patterns, filters `%all-machines`, passes result via `-e`. Keeps its own `%machine-names` alist that must be updated alongside `machines.scm` |
| `scripts/sync-restic-keys.sh` | Syncs restic backup keys to all VMs |
| `proxmox-vms.org` | VM inventory: IPs, VMIDs, purpose — authoritative IP reference |
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
| `docs/fleet-deployment.org` | Fleet deploy rules: when to deploy, `deploy-vms` skill usage, per-host success verification, rollback, consistency audit |
| `docs/media-normalisation.org` | Media-pipeline operations runbook: the payload that actually grabs a release through Radarr 6.3's API (and the shapes that don't), clearing NZBGet's duplicate history for a re-grab, and where Radarr's exceptions really land (`logs.db`, not `/api/v3/log`) |
| `docs/rules-lawyer-loop-plan.org` | **Unimplemented plan** — fix for the rules-lawyer bot's "Stopped after N tool iterations" failure (missing `systems/**/AGENTS.md` in the package + no forced final answer in the bot harness) |
| `docs/openvino-think-budget.md` | **Post-mortem, nothing deployed** — why the LFM2.5 `llm` provider returns an empty answer when the model's `<think>` block never closes, and the two mechanisms that were built and measured to fix it (streamer-abort + forced-close re-prompt; xgrammar-constrained decoding) and why both were removed. Includes what was ruled out (sampling, prompt size to 64k, multi-turn structure, `apply_chat_template`, a replay of the actual failing session) and how to classify a recurrence from the Loki log |
| `docs/koboldcpp-vulkan-lfm2.md` | **Validated fallback, not adopted** — running LFM2.5-24B GGUF on the Arc Pro B70 through `koboldcpp-nocuda-bin`'s Vulkan backend, which is the only local path with an engine-enforced reasoning budget. Measured VRAM/load/throughput against the OpenVINO int8 service, the two quants compared, the flags that matter (`--usevulkan --jinja`), and the VRAM reason the two cannot coexist |
| `.pi/skills/update-channels/` | Skill for updating pinned channel commits across all `peteches/channels/*.scm` files |
| `.pi/skills/deploy-vms/` | The fleet-deploy skill `docs/fleet-deployment.org` mandates for anything touching more than one host |
| `.pi/skills/wireguard-socks5/` | claude-workstation's split-tunnel + SOCKS5 runbook; account-agnostic since Claude left that box |
| `.pi/agents/` | Repo-local pi subagents (`guix-validator`, `secrets-sentinel`, `service-reviewer`, `vm-auditor`) — see AGENTS.md's "Subagent workflow" section |

## System Configurations (`peteches/systems/`)

Two base constructors exist for different machine classes:

**`base.scm`** exports `make-base-os` — desktop/laptop systems with Hyprland, greetd, libvirt, virbr0, Tor, fingerprint, etc. Used by `dagon.scm`, `nyarlothotep.scm`.

Key `make-base-os` flags:
- `laptop?` — enables TLP and thermald
- `intel-cpu?` (default `#t`) — adds intel-microcode and IOMMU kernel args
- `with-nvidia?` — adds NVIDIA driver, firmware, kernel modules, Wayland env vars
- `with-docker?` — enables containerd + Docker services
- `with-bluetooth?`, `with-printing?` — optional services
- `with-nonguix?` — registers nonguix substitute server

`without-gdm` strips GDM from `%desktop-services` and configures guix-build's substitute server (`guix-build.spaniel-cordylus.ts.net:3000`). All systems use gtkgreet inside cage as the greeter, launching a Hyprland session.

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
- `with-nug-offload?` (default `#t`) — enable build offload to guix-build (nug's build-offload successor; kept this name for minimal diff churn, see `peteches/systems/common.scm`). **Requires both** a `guix-offload-key` sops-secret on the VM *and* its `guix-offload` public key in `guix-build.scm`'s authorized-keys. Several VMs have only one half; offload then fails and falls back to local builds

VM system files should `(define-public <name>-os ...)` and end with `<name>-os` as the final expression so both `guix system build FILE` and `guix deploy` (via module import) work.

All VMs use `/dev/vda` as the root disk. The LAN is a **/23** (`192.168.50.0/23`), so VM addresses are written `192.168.51.x/23` — the gateway is `192.168.50.1`. See `proxmox-vms.org` for IPs and VMIDs, and `docs/infrastructure.org` for the Terraform provisioning workflow.

The base firewall (`%vm-base-firewall`) has a **drop** input policy and opens only ssh (22), node-exporter (9100) and ICMP. A service that starts but is unreachable is usually a missing `simple-service … firewall-service-type` extension — see `git.scm` or `rustdesk.scm` for the pattern.

## Home Configurations (`peteches/home/`)

`peteches/home/modules/base.scm` is the orchestrator: it exports `base-packages`
and `base-services`, composing feature modules from `peteches/home/services/`
in this repo (imported as `(peteches home services ...)`) — emacs, git,
hyprland, aws, nyxt, wofi, and more. The focused modules alongside it
(`ssh.scm`, `gpg.scm`, `theming.scm`, `ai.scm`, `claude.scm`, …)
supply configuration values.

`peteches/home/configs/dagon.scm` and `nyarlothotep.scm` append host-specific
extras and each evaluate to a bare `home-environment` record.

Non-Scheme assets under `configs/` are located via `repo-directory` /
`source-path` from `(peteches repository)`, which searches `%load-path` —
never by relative path, which would break under `-L .` and in worktrees.

## Channels (`peteches/channels/`)

`base.scm` exports `%base-channels` — pinned: `sops-guix`, `guix-science`,
`guix-science-nonfree`, `nonguix`, `guix` itself.
`dagon.scm` adds `guix-hpc-non-free` on top.

There used to be a `peteches` channel here too (codeberg.org/peteches/guix-
channel), providing the custom packages, home services and system services
this repo consumes as `(peteches packages …)`, `(peteches home services …)`
and `(peteches services …)`. Codeberg changed its ToS to prohibit
AI-generated code, and since this repo is developed with AI coding agents, that
channel's contents were folded back into this repo instead — see
`peteches/packages/`, `peteches/home/services/` and `peteches/services/`
above. There is no pin to update and no commit to re-pin any more; editing a
service type is just editing the file.

`critical-grind` is the Critical Grind application repository, which is itself
a channel supplying `(critical-grind packages campaign)` and
`(critical-grind services campaign)`. It has **no channel introduction**, so
commits are not signature-verified and every pull warns. Releasing a new
version of the app is a commit bump in `critical-grind.scm` and
`deploy-critical-grind.scm` — `scripts/bump-channel.sh` does exactly this —
and there is nothing to build by hand.

It is a **private GitHub repo fetched over SSH**
(`git@github.com:peteches/critical-grind-battlefronts.git`). That is why it
lives in its own dedicated file (`critical-grind.scm`) rather than in
`base.scm`/`manual.scm`: `guix/git.scm` authenticates git fetches with
`(%make-auth-ssh-agent)` and nothing else — it never reads `~/.ssh/config` or a
key file — so an `ssh://` channel URL requires the key loaded in an agent, and
only machines that load the read-only deploy key into an agent (the CI deploy
task; optionally claude-workstation) should pull a channel they can
authenticate. Exposing every machine's routine `guix pull` to it would fail
with a `remote rejected authentication` message that names neither ssh-agent
nor the key.

**Pins are duplicated across files** — the routine set across
(`base.scm`, `dagon.scm`, `manual.scm`) and the critical-grind pin across
(`critical-grind.scm`, `deploy-critical-grind.scm`) — with nothing enforcing
agreement. Update them together — use the `/update-channels` skill.

For `guix pull -C` or symlinking to `~/.config/guix/channels.scm`, use
**`manual.scm`** (the full plain list).

