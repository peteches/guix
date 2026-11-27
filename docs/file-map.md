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
| `comfyui.scm` | ComfyUI VM — RTX 4090 GPU passthrough with nonguix driver/CUDA; successor to nug's desktop-hosted comfyui. Also runs THREE OVMS instances on the Intel Arc Pro B70 (`ovms-qwen36` serving `Qwen3.6-35B-A3B-int4-ov` on :8093 → caddy's `llm.ts.peteches.co.uk`; `ovms-embed` serving OpenVINO/Qwen3-Embedding-0.6B-int8-ov under the stable alias `embeddings` on :8094 → `embeddings.ts.peteches.co.uk`; and `ovms-rerank` serving OpenVINO/bge-reranker-v2-m3-int8-ov under the alias `reranker` on :8095, tailnet-direct with no caddy route and a Cohere-shaped `/v3/rerank` API), AllTalk TTS (XTTS on the 4090) and the WhisperX transcriber (speaker-labelled transcripts, :8787). The strata engine was RETIRED 2026-11-27 (module kept, unwired) (192.168.51.206) |
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

Three files. All routine channel pins live in a single module
(`channels.scm`); `manual.scm` re-exports it for `guix pull -C`. The
`critical-grind` channel has its own dedicated entry because it needs SSH
agent authentication and is deliberately not shared with routine pulls.
Prefer updating pins via the `/update-channels` skill (it edits all relevant
files).

| File | Purpose |
|---|---|
| `channels.scm` | **Single source of truth** for all routine channel pins. Exports `%base-channels` and `%dagon-channels` as Guile modules. Used by nyarlothotep, dagon's home config, and system configs that import `(peteches channels)`. Pinned: sops-guix, guix-science, guix-science-nonfree, nonguix, guix. |
| `manual.scm` | Thin wrapper — uses `(peteches channels)` to re-export `%dagon-channels` as a plain `(list …)` expression for `guix pull -C` or symlinking to `~/.config/guix/channels.scm`. Contains no independent pin data; if you edit it, you're editing comments. |
| `critical-grind.scm` | The `critical-grind` channel's own dedicated entry — a private GitHub repo fetched over SSH, deliberately **not** in `channels.scm`/`manual.scm`; only machines that load the read-only deploy key into an agent should pull it. Exports both a module (`(peteches channels critical-grind)`) and a bare list form. No channel introduction: commits are unauthenticated. |

No more `base.scm`/`dagon.scm`/`deploy-critical-grind.scm` duplication. There
used to be three files all copying the same commit SHAs by hand, with nothing
enforcing agreement between them — we just proved that caused ABI mismatch
crashes when the system-installed guix version diverged from the pins. Now
every routine pin has exactly one copy, imported programmatically where needed.


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
| `openvino.scm` | `openvino`, `openvino-genai` — Intel OpenVINO runtime + GenAI C++/python serving stack, built from source with every FetchContent/submodule dependency vendored as its own source package (see the module comment). **CURRENTLY UNUSED by any system config** — its only consumer was `(peteches services openvino)`, deleted 2026-11-26 when the LFM2.5 shim was retired. Deliberately kept rather than deleted: it is not LFM2-specific, it is this repo's only from-source Guix packaging of the OpenVINO runtime, and it is the fallback if OVMS's bundled runtime ever becomes a problem |
| `ovms.scm` | `ovms` — Intel OpenVINO Model Server from the upstream prebuilt Ubuntu 24.04 `python_on` tarball (copy-build-system + patchelf over the whole 507 MB tree); bundles its own OpenVINO/GenAI and deliberately does not reuse the from-source `openvino` package. AVX2 required at runtime. Model-agnostic: ships streaming tool/reasoning parsers for eleven tool-call families and seven reasoning families, auto-detected per model. Wired on comfyui via `(peteches services ovms)`, serving **Qwen3.6-35B-A3B INT4** and, as second and third instances, **Qwen3-Embedding-0.6B INT8** under `--task embeddings` and **bge-reranker-v2-m3 INT8** under `--task rerank`; it replaced the now-deleted `openvino-server.py` shim |
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
| `outline.scm` | `outline-service-type` |
| `ovms.scm` | `ovms-service-type` + `ovms-server-configuration` — Intel OpenVINO Model Server (native OpenAI-compatible REST, prebuilt vendor binary from `peteches/packages/ovms.scm`). It replaced the `openvino-server.py` shim, which — along with `openvino.scm`/`openvino-service-type` and `openvino-test.py` — was **deleted 2026-11-26** when LFM2.5 was retired; there is no in-config rollback any more. Default port 8094, loopback-only, `auto-start? #f` and `add-to-profile? #f`, with raising guards against an unauthenticated exposed endpoint and against an injectable `service-name`. **Wired on comfyui as THREE instances inside ONE `(service ovms-service-type (list …))` value** (so `ovms-accounts`' dedup-by-user-name runs; separate service forms would each contribute an `ovms` account and Guix only warns — dedup verified to yield exactly one account and one group at three configs): `ovms-qwen36` on port 8093, serving `Qwen3.6-35B-A3B-int4-ov` from `/media/models/openvino/Qwen3.6-35B-A3B-int4-ov`, so caddy's `llm.ts.peteches.co.uk` upstream is unchanged and only pi's pinned model id moved — its `model-name` is pinned to the IR name because `configs/pi/defaults/models.json` depends on that exact string; and `ovms-embed` on port 8094 (`--task embeddings`), serving `OpenVINO/Qwen3-Embedding-0.6B-int8-ov` under the stable client-visible alias `embeddings` (deliberately NOT the IR name — clients ask for a role, not a checkpoint), proxied by caddy as `embeddings.ts.peteches.co.uk`; and `ovms-rerank` on port 8095 (`--task rerank`), serving `OpenVINO/bge-reranker-v2-m3-int8-ov` under the stable alias `reranker`, with NO caddy route (its only clients are fleet-internal bots dialling the tailnet name directly, and a new vhost would add a deSEC-CNAME/ACME dependency for nothing) — its API is **Cohere-shaped** (`POST /v3/rerank` → `{results:[{index, relevance_score}]}`), not OpenAI-shaped, scores are sigmoid 0–1, and documents **must be plain strings** (the `{text,title}` object form always dies in the tokenizer for this model, so the documented 10-chunks-per-document feature is unreachable; the documented 100-document cap is also not enforced in 2026.4.1 — the real ceiling is `--max_allowed_chunks`, default 10000). `ovms-serve-args` gates `--pipeline_type` and the three Continuous-Batching booleans on `ovms-generative-task?` because OVMS rejects all four at the option-parse phase under **both** `--task embeddings` and `--task rerank`; `text_generation` renders byte-identically to before, so the gate is a provable no-op for the LLM. Note the gate covers only those four — `tool_parser`, `reasoning_parser`, `max_num_seqs`, `cache_size` and `kv_cache_precision` are equally generation-side but are omitted by `maybe-option` while left at their `#f` defaults, so setting any of them on a non-generative instance risks the same parse failure. Per-instance logs land in `/var/log/ovms/<service-name>.log`, shipped to Loki by comfyui's alloy `/var/log/ovms/*.log` glob — the only fleet-visible signal for these instances (no metrics endpoint: `GET :8093/metrics` returns HTTP 400, and `prometheus.scm` has no HTTP-probe job). That means QUERYABLE, not ALERTING: no alertmanager is deployed anywhere in the fleet, `prometheus.scm`'s `rule-files` field is supported but unset, and there is no Loki ruler, so a silently-wedged servable is something a human must go and look for. Both parser fields now default to `#f` (auto-detect); for Qwen3.6 OVMS logs `Auto-detected tool_parser: qwen3coder` / `reasoning_parser: qwen3`. GPU path measured on comfyui's Arc Pro B70, OVMS 2026.4.1: Continuous Batching servable under `--pipeline_type AUTO`, ~35 s to AVAILABLE, decode 101.4–101.9 T/s, cold prefill 1 453 T/s vs 11 850 T/s effective on a repeated 44 817-token prompt (8.2× from `--enable_prefix_caching`), context ceiling 262144 verified empirically (280 017 tokens → HTTP 400), streaming `reasoning_content` before `content`, incremental `tool_calls`, streaming `usage`, `finish_reason: length`. The header also records two rejected paths with their measurements: MTP/`--draft_model_path` (mutually exclusive with prefix caching, segfaults OVMS, changes greedy output) and `--pipeline_type LM` on the retired LFM2.5 graph (clean output but no prefix caching, escalating per-turn latency, then an indefinite hang that survived SIGTERM). Two non-obvious GPU requirements are documented in the module header: `LD_LIBRARY_PATH` must reach the profile because the Intel OpenCL ICD is dlopened and has no RUNPATH, and `OCL_ICD_VENDORS` must point at a synthesised directory holding only `intel.icd` (the service builds it) or OVMS enumerates the box's RTX 4090 instead |
| `pihole.scm` | `pihole-service-type` |
| `plane.scm` | `plane-service-type` |
| `prometheus.scm` | `prometheus-service-type` |
| `prowlarr.scm` | `prowlarr-service-type` |
| `radarr.scm` | `radarr-service-type` |
| `restic.scm` | `restic-vm-backup-service-type` — restic backups over SFTP to Synology |
| `rules-lawyer.scm` | `rules-lawyer-service-type` — the rules-lawyer Discord bot (agents VM); talks to comfyui's `ovms-qwen36` OVMS instance through caddy's tailnet proxy (`llm.ts.peteches.co.uk` → :8093, model `Qwen3.6-35B-A3B-int4-ov`; repointed off the dead strata endpoint 2026-11-27) |
| `rustdesk.scm` | `rustdesk-service-type` |
| `seerr.scm` | `seerr-service-type` |
| `sillytavern.scm` | `sillytavern-service-type` — SillyTavern LLM frontend (agents VM, :8001 via caddy) |
| `sonarr.scm` | `sonarr-service-type` |
| `sops-key-generator.scm` | `sops-key-generator-service-type` — one-shot age keypair generation on first boot |
| `strata.scm` | **UNWIRED since 2026-11-27** — retired from comfyui in favour of OVMS; no system file references `strata-service-type` any more, and the module is kept for reference/possible reuse (its end-state and the UID-drift bug that had already stopped+disabled it are recorded in the file header). `strata-service-type` + `strata-configuration` — Strata MoE inference engine (Intel Arc SYCL): native-pack models (`%native-models`), conversation parking, `sycl-engine` field serving the giridhart-fork engine binary directly (the #606 non-finite-logits fix), `firewall-sources` tailnet restriction |
| `tailscale.scm` | `tailscale-service-type` |
| `transcriber.scm` | `transcriber-service-type` + `transcriber-configuration` — WhisperX (faster-whisper ASR + wav2vec2 alignment) on the 4090 with **SortFormer diarization**, answering "who said what" where AllTalk's Transcribe tab cannot; FastAPI upload UI + `POST /transcribe` on :8787 (comfyui VM), lazy model load + idle unload so it does not squat VRAM. Diarization is ungated (no HF token, no terms acceptance — that was the pyannote requirement) and runs on CPU via `transcriber-diarizer.py`; the ONNX checkpoint downloads itself into `<cache-dir>/onnx` on first use. `summarize=true` asks the co-resident `ovms-qwen36` OVMS instance (:8093, `Qwen3.6-35B-A3B-int4-ov` — comfyui sets `summarizer-base-url`/`summarizer-model` explicitly; the record's `http://127.0.0.1:8000` default is dead since strata's 2026-11-27 retirement) for markdown minutes incl. agreed tasks + owners (a field in json output, a `---` section in txt; srt/vtt cannot carry prose). **The service writes no files** — comfyui is a processor, not storage, so persistence belongs to the client: `configs/bin/transcribe` writes to `-o`, else `$TRANSCRIBE_OUT_DIR`, else stdout. `transcriber-sync` one-shot builds the venv on first boot |
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
| `scripts/deploy.scm` | `guix deploy` wrapper — parses `--hosts` patterns, filters `%all-machines`, passes result via `-e`. Keeps its own `%machine-names` alist that must be updated alongside `machines.scm`. **Derivation trap:** see the warning below this table — editing this file changes every VM's `system.drv` |
| `scripts/sync-restic-keys.sh` | Syncs restic backup keys to all VMs |
| `scripts/strata-soak.js` | **RETIRED ENDPOINT, kept for reference** — 41-request load soak for the strata engine (detects the #606 single-token `!`-loop). Defaults to `https://strata.ts.peteches.co.uk`, which no longer routes anywhere: the strata service was retired from comfyui 2026-11-27 and caddy's route was renamed to `llm.ts.peteches.co.uk` long before that. Nothing in-tree cites it. If ever revived, pass `https://llm.ts.peteches.co.uk` as argv[2] and replace the dead `model:"strata"` id with the live `Qwen3.6-35B-A3B-int4-ov`. **Do not edit casually** — see the derivation trap below |
| `scripts/strata-battery.js` | **RETIRED ENDPOINT, kept for reference** — quality battery (long prose, executed-code check, OpenAI+Anthropic tool calling, multi-turn) for the strata engine. Same dead default base URL and same revival notes as `strata-soak.js`; its executed-code check additionally assumes strata's sandboxed code-execution tool, which OVMS does not provide. **Do not edit casually** — see the derivation trap below |
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
| `docs/openvino-think-budget.md` | **Post-mortem, nothing deployed; HISTORICAL since 2026-11-26** (dated postscript at the top) — why the LFM2.5 `llm` provider returns an empty answer when the model's `<think>` block never closes, and the two mechanisms that were built and measured to fix it (streamer-abort + forced-close re-prompt; xgrammar-constrained decoding) and why both were removed. Includes what was ruled out (sampling, prompt size to 64k, multi-turn structure, `apply_chat_template`, a replay of the actual failing session) and how to classify a recurrence from the Loki log. The shim it analyses is deleted and the think-budget problem did not carry over to Qwen3.6, but its log-classification recipe is still cited by `peteches/home/modules/pi.scm`'s livelock post-mortem |
| `docs/koboldcpp-vulkan-lfm2.md` | **Validated fallback, not adopted; RULED OUT for agent work by the 2026-11-26 postscript** — running LFM2.5-24B GGUF on the Arc Pro B70 through `koboldcpp-nocuda-bin`'s Vulkan backend, which is the only local path with an engine-enforced reasoning budget. Measured VRAM/load/throughput against the OpenVINO int8 service, the two quants compared, the flags that matter (`--usevulkan --jinja`), and the VRAM reason the two cannot coexist. The postscript re-tested both LFM2.5 Q4_K_M and Qwen3.6 Q4_K and records that the missing GPU-resident prefix cache makes koboldcpp unusable for multi-turn agent work regardless of model (99 min / 48 requests unfinished vs 62 s on OVMS), plus that `--contextsize 163840` copied from the CUDA config is wrong for the B70 |
| `.pi/skills/update-channels/` | Skill for updating pinned channel commits across all `peteches/channels/*.scm` files |
| `.pi/skills/deploy-vms/` | The fleet-deploy skill `docs/fleet-deployment.org` mandates for anything touching more than one host |
| `.pi/skills/wireguard-socks5/` | claude-workstation's split-tunnel + SOCKS5 runbook; account-agnostic since Claude left that box |
| `.pi/agents/` | Repo-local pi subagents (`guix-validator`, `secrets-sentinel`, `service-reviewer`, `vm-auditor`) — see AGENTS.md's "Subagent workflow" section |

### Derivation trap: `scripts/` (and `configs/bin/`) are embedded verbatim into derivations

`peteches/packages/generation-gc.scm` builds its package from
`(local-file "../../scripts" ... #:recursive? #t)` — the **whole `scripts/`
directory**, not just `generation-gc` — and `generation-gc` is in *every*
`vm-base` VM's system profile (`#:with-generation-gc?` defaults `#t`).
`peteches/packages/proxmox-scripts.scm` embeds the same directory recursively
for the desktops' home environments. Consequence, measured 2026-11-27:
**any edit to any file under `scripts/` — including comments, and including
`scripts/deploy.scm` itself — changes every VM's `system.drv`** (a two-line
comment added to `scripts/strata-soak.js` moved the agents hash from
`cynqkcajr983sgjf6ymhz54n76cmbcym-system.drv` to
`gwxqisrh6dz7lja34c78k2b7wii0r6y2-system.drv`, proven by bisection). Such an
edit is therefore a **fleet-wide change owed a deploy to all hosts** per
`docs/fleet-deployment.org`'s blast-radius rules, even when it is cosmetic —
never add one just for documentation. Cheap check before/after any `scripts/`
edit (or any edit claimed to be derivation-neutral):

```bash
/home/peteches/.config/guix/current/bin/guix system build -L . --dry-run \
    peteches/systems/<host>.scm 2>&1 | grep -o '/gnu/store/[a-z0-9]\{32\}-system\.drv'
```

Same class, narrower blast radius: `peteches/packages/desktop-scripts.scm`
embeds `configs/bin/` recursively (desktop **home** derivations only — no VM
profile uses it), `peteches/services/transcriber.scm` embeds
`transcriber-server.py`/`transcriber-diarizer.py` via `local-file` (comfyui's
`system.drv` only), and the various `configs/pi`, `configs/hypr`,
`configs/matugen`, `configs/dms/plugins`, `configs/claude` and
`peteches/channels/manual.scm` embeds in `peteches/home/` affect the
respective home environments. `docs/` is not embedded anywhere —
`grep -rn 'local-file\|plain-file\|computed-file' peteches/` is the
up-to-date inventory.

### Derivation trap 2: a system config embeds *itself*, so its comments are not free

`peteches/systems/<host>.scm` is copied into that host's own derivation as
`configuration.scm` (visible on the box as
`/run/current-system/configuration.scm` → a store path). Consequence,
measured 2026-11-27: **a comment-only edit to a system config changes that
host's `system.drv`.** Comment edits to `comfyui.scm` moved
`jhkbzxwqxz860sjq4h9xw9ka4291cxdm-system.drv` →
`qaz3q7522ay6rx74pmbgypb78d5zvn2k-system.drv` with `git diff -U0` showing
**zero** non-comment lines changed, and the new comment text is present
verbatim in the deployed `configuration.scm`.

So the phrase "comment-only, therefore derivation-neutral" is **false** for:
`peteches/systems/*.scm` (this trap), anything under `scripts/` or
`configs/bin/` (trap 1), and any `.py`/asset pulled in via `local-file`.
It remains **true** for `docs/`, `*.md`, `*.org`, and for library modules such
as `peteches/services/*.scm` and `peteches/packages/*.scm`, which Guix
*evaluates* rather than embeds — editing a comment there changes nothing.

This is cosmetic, not functional: the rebuilt derivation produces an
identical running system, and the only cost is that the host shows as owing a
deploy until someone reconfigures it. It matters because it invalidates the
common shorthand "it's only a comment", and because a fleet-consistency audit
comparing `system.drv` hashes will flag such a host as drifted. Verify with
the `--dry-run` hash check above before claiming neutrality — do not reason
from the diff.

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
version of the app is a single commit bump in `critical-grind.scm` —
`scripts/bump-channel.sh` updates it — and there is nothing to build by hand.
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

**Pins live in a single file** — `channels.scm` contains all routine channel
pins. The `/update-channels` skill edits them there, and other files import
programmatically via `#:use-module`. No more manual synchronization or drift.

For `guix pull -C` or symlinking to `~/.config/guix/channels.scm`, use
**`manual.scm`** (the thin wrapper). For standard `guix pull`, it works directly
because guix installs channel modules into your profile automatically — the old
setup needed `guix pull -C` plus explicit `-L` flags for every channel, which
caused ABI mismatches when the pulled guix version differed from the system one.

