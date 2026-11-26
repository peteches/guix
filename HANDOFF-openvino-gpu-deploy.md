# Handoff: deploy `device=GPU` to comfyui + OpenVINO GPU test on B70

> **COMPLETED / SUPERSEDED (2026-11-25).** Both goals of this handoff are
> done in git: the `device=GPU` change is deployed to comfyui and serving as
> `llm.ts.peteches.co.uk` (ed6307f), and the record-ABI-mismatch blocker
> below resolved with the channel pull (current profile: guix c947335).
> Verification against the pinned pins also requires
> `export PATH="$HOME/.config/guix/current/bin:$PATH"` — the system Guix on
> this box predates `linux-libre-7.3`. See `docs/openvino-deploy-fixes.md`
> for the closure record; kept below for history.

**Working dir:** `~/area_51/guix` (on the `agents` VM). **Target:** `comfyui.spaniel-cordylus.ts.net`.
**You are on the `agents` VM (4 cores / 3 GB). Do NOT loop — execute, then report.**

## Goal
1. Deploy the `device=GPU` system-config change to comfyui via `./scripts/deploy.scm -h comfyui`.
2. Then test whether OpenVINO's GPU plugin sees the Intel Arc Pro B70 (32 GB) on comfyui, and if so run `openvino-test.py` → `openvino-server.py` (8B smoke, then 24B benchmark).

## What is ALREADY DONE (do not redo)
- `uv` fix: `peteches/systems/comfyui.scm:78` now uses `((gnu packages rust-apps) #:select (uv))` (was `build-tools`). Confirmed in repo.
- `guix pull -C peteches/channels/manual.scm` was run on agents → **agents' guix is now `87bc27df3cc0a30004131159b7fb1e1161f0b454`** (was `64d4de2`). `~/.config/guix/current/bin/guix` reports `87bc27d`.
- **`~/.config/guix/channels.scm` was re-pointed** from a stale store symlink (old target: `/gnu/store/8rcl7scvrq13b8sicidsi3b2nlndizpy-manual.scm`, which pinned guix `b13c7c0` + nonguix `caa8c0b`) to the **repo's** `~/area_51/guix/peteches/channels/manual.scm` (guix `87bc27d` + nonguix `ee118ce90d55c9d80af11295f28d6f971369161c`). Revert if needed by re-symlinking to the old store target.
- The **nonguix blocker is FIXED**: `(nongnu packages linux)` loads OK against guix `87bc27d` + nonguix `ee118ce` (that checkout's `linux` → `linux-libre-7.3`, which guix `87bc27d` has). `comfyui.scm:192` uses `#:kernel linux` (nonguix).
- openvino + openvino-genai are already in the store on **both** agents and comfyui:
  - `/gnu/store/dr6gkjx88by6sq9ngbw0b2l3krycrcd7-openvino-2026.4.1` (176M)
  - `/gnu/store/r7yfzn4s84riz94ba9gnppi7473q6rqh-openvino-genai-2026.4.1.0` (28M)
- Model artifacts already on comfyui: `LFM2.5-24B-A2B-int4-ov/` (12.6 GB), `LFM2.5-8B-A1B-int4-ov/` (4.5 GB). CPU baseline already proven: ~6.2 tok/s plain / ~7.9 tok/s ChatHistory on 24B int4.

## THE CURRENT BLOCKER (this is where you pick up)
`guix deploy --dry-run` for comfyui **fails** with:
```
guix/ui.scm:1040:18: Throw to key `record-abi-mismatch-error' ...
  (#<record-type <network-route>>) ... "record ABI mismatch; recompilation needed"
```
thrown from `make-vm-os` in `peteches/systems/vm-base.scm:469`.

### Root cause (already traced — do not re-trace)
- `<network-route>` is a Guix record type defined in **`gnu/services/base.scm`**.
- The running guix binary (`~/.config/guix/current/bin/guix`, version `87bc27d`) loads its modules from the **`guix-module-union`** `/gnu/store/jmvd5ggzv99h234r1l1hprzx5g3fkvac-guix-module-union`. Its `gnu/` dir symlinks to the checkout **`/gnu/store/4rb7a3974fd2k08d2amr19fcs2ifj2n0-guix-87bc27df3-modules`**.
- That checkout has **1004 `.go` bytecode files** (incl. `lib/guile/3.0/site-ccache/gnu/services/base.go`, 4.6 MB). The bytecode was compiled against a **different (bootstrap) guix version** than the checkout's own source → classic **guix bootstrap ABI mismatch**. (A separate source-only checkout `/gnu/store/d34m2jgw19hmrmsjkhs66rq3k94nxnrf-guix-87bc27d` has 0 `.go` files.)
- This stale build was **already in agents' store before the pull** (`guix pull` said "nothing to be done" for guix `87bc27d`).

### PRECISE ROOT CAUSE (confirmed 2026-11-22, refines the above)
The stale `.go` comes from the **`guix-system-modules`** package
`/gnu/store/yn90nx4jghmwyfkq2vm6alrpnrmvhz1k-guix-system-modules` (a constituent
of the modules union `4rb7a397…`). That package ships **BOTH**:
- `share/guile/site/3.0/gnu/services/base.scm` — which **MATCHES** the 87bc27d
  source (7-field `<network-route>`: destination, source, device, ipv6?, gateway,
  scope, table), and
- `lib/guile/3.0/site-ccache/gnu/services/base.go` — **stale** bytecode compiled
  from a *different* (older) source with a different `<network-route>` field set.

So the package's `.go` is internally inconsistent with its own `.scm`. When guix
loads `(gnu services base)` it loads the stale `.go` → `record-abi-mismatch-error`.
**Fix = rebuild that package (or the whole guix 87bc27d bootstrap) via the daemon**
to regenerate the `.go` from the `.scm`. The store is read-only for `peteches`
(normal — the daemon writes as root); the daemon socket IS up
(`/var/guix/daemon-socket/socket`). Rebuilding the bootstrap is the sure fix but is
a long build on 4 cores / 3 GB (OOM risk). A targeted rebuild of just
`guix-system-modules` would be much faster IF it can be referenced (it is an
internal sub-package of the guix package — see "open question" below).

### What to check / try NEXT (in order)
1. **Is the guix `87bc27d` derivation a local build or a substitute download?** The guix-build substitute key is present: `/gnu/store/r8dvd6x9c8nxp2l9qnah535m1izaqqc4-guix-build-substitute-key.pub`. If the stale guix build is LOCAL, a clean substitute from the guix-build server would fix it — check whether the server has a good `87bc27d` guix build (`guix build` the guix derivation with substitutes enabled, or inspect the server).
2. **Rebuild the guix `87bc27d` derivation locally** to regenerate consistent bytecode. On 4 cores this is a long build — estimate before committing. (This is the sure fix if no clean substitute exists.)
   - The derivation is `/gnu/store/7zhs44672rp678v0q7k05d783p3yyslp-guix-87bc27df3.drv`.
3. After guix is fixed, re-run:
   ```
   export PATH="$HOME/.config/guix/current/bin:$PATH"
   cd ~/area_51/guix
   ./scripts/deploy.scm -h comfyui --dry-run     # must pass first
   ```
   then the real deploy. **`guix deploy` has NO auto-confirm flag** (only `-n/--dry-run`); confirmation is a stdin prompt — pipe `yes` (e.g. `yes | ./scripts/deploy.scm -h comfyui`) or answer interactively.

## Deploy script facts (already read — `scripts/deploy.scm`)
- Line 2: `exec guix repl -- "$0" "$@"` → uses `guix` on **PATH** (must be the new `87bc27d`).
- Final: `(apply system* "guix" "deploy" "-L" repo-root "-e" deploy-expr passthrough)`.
- `-h comfyui` → deploy-expr `(begin (use-modules (peteches machines)) (list comfyui-machine))`.
- `comfyui-machine` is in `%machine-names`.

## Commands that must be used (PATH)
`guix` on the default PATH is still the OLD `64d4de2` (via `/run/current-system/profile/bin/guix`). **Always** `export PATH="$HOME/.config/guix/current/bin:$PATH"` first so the new `87bc27d` guix runs.

## OpenVINO GPU test (after deploy succeeds)
Previous direct test on comfyui (bypassing the service) showed `ov.Core().available_devices == ['CPU']` in 5 runs — B70 NOT detected. Remaining hypotheses (unconfirmed):
- **(a) bundled/different `libze_loader.so`** (PRIMARY): the OpenVINO GPU plugin may resolve its own `libze_loader.so` instead of the one on `LD_LIBRARY_PATH`. Check with Python `ctypes.CDLL` / `/proc/self/maps` (`ldd` is NOT on comfyui).
- (b) `pocl` dependency missing (strata's runtime includes `pocl`).
- (c) loader (level-zero 1.32.0) vs driver (intel-level-zero-gpu 26.35.39758.10) version mismatch.
- (d) OpenVINO initializes level-zero differently than strata's SYCL engine.
- (e) GPU plugin silently failing to load.

Level-zero components on comfyui:
- Loader: `/gnu/store/7bjq7lnpjbnr6pyvd1f7y6a3bzzi25yy-level-zero-1.32.0/lib/libze_loader.so.1`
- Driver: `/gnu/store/jyjwjwym117fdzif6kwg57f3rsqs44pg-intel-level-zero-gpu-26.35.39758.10/lib/libze_intel_gpu.so.1`
- ICD: `/etc/level_zero.d/ze_intel_gpu.icd.json` (strata sets `ZE_ICD_FILE` to a state-dir copy)
- B70 render nodes: `/dev/dri/renderD128`, `/dev/dri/renderD129` (root:video, `crw-rw-rw-` world-writable). `peteches` is NOT in `video` group but nodes are world-writable.
- Test python: `/srv/comfyui/.venv/bin/python` (has numpy 2.2.6, CPython 3.12). No `ze_diag`/`ze_loader_test`/`ldd` on the box.

GPU test env (used before):
```
OV=/gnu/store/dr6gkjx88by6sq9ngbw0b2l3krycrcd7-openvino-2026.4.1
ZELOADER=/gnu/store/7bjq7lnpjbnr6pyvd1f7y6a3bzzi25yy-level-zero-1.32.0
ZEDRIVER=/gnu/store/jyjwjwym117fdzif6kwg57f3rsqs44pg-intel-level-zero-gpu-26.35.39758.10
ZLIB=/gnu/store/2jzflb96n50lknzn7znfq9ri4qlbifjc-zlib-1.3.1
export ZE_ICD_FILE=/etc/level_zero.d/ze_intel_gpu.icd.json
export PYTHONPATH="$OV/python"
export LD_LIBRARY_PATH="$OV/runtime/lib/intel64:$ZELOADER/lib:$ZEDRIVER/lib:$ZLIB/lib"
/srv/comfyui/.venv/bin/python -c "import openvino as ov; print(ov.Core().available_devices)"
```

## Key files
- `peteches/services/openvino.scm` (service type; `auto-start? #f` — deploy won't auto-start it)
- `peteches/services/openvino-server.py`, `openvino-test.py` (co-located; server is FastAPI/uvicorn, loopback-only by default)
- `peteches/systems/comfyui.scm` (system config; `#:kernel linux` nonguix, `uv` from rust-apps)
- `peteches/channels/manual.scm` (channel pins: guix `87bc27d`, nonguix `ee118ce`)

## Do NOT
- Do not re-verify the nonguix fix (it's confirmed working).
- Do not re-read `scripts/deploy.scm` (facts above).
- Do not enter verification loops. When blocked, report the specific error.
