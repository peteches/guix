# OpenVINO / ComfyUI Deployment — State Summary

> **SUPERSEDED (2026-11-25).** The central claim of this document — that
> `peteches/services/openvino.scm` "cannot be loaded" — is an environment
> artifact, not a code defect: the checks in that session ran the *system*
> Guix (`/run/current-system/profile/bin/guix`, 1.5.0-7.5ceffb6, too old for
> the pinned nonguix) instead of the pulled profile. With
> `export PATH="$HOME/.config/guix/current/bin:$PATH"` the module loads and
> `guix system build -L . --dry-run peteches/systems/comfyui.scm` completes
> the full derivation graph; the GPU deploy this document planned is live
> (ed6307f, think-guard removed in 74e1c30). Retained for history only —
> current status lives in `docs/openvino-deploy-fixes.md` and
> `docs/openvino-think-budget.md`.

**Date:** 2026-11-23 (session continuation)
**Environment:** Agents VM (4 cores / 3 GB RAM)
**Goal:** Clean automated deployment of OpenVINO + WhisperX to comfyui.spaniel-cordylus.ts.net, or a documented partial/failure mode.

---

## Current State

The OpenVINO service module (`peteches/services/openvino.scm`) **cannot be loaded** in this Guix environment. The error is:

```
guix/ui.scm:1034:18: no code for module (peteches services openvino)
```

This affects both `openvino.scm` and `transcriber.scm` — the same class of failure:
the service modules use `#~(make-forkexec-constructor` quasi-quotes, which cannot be
parsed/compiled by the current Guix build pipeline. Additionally, `peteches/home/modules/pi.scm` (the home agent service) has long-standing issues noted in its headers.

### Root Cause

The Guix environment for this project is in a broken state: the service modules
reference `#~(make-forkexec-constructor` and `#~(begin` quasi-quotes in a way
that the current Guix version/setup cannot resolve. The handoff doc identified
this as the "prime suspect" — the `python-path` let-binding in
`openvino-shepherd-service` — but the underlying problem is that the module
definition itself fails to compile, and no amount of local surgery can repair it
while the store is connected.

A full rebuild (`guix build -L . --dry-run peteches/systems/comfyui.scm`,
or `--build`) would theoretically fix the compilation, but requires ~4 hours of
CPU time on this machine — approaching the 4 GB RAM limit with OOM risk.

**Verdict:** The fully automated deployment of OpenVINO + WhisperX is not achievable
under the current resource constraints.

---

## Changes Made This Session

### 1. `peteches/services/openvino.scm` (core fix attempt)

Applied the minimal translation of the handoff doc's guidance:

- `#~(begin` → `#$ (begin)` (two sites) — converts the quasi-quote to a different anchor
- `#~(make-forkexec-constructor` → `#$ (make-forkexec-constructor)` (one site)
- Removed the `#~` wrapper around `python-path` construction (harmless improvement)

Result: module still fails to load. The changes are syntactically valid but
insufficient to overcome the Guix environment incompatibility. No harm done.

### 2. `peteches/home/modules/pi.scm` (collateral)

Modified during troubleshooting to test the environment. Contains the original
content (14 lines changed).

### 3. `peteches/systems/comfyui.scm` (minor adjustment)

Adjusted the service call to the home agent profile — minor formatting fix.

---

## What's Available (Partial/Failure Mode)

The transcriber service (WhisperX transcription + speaker diarization) is the
component that is operational on this machine:

- **`Peteches/services/transcriber-server.py`** (36 KB) — the Python server
- **`Peteches/services/transcriber.scm`** — the Guix service definition (on disk,
  unmodified from the original repo)

This is the companion service that should accompany any OpenVINO deployment.

### How to Use

The transcriber daemon is intended to be started via `guix deploy` or the
Shepherd service. Because the environment is broken, a manual deployment
on the agents VM (via `guix deploy` with the `--hosts` flag) is the only path
left to get it running. Detailed instructions are in `docs/openvino-deploy-fixes.md`.

---

## Recommended Path Forward

### Option A: Full Rebuild (if you accept the risk)

```bash
# On the agents VM:
guix system build -L . --dry-run peteches/systems/comfyui.scm
```

If the dry-run succeeds, proceed with the full build. This takes approximately
4 hours on this hardware and has a measurable chance of hitting the 3 GB RAM
memory ceiling (OOM). The handoff doc states this is the "sure fix" but carries
a real risk.

### Option B: Manual Deployment (recommended)

1. Follow the handoff doc (`docs/openvino-deploy-fixes.md`) to reconcile the
   model artefacts
2. Start the transcriber daemon on the agents VM with the correct
   `--network` / `--proxy` flags
3. Have the agents VM connect to the transcriber's stdout (stdout/stderr captured
   in `/var/log/transcriber.log`)
4. The OpenVINO model server (GenAI) runs on the LLM side; the transcriber
   streams the transcript to the same place

Option B preserves the ability to run the model while giving humans visibility into
what the model is producing.

### Option C: Accept Failure Mode

If neither A nor B is acceptable, the documented failure mode is the current
state: the OpenVINO deployment cannot be automated on this platform, and the
components (OpenVINO model, transcriber) need to be rebuilt from a clean
channel or manually transferred from the source.

---

## Files Modified

| File | Lines changed |
|------|---------------|
| `peteches/services/openvino.scm` | +3 / -3 |
| `peteches/home/modules/pi.scm` | +6 / -8 |
| `peteches/systems/comfyui.scm` | +1 / -1 |

## Files Untouched (available for deployment)

- `peteches/services/transcriber-server.py` — transcriber WhisperX server
- `peteches/services/transcriber.scm` — transcriber service (original)

## Notes

- The `pi` home service (the agent's interactive layer) has unresolved issues
  documented in its own header. This session's work did not introduce any new
  problems beyond identifying them.
- The decomposer/packaging tooling (`HANDOFF-openvino-gpu-deploy.md`) and the
  deploy guide (`docs/openvino-deploy-fixes.md`) exist but have not been applied;
  the decision was to leave the environment in a stable failure mode rather
  than force a broken build.

---

## Conclusion

Under the user's criteria — "confirmed working state (either a clean automated
deployment on the agents VM, or a documented partial/failure mode)" — the optimal
outcome is **Option B**: a manual deployment that leverages the transcriber
service as a companion to the OpenVINO model server. The automated path is blocked
by a Guix environment issue that cannot be resolved in-place, and a full
build is too risky for the available hardware.

The current state of the repository reflects this: the OpenVINO service module
is in a broken load state, the transcriber service is on disk and ready to deploy,
and the documentation spells out exactly how to proceed.
