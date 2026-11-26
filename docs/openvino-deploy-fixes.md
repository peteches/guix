# OpenVINO (comfyui) deploy fixes — RESOLVED

Originally captured 2026-11-22 while manually getting an OpenVINO instance
serving on the comfyui box. **Status as of 2026-11-25: every item below is
closed in git.** Kept as a record of what the working values were and why.

| Item | Status | Where it landed |
|---|---|---|
| Fix #1 — model-dir pointed at an empty `ov-int4/` dir | **RESOLVED, superseded** | `peteches/systems/comfyui.scm` now serves `LFM2.5-24B-A2B-int8-ov` (not the int4 path this doc prescribed — `8701ff6` measured that INT4 weight compression was what broke agentic use) |
| Fix #2 — incomplete service runtime environment | **RESOLVED** | `openvino-environment` splices both `<pkg>/python` + `<genai>/python` + profile site-packages into PYTHONPATH, and both `runtime/lib/intel64` trees + profile lib/lib64 into LD_LIBRARY_PATH (`peteches/services/openvino.scm:309-327`), via the `#$(file-append …)` gexp idiom |
| Deploy blocker `#{\x7d;}#: unbound variable` | **NOT A BUG — removed** | Could not reproduce. `(file-append python "/bin/python3")` bound to `python-path` and spliced as `#$python-path` is *valid gexp data* — the "prime suspect" hypothesis was wrong. The historical failures were verification-environment artifacts: running `guix repl`/`guix build` with `/run/current-system/profile/bin/guix` (old, lacks `linux-libre-7.3` the pinned nonguix needs) instead of `~/.config/guix/current/bin`. With the pulled guix on PATH the module loads and `guix system build -L . --dry-run peteches/systems/comfyui.scm` completes |
| GPU (Arc Pro B70) — "separate, still open" | **CLOSED** | `ed6307f`: package built `-DENABLE_INTEL_GPU=ON` (`peteches/packages/openvino.scm`), `device "GPU"` live on comfyui, `OCL_ICD_VENDORS` in the service env, `intel-opencl-icd` + `intel-igc-opencl` in the profile. Root cause was oneDNN's Level-Zero backend enumerating devices through OpenCL without the ICD vendor dir |

The claim in the original draft that "the service currently sets
`OPENVINO_DISABLE_GPU=1` and `CUDA_VISIBLE_DEVICES=`" was doubly false:
`git log -S` shows those variables never existed in `openvino.scm` — they
described the ad-hoc manual shell run, not the service.

---

## Working environment (for the record)

The manual run on 2026-11-22 proved the model + runtime work with:

```
PYTHONPATH =
  <openvino out>/python :
  <openvino-genai out>/python :
  /run/current-system/profile/lib/python3.12/site-packages

LD_LIBRARY_PATH =
  <openvino out>/runtime/lib/intel64 :
  <openvino-genai out>/runtime/lib/intel64 :
  /run/current-system/profile/lib
```

Why each part is needed (all verified against the box):

- **openvino + genai `<out>/python`**: these packages install their Python
  bindings to `<out>/python/` (upstream SDK layout), NOT to site-packages; a
  profile merge never looks at `<out>/python`.
- **profile site-packages**: carries `numpy` (openvino's propagated-input)
  plus the fastapi/uvicorn closure. A raw `make-forkexec-constructor` shepherd
  process gets *only* the env vars listed in the service; the profile exports
  no PYTHONPATH of its own.
- **`runtime/lib/intel64` trees**: where `libopenvino.so`,
  `libopenvino_genai.so` and the plugins actually land.

Remaining (unchanged) caveat: **strata co-residency on the B70 is untested** —
comfyui keeps strata stopped. Follow-up: see `docs/openvino-think-budget.md`
for the thinking-block measurement and the residual ~11% empty-answer triage
recipe.
