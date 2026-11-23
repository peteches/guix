# OpenVINO (comfyui) deploy fixes — pending

Captured 2026-11-22 while manually getting an OpenVINO instance serving on the
comfyui box (port 8000, strata's port). The manual run proved the model + runtime
work and surfaced the concrete bugs that make the **deployed shepherd service**
fail. These are the fixes to apply to the repo so the service works once deployed.

**Status of the manual instance (for reference):** running as `peteches` via
`setsid nohup` over SSH, `OPENVINO_DEVICE=CPU`, listening on `0.0.0.0:8000`,
serving LFM2.5-24B-A2B int4, `/healthz` + `/v1/models` + `/v1/chat/completions`
all respond. Kill it with `pkill -f openvino-server.py`. Log: `/tmp/openvino-manual.log`.

Store paths on the box (for reference; will change on rebuild):
- openvino:   `/gnu/store/7ds740wanah3a655hhz1ss38zki49khy-openvino-2026.4.1`
- openvino-genai: `/gnu/store/r7yfzn4s84riz94ba9gnppi7473q6rqh-openvino-genai-2026.4.1.0`
- server script: `/gnu/store/q1q7ygbp96sq9aiigdmq8p6jk1dq03cv-openvino-server.py`
- profile python: `/run/current-system/profile/bin/python3.12` (3.12.15)

---

## Fix #1 — model-dir points at an empty directory (one line)

`peteches/systems/comfyui.scm`, the `openvino-service-type` block:

```scheme
(model-dir "/media/models/openvino/LFM2.5-24B-A2B/ov-int4")   ; ← EMPTY dir
```

The real, complete IR export (12.5 GB `openvino_model.bin` + tokenizer/detokenizer
IR + config) lives at:

```
/media/models/openvino/LFM2.5-24B-A2B-int4-ov/
```

Change the value to `"/media/models/openvino/LFM2.5-24B-A2B-int4-ov"`.

Note: `/media/models/openvino/LFM2.5-24B-A2B/` is the base HF safetensors (27
files) — NOT an OpenVINO IR export, and it has no `ov-int4` subdir. The
`LFM2.5-8B-A1B-int4-ov/` export also exists as a lighter fallback.

---

## Fix #2 — the service runtime environment is incomplete

The deployed generation dies on `import numpy` (ModuleNotFoundError). Getting the
manual instance to run required a **complete** environment. The working values:

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

Concrete (current box):
```
PYTHONPATH =
  /gnu/store/7ds740wanah3a655hhz1ss38zki49khy-openvino-2026.4.1/python :
  /gnu/store/r7yfzn4s84riz94ba9gnppi7473q6rqh-openvino-genai-2026.4.1.0/python :
  /run/current-system/profile/lib/python3.12/site-packages

LD_LIBRARY_PATH =
  /gnu/store/7ds740wanah3a655hhz1ss38zki49khy-openvino-2026.4.1/runtime/lib/intel64 :
  /gnu/store/r7yfzn4s84riz94ba9gnppi7473q6rqh-openvino-genai-2026.4.1.0/runtime/lib/intel64 :
  /run/current-system/profile/lib
```

Why each part is needed (all verified against the box):
- **openvino + genai `<out>/python`**: these packages install their Python bindings
  to `<out>/python/` (upstream SDK layout), NOT to site-packages. A system/home
  profile merge never looks at `<out>/python`, so the profile python cannot
  `import openvino` / `import openvino_genai` without these on PYTHONPATH.
- **profile site-packages**: carries `numpy` (an openvino propagated-input) plus the
  whole `fastapi`/`uvicorn` closure. A raw `make-forkexec-constructor` shepherd
  process gets *only* the env vars listed in the service — the profile exports no
  PYTHONPATH of its own (verified: `env -i python3` sees it unset) — so this dir
  must be named explicitly or `import numpy` dies.
- **openvino + genai `runtime/lib/intel64`**: that's where `libopenvino.so`,
  `libopenvino_genai.so` and the plugins (incl. `libopenvino_intel_gpu_plugin.so`)
  actually land. The GPU plugin additionally needs `libOpenCL.so.1`, which is in
  the profile lib (see the GPU section below).

### What the current repo code has (and what's wrong)

`peteches/services/openvino.scm`, `openvino-environment` (~line 263):

```scheme
(pythonpath (string-append "PYTHONPATH=" site-packages))
(ld-library-path "LD_LIBRARY_PATH=/run/current-system/profile/lib:/run/current-system/profile64")
```

- **PYTHONPATH is missing the two `<out>/python` dirs.** It only names the profile
  site-packages. The `pkg` and `genai` let-bindings are computed but never spliced
  into `pythonpath`. Add them:
  ```scheme
  (pythonpath (string-join
               (list (file-append pkg "/python")
                     (file-append genai "/python")
                     site-packages)
               ":"))
  ```
  (i.e. `"PYTHONPATH="` + that join). `pkg`/`genai` are bound packages here, so
  `file-append` yields a plain string at meta time — no gexp needed.
- **LD_LIBRARY_PATH names only the profile lib.** The profile lib *does* carry the
  merged openvino/genai `runtime/lib/intel64` libs (the manual run reached
  `libopenvino.so` via the profile lib), so this may be sufficient on its own — but
  spelling the two `runtime/lib/intel64` dirs explicitly (as in the working values
  above) is the robust form and matches what actually worked.

### The deploy blocker — `#{\x7d;}#: unbound variable`

Separate from the env completeness: a `guix deploy`/system build of this module has
been failing with a corrupted-symbol error (`#{\x7d;}#: unbound variable`). It was
reproduced locally in earlier turns but its current status is **unverified**. If it
still reproduces, the prime suspect is the `python-path` let-binding in
`openvino-shepherd-service` (~line 450):

```scheme
(python-path (file-append python "/bin/python3"))   ; a <file-append> object
```

...which is embedded via `#$python-path` in the start gexp. `koboldcpp.scm` (the
working reference) uses **only plain strings** in its env and does NOT embed a
`file-append` object in a gexp. The fix, if needed: compute `python-path` as a
plain string at meta time (bind the `file-append` result to a string) before
splicing, or reference the profile python by plain path.

**Verification recipe (fresh process per module, per AGENTS.md rule #3):**
```bash
cd ~/area_51/guix
export PATH="$HOME/.config/guix/current/bin:$PATH"; hash guix 2>/dev/null
guix repl -L . -- /dev/stdin <<'EOF'
(resolve-interface '(peteches services openvino))
(write "module loads OK") (newline)
EOF
```
And the real test is a `guix system build -L . --dry-run peteches/systems/comfyui.scm`
(or an actual deploy) — that's where the `#{\x7d;}#` error would surface.

---

## GPU (Arc Pro B70) — separate, still open

`device=GPU` fails at engine creation. The plugin **loads and sees the Arc B70 via
Level Zero** (`onednn_verbose ... level zero device count:1`) but dies with:
```
[GPU] ProgramBuilder build failed!
could not create an engine
... error,ocl,errcode -33,CL_INVALID_DEVICE ...
... common,error,ze,invalid gpu device in environment: index 0 ...
```

Box GPU situation (verified):
- Two render nodes: `/dev/dri/renderD128`, `/dev/dri/renderD129`.
- **NVIDIA driver is broken**: `nvidia-smi` → "Failed to initialize NVML:
  Driver/library version mismatch. NVML library version: 595.112".
- OpenCL ICD vendors (`/run/current-system/profile/etc/OpenCL/vendors/`):
  `nvidia.x86_64.icd` (→ broken driver) + `pocl.icd` (CPU). **No Intel GPU.**
- Level Zero ICD: `/etc/level_zero/vendors/intel-gpu.so` →
  `libze_intel_gpu.so.1` (the Arc B70). Loader + driver both in profile lib.
- `OCL_ICD_VENDORS=/run/current-system/profile/etc/OpenCL/vendors` is set in the
  login env.

Tried (no effect): emptying `OCL_ICD_VENDORS` to an empty dir — the plugin uses its
own linked OpenCL, so the system ICD vendors don't gate it; the Level Zero
`invalid gpu device in environment: index 0` error persists.

Next things to try for the GPU:
- Confirm which render node is the Arc B70 vs the (broken) NVIDIA, and whether the
  plugin is targeting the wrong one.
- Level Zero driver version (`intel-level-zero-gpu-26.35.39758.10`) vs OpenVINO
  plugin (2026.4.1) compatibility.
- Whether the broken NVIDIA GPU (present as a render node + OpenCL ICD) is
  poisoning the plugin's device enumeration — possibly needs the NVIDIA ICD/driver
  fixed or removed from the equation.
- The service currently sets `OPENVINO_DISABLE_GPU=1` and `CUDA_VISIBLE_DEVICES=`
  (see `openvino-environment`) — those were for the CPU-only era and must be
  dropped/relaxed for a GPU run.
