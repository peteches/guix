# Running LFM2.5-24B on the Arc Pro B70 with koboldcpp (Vulkan)

Status: **validated, not adopted.** Proven working on comfyui 2026-11-24 as a
fallback for the OpenVINO-served `llm` provider. Nothing here is deployed; the
GGUFs are on the box and the binary is already packaged.

This exists because koboldcpp has the one thing the OpenVINO stack does not: a
**reasoning budget the engine actually enforces**. See
`docs/openvino-think-budget.md` for why OpenVINO cannot grow one.

## Why this was not obvious

`peteches/packages/koboldcpp.scm` ships **prebuilt binaries**, and only two:
`koboldcpp-bin` (CUDA) and `koboldcpp-nocuda-bin` (CPU). Neither name suggests it
can drive an Intel Arc. But upstream's README says the nocuda build carries "CPU
**and Vulkan**" support, and `--usevulkan` is in its `--help`. comfyui has the
Intel ANV ICD installed (`intel_icd.x86_64.json` in the system profile), so
`koboldcpp-nocuda-bin` reaches the B70 through Vulkan.

Two traps worth knowing:

- The CUDA variant is useless here even though comfyui **also has an RTX 4090**
  (`lspci` shows both `8086:e223` Battlemage and `10de:2684` AD102). That GPU's
  driver is currently broken — `nvidia-smi` reports `Failed to initialize NVML:
  Driver/library version mismatch` — and the 4090 is ComfyUI's anyway.
- `koboldcpp.service` on comfyui defaults to `koboldcpp-bin`, i.e. the CUDA
  build, which is presumably why it is stopped. Anything reviving it should point
  at `koboldcpp-nocuda-bin` and pass `--usevulkan`.

## Models

LiquidAI publishes official GGUFs (`LiquidAI/LFM2.5-24B-A2B-GGUF`), and
`llama.cpp` supports the architecture: it reports `arch = lfm2moe`. Both quants
are on the box under `/media/models/koboldcpp-test/`:

| file | size | note |
|---|---|---|
| `LFM2.5-24B-A2B-Q4_K_M.gguf` | 13.53 GB | recommended; indistinguishable from Q8 on every test run |
| `LFM2.5-24B-A2B-Q8_0.gguf` | 23.74 GB | quality parity with the OpenVINO int8 export, ~8% slower |

There is also `LFM2.5-VL-3B-GGUF` upstream if vision is ever wanted — relevant
because `VLMPipeline` is compiled into the OpenVINO build but the served model is
text-only, so vision needs a different model either way.

## How to run it

```bash
K=/gnu/store/…-koboldcpp-nocuda-bin-1.118.1/bin/koboldcpp
$K --model /media/models/koboldcpp-test/LFM2.5-24B-A2B-Q4_K_M.gguf \
   --usevulkan --gpulayers 999 \
   --contextsize 32768 --genlimit 8192 --quantkv q8_0 \
   --jinja --reasoningeffort medium \
   --host 127.0.0.1 --port 5099 --threads 8
```

`--jinja` matters: without it the reasoning split is unreliable (one run leaked a
literal `<think>` into `content` and ran to the cap). Server comes up in **9–21 s**.

**It cannot run alongside the OpenVINO service.** 24.01 GB of int8 weights plus
13.85 GB of GGUF is 38 GB against the B70's 31.9 GiB. Testing means
`sudo herd stop openvino-lfm2` first, and the Arc needs ~10 s to release VRAM.

## Measured

Q4_K_M unless stated. Vulkan device as reported: `Intel(R) Graphics (BMG G31)
(Intel open-source Mesa driver) | fp16: 1 | warp size: 32 | matrix cores:
KHR_coopmat`.

| | OpenVINO int8 (Level Zero) | koboldcpp Q4_K_M | koboldcpp Q8_0 |
|---|---|---|---|
| weights VRAM | 24.01 GB | **13.85 GB** | 24.30 GB |
| KV buffer @ 33k ctx | dynamic, ~9.5 GiB budget | **343 MiB** | 343 MiB |
| model load | 56–70 s | **9–12 s** | 21 s |
| native context | 131072 | 131072 | 131072 |
| decode, short prompt | 112–140 tok/s | 75–130 tok/s | 69–104 tok/s |
| decode, 16k prompt | **122 tok/s** | 67 tok/s | 62 tok/s |
| reasoning params | **ignored entirely** | honoured | honoured |

The 343 MiB KV buffer is the hybrid architecture paying off: LFM2 keeps
recurrent state instead of a growing KV cache, so context is nearly free. That is
why it can hold 131k on a card where a transformer of the same size could not.

**Reasoning budget, the reason to care.** Same short prompt, four settings,
reasoning chars returned:

| | none | `reasoning_effort=low` | `=high` | `reasoning_budget_tokens=64` |
|---|---|---|---|---|
| OpenVINO int8 | 1023 | 1023 | 1023 | 1023 |
| koboldcpp Q8_0 | 1277 | **687** | **1471** | **902** |
| koboldcpp Q4_K_M | 868 | 642 | 720 | 579 |

OpenVINO returns byte-identical output for all four — it has no concept of a
reasoning budget. koboldcpp separates `reasoning_content` from `content` and
scales the former on request.

**Quality.** Both quants scored 5/5 on a short battery (capital, bat-and-ball
trap, arithmetic, exact JSON, `is_palindrome`) and both answered a 16k-token
document-analysis question correctly and specifically. Q4 is 43% smaller and ~8%
faster; nothing measured justifies Q8's extra 10 GB.

## Unresolved

**Q8_0 + `--jinja` failed an agentic tool loop** — six identical
`count_lines(fruit.txt)` calls, correct result returned every time, never
answering — while Q8_0 *without* `--jinja` did it in two calls, and Q4_K_M did it
correctly with and without. That is the signature of a model that never sees the
tool result, i.e. a template rendering problem, not a quant problem. But every
cell was a **single sample at temperature 0.6 with no fixed seed**, so it may be
noise. It needs 5–10 repeats per cell before anyone acts on it.

**`reasoning_budget_tokens` is not to be trusted yet.** One 24B run with
`reasoning_budget_tokens=1024` on a 16k prompt leaked the think block into
`content` and ran to the cap, while the same parameter at 64 tokens worked on
both quants. `configs/pi/defaults/models.json` currently sets
`compat.reasoning_budget_tokens: 12288` for the koboldcpp provider. That config
is inert while the service is stopped; re-test it at production prompt sizes
before reviving koboldcpp.

## If it is ever adopted

1. Point `koboldcpp-configuration-package` at `koboldcpp-nocuda-bin` and add
   `--usevulkan --jinja` plus the sizing flags above.
2. Decide what happens to `openvino-lfm2` — they cannot share the card. Either
   OpenVINO steps down as the `llm` provider, or it moves to the int4 export
   (which `peteches/systems/comfyui.scm` documents as having destroyed agentic
   behaviour, so probably not).
3. Repeat the agentic cell N times per quant × `--jinja` combination first.
4. `configs/pi/defaults/models.json` would need the `llm` provider's
   `contextWindow`/`maxTokens` and `thinkingLevelMap` revisited — koboldcpp
   reserves its generation budget *inside* the context window, which is why the
   existing entry declares `contextWindow = --contextsize - --genlimit`.
