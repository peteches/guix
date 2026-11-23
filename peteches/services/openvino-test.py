"""First real inference test for the Guix-built openvino + openvino-genai packages.

Run on comfyui against store paths copied there with `guix copy' (no profile, no
service, no reconfigure): this exercises exactly the environment peteches/services/
openvino.scm builds for the shepherd service, minus fastapi/uvicorn.  CPU only, a
deliberate thread cap, and nice(19) so strata keeps priority on the box.

Usage: ovtest.py MODEL_DIR [THREADS]
"""

import os
import resource
import sys
import time

MODEL_DIR = sys.argv[1]
THREADS = sys.argv[2] if len(sys.argv) > 2 else "8"
DEVICE = os.environ.get("OPENVINO_DEVICE", "CPU")

import openvino as ov
import openvino_genai as ov_genai


def rss_mb():
    return resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1024.0


print(f"openvino devices: {ov.Core().available_devices}", flush=True)
print(f"genai version: {ov_genai.get_version()}", flush=True)

props = {"INFERENCE_NUM_THREADS": THREADS}
pipe = None
for how, make in (
    ("kwargs", lambda: ov_genai.LLMPipeline(MODEL_DIR, DEVICE, **props)),
    ("config-dict", lambda: ov_genai.LLMPipeline(MODEL_DIR, DEVICE, props)),
    ("none", lambda: ov_genai.LLMPipeline(MODEL_DIR, DEVICE)),
):
    try:
        t0 = time.time()
        pipe = make()
        print(f"pipeline via {how} in {time.time() - t0:.1f}s "
              f"(peak RSS {rss_mb():.0f} MB)", flush=True)
        break
    except Exception as exc:
        tail = [l for l in str(exc).splitlines() if l.strip()]
        print(f"  {how} rejected: {type(exc).__name__} "
              f"{(tail[-1] if tail else str(exc))[:90]}", flush=True)
if pipe is None:
    sys.exit("no construction shape worked")

print(f"has set_property: {hasattr(pipe, 'set_property')}", flush=True)

cfg = ov_genai.GenerationConfig()
cfg.max_new_tokens = 48
cfg.do_sample = False


def timed(label, thunk):
    t0 = time.time()
    try:
        out = thunk()
    except Exception as exc:
        print(f"{label}: {type(exc).__name__}: {str(exc)[:160]}".replace("\n", " | "),
              flush=True)
        return None
    dt = time.time() - t0
    kind = type(out).__name__
    text = getattr(out, "text", None)
    if not isinstance(text, str):
        texts = getattr(out, "texts", None)
        text = texts[0] if isinstance(texts, (list, tuple)) and texts else out
    ntok = len(getattr(out, "tokens", []) or []) or 48
    print(f"{label}: ok in {dt:.2f}s via {kind} (~{48 / dt:.1f} tok/s), "
          f"peak RSS {rss_mb():.0f} MB", flush=True)
    print(f"--- {label} output ---")
    print(text if isinstance(text, str) else repr(text)[:300], flush=True)
    return out


timed("plain-prompt", lambda: pipe.generate(
    "Question: What is the capital of France?\nAnswer:", generation_config=cfg))

ch_factory = getattr(ov_genai, "ChatHistory", None)
print(f"binding exposes ChatHistory: {ch_factory is not None}", flush=True)
if ch_factory is not None:
    timed("chathistory", lambda: pipe.generate(
        ch_factory([{"role": "user", "content": "Say OK."}]), generation_config=cfg))

timed("message-dicts", lambda: pipe.generate(
    [{"role": "user", "content": "Say OK."}], generation_config=cfg))

timed("flattened-text", lambda: pipe.generate(
    "<|startoftext|>user\nSay OK.<|im_end|>\n<|startoftext|>assistant\n",
    generation_config=cfg))
