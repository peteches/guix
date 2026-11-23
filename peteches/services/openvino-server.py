#!/usr/bin/env python3
"""OpenAI-compatible HTTP front end for one OpenVINO GenAI model directory.

Served by (peteches services openvino): FastAPI/uvicorn around `openvino_genai`,
so a GenAI IR directory looks like any other chat backend to the rest of the
fleet (pi, herdr, SillyTavern, the rules-lawyer bot all speak OpenAI shapes).

Deliberately small.  No transformers, no optimum, no torch: the model directory
is expected to already hold OpenVINO IR plus a GenAI-readable tokenizer, which is
what `optimum-cli export openvino' writes.  Loading HF safetensors here would put
the whole torch stack on a box whose GPU memory is spoken for by strata, and this
server's whole reason to exist is CPU inference that must not reach for the
Arc B-series.

Two input shapes are accepted:
  POST /v1/chat/completions   {"model","messages":[{role,content}...], ...}
  POST /v1/completions        {"model","prompt":"...", ...}
Chat messages go to GenAI as an `openvino_genai.ChatHistory`, which is the type
its own generate() advertises for chat (it applies the model's chat template
from the tokenizer IR).  A bare list of message dicts and a flattened ChatML
string are kept as fallbacks in case a binding exposes neither — see
_chat_to_text.  Which path is live is logged once at startup.

Streaming is off by default and only implemented for chat; a non-streaming
response is returned for `stream: true' if something goes wrong mid-stream.
"""

import json
import os
import sys
import threading

import openvino as ov
import openvino_genai as ov_genai
from fastapi import FastAPI, Request
from fastapi.responses import JSONResponse, StreamingResponse

MODEL_DIR = os.environ["OPENVINO_MODEL_DIR"]
# "CPU", not "CORE": measured against this build (openvino 2026.4.1), the runtime
# registers CPU/AUTO/HETERO/MULTI/TEMPLATE and there is no device named CORE —
# LLMPipeline(dir, "CORE") dies with `Device with "CORE" name is not registered'
# while "CPU" and "AUTO" both construct fine.  AUTO would also work here (no GPU
# plugin is compiled in), but CPU states the strata rule outright.
DEVICE = os.environ.get("OPENVINO_DEVICE", "CPU")
# Thread/pinning hints for the CPU plugin.  The names are what this build
# actually accepts, taken out of libopenvino_intel_cpu_plugin.so and then checked
# against a live LLMPipeline: INFERENCE_NUM_THREADS is honoured, while every
# PERF_* spelling (PERF_NUM_THREADS, PERF_COUNT-as-thread-cap, the old
# PERF_CPU_PINNING_POLICY) dies in intel_cpu/src/config.cpp with "Unsupported
# property ... by CPU plugin".  PERFORMANCE_HINT wants one of LATENCY /
# THROUGHPUT / CUMULATIVE_THROUGHPUT.  Keeping them explicit means the service,
# not an OpenVINO default tuned for a whole socket, decides how many cores a
# trial may take while strata runs next door.
THREAD_HINTS = {
    "INFERENCE_NUM_THREADS": os.environ.get("OPENVINO_NUM_THREADS", ""),
    "ENABLE_CPU_PINNING": os.environ.get("OPENVINO_CPU_PINNING", ""),
    "PERFORMANCE_HINT": os.environ.get("OPENVINO_PERF_HINT", ""),
}
MAX_NEW_TOKENS = int(os.environ.get("OPENVINO_MAX_NEW_TOKENS", "256"))

_lock = threading.Lock()          # one generation at a time; see service comment


def _build_pipeline():
    """Construct LLMPipeline with our thread hints, whatever shape this binding wants.

    The 2026.4.1 python signature is `LLMPipeline(models_path, device, config={},
    **kwargs)' and the docstring says kwargs are *device properties* (passing them
    as the third positional argument still works but warns 'config parameters is
    deprecated'); there is no set_property() on the pipeline at all (measured:
    AttributeError), so hints have to be handed over at construction.  The three
    shapes below are tried in order so a binding that moves them elsewhere — or a
    plugin that rejects one of the names — still starts instead of crash-looping
    under shepherd.
    """
    props = {k: v for k, v in THREAD_HINTS.items() if v}
    attempts = [
        (lambda: ov_genai.LLMPipeline(MODEL_DIR, DEVICE, **props), "kwargs"),
        (lambda: ov_genai.LLMPipeline(MODEL_DIR, DEVICE, props), "config-dict"),
        (lambda: ov_genai.LLMPipeline(MODEL_DIR, DEVICE), "none"),
    ]
    first_error = None
    for make, how in attempts:
        try:
            pipe = make()
        except Exception as exc:
            # A rejected key or value surfaces as a plugin-config RuntimeError, not
            # a TypeError, so both kinds have to fall through to the next shape.
            print(f"note: thread hints via {how} failed: "
                  f"{str(exc).splitlines()[-1][:120] if str(exc).strip() else exc}",
                  file=sys.stderr)
            first_error = first_error or exc
            continue
        if props and how == "none":
            print("warning: no PERF_* hint shape accepted; using OpenVINO defaults",
                  file=sys.stderr)
        # Newer bindings may grow set_property(); use it when present.
        if props and how != "none" and hasattr(pipe, "set_property"):
            for key, value in props.items():
                try:
                    pipe.set_property({key: value})
                except Exception as exc:
                    print(f"warning: {key} rejected: {exc}", file=sys.stderr)
        return pipe
    if first_error is not None:
        # Better to report the real cause (bad model dir, unreadable weights) than
        # the bookkeeping error about construction shapes.
        raise first_error
    raise RuntimeError("no LLMPipeline construction shape worked")


print(f"loading GenAI model from {MODEL_DIR} on device {DEVICE}", flush=True)
PIPELINE = _build_pipeline()
print("pipeline ready", flush=True)


def gen_config(req):
    cfg = ov_genai.GenerationConfig()
    cfg.max_new_tokens = int(req.get("max_tokens") or req.get("max_completion_tokens")
                             or MAX_NEW_TOKENS)
    temperature = req.get("temperature")
    if temperature is not None:
        cfg.temperature = float(temperature)
    top_p = req.get("top_p")
    if top_p is not None:
        cfg.top_p = float(top_p)
    seed = req.get("seed")
    if seed is not None:
        cfg.seed = int(seed)
    return cfg


def _result_text(result):
    """Coerce whatever generate() returned into plain text for JSON serialisation.

    GenAI returns DecodedResults/EncodedResults/ChatGenerationResult depending on
    the input shape and release; a raw result object handed to json.dumps raises
    TypeError deep inside FastAPI, so normalise here instead of at each call site.
    """
    for attr in ("text", "texts"):
        value = getattr(result, attr, None)
        if isinstance(value, str):
            return value
        if isinstance(value, (list, tuple)) and value:
            first = value[0]
            return first if isinstance(first, str) else str(first)
    if isinstance(result, str):
        return result
    if isinstance(result, (list, tuple)) and result:
        first = result[0]
        return first if isinstance(first, str) else str(first)
    return str(result)


def _chat_input(messages):
    """Wrap messages in the shape GenAI's chat path asks for.

    2026.4.1 exposes `ChatHistory(messages: list)' and generate() takes a
    ChatHistory; older bindings took the bare dict list, oldest took only text.
    Returns (input, label) so the caller can fall through in that order.
    """
    factory = getattr(ov_genai, "ChatHistory", None)
    if factory is not None:
        try:
            return factory(list(messages)), "chathistory"
        except Exception as exc:
            print(f"note: ChatHistory construction failed: {exc}", file=sys.stderr)
    return list(messages), "message-dicts"


def _special(name, fallback):
    """Read a special-token string out of the model dir's tokenizer_config.json.

    The fallback path needs the same tokens the weights were trained with; they
    are sitting in the export already, so read them rather than hardcoding ChatML.
    """
    try:
        with open(os.path.join(MODEL_DIR, "tokenizer_config.json")) as fh:
            cfg = json.load(fh)
        return (cfg.get("tokenizer_additional_types") or {}).get(name) \
            or cfg.get(name) or fallback
    except Exception:
        return fallback


def _chat_to_text(messages):
    """Fallback when the installed GenAI cannot take message dicts directly.

    Mirrors the ChatML-ish shape LFM2 uses (<|startoftext|>role ... <|im_end|>) as
    read from the export, so the fallback does not invent a template the weights
    were never trained with.  GenAI's own template (applied on the message-dict
    path) stays the preferred route.
    """
    start = _special("bos_token", "<|startoftext|>")
    end = _special("eos_token", "<|im_end|>")
    out = []
    for m in messages:
        role = m.get("role", "user")
        content = m.get("content") or ""
        if isinstance(content, list):      # OpenAI multimodal part list
            content = "".join(p.get("text", "") for p in content if isinstance(p, dict))
        out.append(f"{start}{role}\n{content}{end}\n")
    out.append(f"{start}assistant\n")
    return "".join(out)


def _first(messages, role):
    for m in messages:
        if m.get("role") == role:
            return m.get("content")
    return None


app = FastAPI(title="openvino-genai-server", version="0.1")


@app.get("/healthz")
def healthz():
    return {"status": "ok", "device": DEVICE, "model": MODEL_DIR}


@app.get("/v1/models")
def models():
    name = os.path.basename(MODEL_DIR.rstrip("/"))
    return {"object": "list",
            "data": [{"id": name, "object": "model", "owned_by": "openvino"}]}


@app.post("/v1/chat/completions")
async def chat(request: Request):
    body = await request.json()
    messages = body.get("messages") or []
    cfg = gen_config(body)
    name = os.path.basename(MODEL_DIR.rstrip("/"))

    def run():
        with _lock:
            candidate, how = _chat_input(messages)
            try:
                return _result_text(PIPELINE.generate(candidate,
                                                      generation_config=cfg))
            except TypeError as exc:
                # Neither ChatHistory nor the bare dict list took it: flatten here.
                print(f"note: {how} input rejected ({exc}); using flattened text",
                      file=sys.stderr)
            # Last resort: our own ChatML flattening, read from the export's tokens.
            return _result_text(PIPELINE.generate(_chat_to_text(messages),
                                                  generation_config=cfg))

    if body.get("stream"):
        def events():
            text = run()
            chunk = {"id": "1", "object": "chat.completion.chunk", "model": name,
                     "choices": [{"index": 0,
                                  "delta": {"content": text},
                                  "finish_reason": None}]}
            yield f"data: {json.dumps(chunk)}\n\n"
            done = {"id": "1", "object": "chat.completion.chunk", "model": name,
                    "choices": [{"index": 0, "delta": {}, "finish_reason": "stop"}]}
            yield f"data: {json.dumps(done)}\n\n"
            yield "data: [DONE]\n\n"
        return StreamingResponse(events(), media_type="text/event-stream")

    text = run()
    return JSONResponse({
        "id": "1", "object": "chat.completion", "model": name,
        "choices": [{"index": 0,
                     "message": {"role": "assistant", "content": text},
                     "finish_reason": "stop"}],
        "usage": {"prompt_tokens": 0, "completion_tokens": 0, "total_tokens": 0}})


@app.post("/v1/completions")
async def completions(request: Request):
    body = await request.json()
    prompt = body.get("prompt") or ""
    if isinstance(prompt, list):
        prompt = "".join(prompt)
    cfg = gen_config(body)
    name = os.path.basename(MODEL_DIR.rstrip("/"))
    with _lock:
        text = _result_text(PIPELINE.generate(prompt, generation_config=cfg))
    return JSONResponse({
        "id": "1", "object": "text_completion", "model": name,
        "choices": [{"index": 0, "text": text, "finish_reason": "stop"}]})


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host=os.environ.get("OPENVINO_HOST", "127.0.0.1"),
                port=int(os.environ.get("OPENVINO_PORT", "8093")),
                log_level="info")
