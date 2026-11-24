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

Chat streaming is real: `generate(..., streamer=…)` pushes decoded pieces onto
a queue that the SSE generator drains, so tokens reach the client as they are
produced and the thinking block arrives *before* the answer rather than being
bolted on after it.  Tool-bearing turns are the exception and stay buffered --
a `<|tool_call_start|>` block only parses once complete -- as is any turn where
the incremental path cannot start, which falls back to one well-formed chunk.
Every generation logs one metadata line (tokens, finish_reason, latency,
truncation); set OPENVINO_LOG_REQUESTS=0 to silence it.
"""

import ast
import json
import os
import queue
import re
import sys
import threading
import time

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
# Raised from 256: LFM2.5 reasons before it answers, and a budget that only
# covers the reasoning returns no answer at all.  Measured on this box the
# thinking block alone ran 71-1240 tokens depending on the prompt.
MAX_NEW_TOKENS = int(os.environ.get("OPENVINO_MAX_NEW_TOKENS", "2048"))

# Prepended when a client sends no system message of its own.  Set to "" to
# disable.  See _with_default_system for the measurements behind the wording.
SYSTEM_PROMPT = os.environ.get(
    "OPENVINO_SYSTEM_PROMPT",
    "You are a concise assistant. Keep your internal reasoning brief, "
    "then answer directly.")

# Appended to a system message the CALLER supplied.  SYSTEM_PROMPT above only
# ever applies when a client sends no system message at all, which in practice
# none of them do (pi, SillyTavern and the rules-lawyer bot all send one), so
# before this knob existed the brevity nudge never reached a single production
# request and thinking ran unbounded.  Set to "" to disable.
REASONING_HINT = os.environ.get(
    "OPENVINO_REASONING_HINT",
    "\n\nKeep your internal reasoning brief, then answer directly.")

# Think guard: bound the reasoning block separately from the answer.  The
# template opens every assistant turn inside `<think>`, so a completion only
# reaches the user after `</think>` -- and when the model never closes that
# block before the budget runs out, _split_thinking returns an EMPTY answer
# with finish_reason "length".  Measured on the live comfyui log
# (/var/log/openvino/openvino-lfm2.log, 2026-11-24): 11 of 94 generations were
# TRUNCATED EMPTY-ANSWER(think-block-never-closed), every one content_chars=0.
# Two signatures, both leaving the client with reasoning and no reply:
#   finish=length completion_tokens=8192 reasoning_chars=30368  <- thought to
#                                                                   the cap
#   finish=length completion_tokens=6399 reasoning_chars=25722  <- EOS inside
#                                                                   the block
# Raising the budget is not the fix (8192 is already large; successful turns in
# the same log spent 24201 reasoning chars and still answered) -- the thinking
# has to be bounded apart from the answer.  THINK_BUDGET caps pass-1 reasoning;
# ANSWER_RESERVE is held back for the forced-close recovery pass(es) that
# recapture the answer, so recovery never pushes the turn past the client's
# max_tokens.  Set ANY of the three to 0 (or MAX_THINK_PASSES below 2) to
# disable the guard entirely, in which case the server behaves exactly as it did
# before this block existed.  See _ThinkGuard and _run_think_recovery for the
# bounded recovery loop.
THINK_BUDGET = int(os.environ.get("OPENVINO_THINK_BUDGET", "2048"))
ANSWER_RESERVE = int(os.environ.get("OPENVINO_ANSWER_RESERVE", "1024"))
# The streamer sees decoded TEXT, not token counts, so the think budget is
# converted to characters at this ratio.  The live log gives it directly:
# 8192 tokens -> 30368 chars (3.71), 2540 -> 9589 (3.78), 5728 -> 24201 (4.22),
# 6399 -> 25722 (4.02).  4.0 sits in the middle of those and errs slightly
# generous, so the guard fires a touch later rather than chopping a think block
# that was about to close on its own.
CHARS_PER_TOKEN = float(os.environ.get("OPENVINO_CHARS_PER_TOKEN", "4.0"))
# The model can RE-OPEN a think block after a forced close -- a live probe
# (LFM2.5, openvino_genai 2026.4.1) measured pass 2 of a recovered turn
# beginning with a literal `<think>` -- so one forced close is not always
# enough.  The recovery loop re-prompts up to MAX_THINK_PASSES times IN TOTAL
# (pass 1 included) and stops the moment a pass yields a non-empty answer.  Set
# to 1 (or 0) to allow no recovery pass, which also disables the guard -- there
# is no point bounding pass 1's thinking if nothing recaptures the answer.
MAX_THINK_PASSES = int(os.environ.get("OPENVINO_MAX_THINK_PASSES", "3"))

# Sampling for TOOL-BEARING requests only, and the reason is measured, not
# stylistic.  The export's generation_config.json ships temperature 0.1 (the
# model card's chat recommendation), and at 0.1 this model degenerates on a
# large agent prompt: replaying pi's real 7-tool request (2551 prompt tokens)
# produced an endless "I'm an expert coding agent. I'm an expert coding
# agent. ..." loop that ran to the 8192-token cap with finish_reason=length and
# no tool call at all.  The identical request at temperature 0.6 answered
# correctly with bash({"command": "ls -la"}) in 461 tokens; 1.0 also worked.
# Trimming the tool list to 2 of 7 also fixed it at 0.1 (1369 prompt tokens),
# so the trigger is near-greedy decoding over a big structured prompt.
# The model card's own agentic benchmarks use exactly this range --
# AutomationBench and WebArena at temperature 0.6 / top_p 0.95, LiveClaw at
# 1.0 -- while its chat recommendation stays 0.1.  Plain chat therefore keeps
# the model's 0.1 (SillyTavern and the Q&A suite both measure better there);
# only requests that carry `tools' are warmed up.  A client that sends an
# explicit temperature always wins.
TOOL_TEMPERATURE = float(os.environ.get("OPENVINO_TOOL_TEMPERATURE", "0.6"))
TOOL_TOP_P = float(os.environ.get("OPENVINO_TOOL_TOP_P", "0.95"))
# Measured on pi's real 7-tool request (2551 prompt tokens), temp 0.6, top_k 50,
# three identical runs per setting -- generation is deterministic here, so these
# are reproducible rather than lucky samples:
#   rep 1.05 (the model's own)  671 tokens, usable but verbose, and elsewhere
#                              produced a 1282-token turn with NO tool call
#   rep 1.15                     77 tokens, `bash(pwd && ls -la)`  <- chosen
#   rep 1.20                     39 tokens, `bash(pwd)` -- cheaper but it stopped
#                              short of listing the directory it needed
#   rep 1.30                    783 tokens, `bash(pwd)`
# top_k must stay at the model's 50: setting top_k=0 (disabled) degenerated into
# a 2048-token repetition loop with no tool call.
TOOL_REPETITION_PENALTY = float(os.environ.get("OPENVINO_TOOL_REPETITION_PENALTY",
                                              "1.15"))

# The model's chat template ends every assistant turn with
# `<|im_start|>assistant\n<think>`, so completions always open inside a
# thinking block and the user-facing answer starts after this tag.
THINK_CLOSE = "</think>"

# Appended to a truncated think block to force it shut before a recovery pass:
# exactly the token sequence the template emits to end the block, so the next
# pass picks up where the answer begins.  Kept as ONE named constant so it can
# be retuned in a single place -- a probe is measuring whether a different
# suffix reduces how often the model re-opens a fresh `<think>` in recovery.
# We deliberately do NOT use generation_config.stop_strings for this: the export
# ships stop_strings=set() (measured), and only the streamer abort works
# uniformly on the streaming and buffered paths while letting us count the block
# as it grows.
FORCED_CLOSE_SUFFIX = THINK_CLOSE + "\n"

_lock = threading.Lock()          # one generation at a time; see service comment

# Set to "0" to reduce the log to uvicorn's access lines plus startup.
LOG_REQUESTS = os.environ.get("OPENVINO_LOG_REQUESTS", "1") != "0"


def _log(msg):
    """One line to stdout, which shepherd appends to the log file.

    The access log uvicorn emits says only that a POST returned 200 -- it
    cannot answer "was that reply truncated?", "how much of the budget went on
    thinking?" or "how slow was it?", which are the three questions this
    server actually gets asked.  Every generation therefore logs one line of
    metadata (never prompt or completion text: this endpoint is shared and the
    log is world-readable on the box).

    The same rule covers EXCEPTION text, and since this file is now tailed by
    alloy and shipped to Loki (peteches/systems/comfyui.scm's log-files) it
    covers it fleet-wide rather than just on this box.  Every print that
    reports a failure from a call carrying a client's conversation -- the
    chat-template render, ChatHistory construction, generate() itself -- logs
    the exception's TYPE NAME and not its message, because that message is
    composed by a third-party pybind/Jinja/C++ layer nobody here controls, and
    the template's own errors are known to quote the offending value back ("Tool
    call arguments must be a mapping ...").  Startup, device-property and
    perf-metrics failures keep their full messages: no client input reaches
    those calls, and their text is what makes a bad model directory or an
    unsupported plugin property diagnosable.

    No timestamp of our own: shepherd already prefixes one to every line it
    captures, so adding a second made each entry read
    "2026-11-24 10:50:18 2026-11-24 10:50:18 gen ..." (hit live) and broke
    any grep or Loki pipeline keyed on the leading stamp.
    """
    if LOG_REQUESTS:
        print(f"gen {msg}", flush=True)


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


def gen_config(req, has_tools=False):
    """Per-request generation config, layered on top of the MODEL's own.

    This used to start from a fresh `ov_genai.GenerationConfig()', which
    silently discards the export's generation_config.json.  A blank object
    carries eos_token_id=-1, an empty stop_token_ids, do_sample=False,
    temperature=1.0, top_k unlimited and repetition_penalty=1.0, whereas this
    model ships do_sample=True, temperature=0.1, top_k=50,
    repetition_penalty=1.05 and eos/stop token 124900 — so every request was
    decoded with settings nobody chose.  pipeline.get_generation_config()
    returns the model's values; request fields override them.
    """
    try:
        cfg = PIPELINE.get_generation_config()
    except Exception as exc:
        print(f"note: get_generation_config() failed ({exc}); falling back to "
              f"library defaults", file=sys.stderr)
        cfg = ov_genai.GenerationConfig()
    cfg.max_new_tokens = int(req.get("max_tokens") or req.get("max_completion_tokens")
                             or MAX_NEW_TOKENS)
    temperature = req.get("temperature")
    if temperature is None and has_tools:
        # See TOOL_TEMPERATURE: the model's own 0.1 degenerates into a
        # repetition loop on large agent prompts.
        temperature = TOOL_TEMPERATURE
    if temperature is not None:
        cfg.temperature = float(temperature)
        # Turning temperature up implies sampling; the model's own config sets
        # do_sample, but a client asking for temperature 0 wants greedy.
        cfg.do_sample = float(temperature) > 0
    top_p = req.get("top_p")
    if top_p is None and has_tools:
        top_p = TOOL_TOP_P
    if top_p is not None:
        cfg.top_p = float(top_p)
    seed = req.get("seed")
    if seed is not None:
        cfg.seed = int(seed)
    # Anti-repetition knobs, passthrough so they can be tuned per request.
    # This model degenerates into loops ("We need to read files. We need to
    # read files. ...") on large agent prompts even at temperature 0.6, and the
    # only levers that reach it are the penalty terms and top_k -- so they have
    # to be settable from outside rather than welded to generation_config.json.
    for field, key in (("repetition_penalty", "repetition_penalty"),
                       ("frequency_penalty", "frequency_penalty"),
                       ("presence_penalty", "presence_penalty"),
                       ("top_k", "top_k")):
        value = req.get(key)
        if value is None and has_tools and field == "repetition_penalty":
            value = TOOL_REPETITION_PENALTY
        if value is not None and hasattr(cfg, field):
            try:
                setattr(cfg, field, type(getattr(cfg, field))(value))
            except Exception as exc:
                # Type name only, and the client-supplied VALUE is dropped with
                # it: `key' is from the fixed allowlist above, `value' is
                # whatever the caller sent.  See _log's docstring.
                print(f"note: ignoring {key} ({type(exc).__name__})",
                      file=sys.stderr)
    return cfg


def _with_default_system(messages):
    """Guarantee a system message, and that it asks for brief reasoning.

    The template always opens the assistant turn with `<think>`, so the model
    reasons first and the answer only follows the closing tag.  Asking it to
    keep that reasoning brief cuts the thinking budget several-fold with no
    measurable loss of answer quality — measured on this box, "in one sentence,
    explain what a reverse proxy does" went from 1653 generated tokens (8.3 s,
    and it overran a 1024 budget without ever answering) to 287 tokens (1.5 s)
    for the same sentence, and the is_palindrome prompt from 1240 tokens (6.3 s)
    to 381 (2.0 s) with cleaner output.

    That measurement used to be unreachable in production.  This function
    returned early whenever the caller supplied ANY system message, and every
    real client does, so the nudge applied to nothing but hand-rolled curl
    probes while pi's turns kept thinking for as long as they liked (one live
    request logged 3563 characters of reasoning ahead of an 851-character
    answer).  Since max_new_tokens has to cover thinking AND reply, that
    thinking is exactly what was eating the answer budget.

    So: caller has no system message -> prepend SYSTEM_PROMPT, as before.
    Caller has one -> append REASONING_HINT to it, leaving their own
    instructions intact and ahead of ours.  Idempotent, so a multi-turn history
    that replays an already-hinted system message does not stack the hint.
    """
    msgs = list(messages)
    for i, m in enumerate(msgs):
        if not (isinstance(m, dict) and m.get("role") == "system"):
            continue
        if not REASONING_HINT:
            return msgs
        content = m.get("content")
        needle = REASONING_HINT.strip()
        if isinstance(content, str):
            if needle and needle in content:
                return msgs                      # already hinted
            patched = dict(m)
            patched["content"] = content + REASONING_HINT
            msgs[i] = patched
            return msgs
        if isinstance(content, (list, tuple)):
            # OpenAI multimodal part list.  _parse_content concatenates the
            # `text' of each dict part, so appending one more text part lands
            # the hint at the end of the rendered system message.
            flat = "".join(p.get("text", "") if isinstance(p, dict) else str(p)
                           for p in content)
            if needle and needle in flat:
                return msgs
            patched = dict(m)
            patched["content"] = (list(content)
                                  + [{"type": "text", "text": REASONING_HINT}])
            msgs[i] = patched
        # Anything else (a bare dict, a number, None) is left exactly as it
        # came: better to lose the hint than corrupt a system message we
        # cannot read.
        return msgs
    if SYSTEM_PROMPT:
        return [{"role": "system", "content": SYSTEM_PROMPT}] + msgs
    return msgs


def _split_thinking(text):
    """Split `<think>...</tool_response>` into (reasoning, answer, closed).

    When the model never closed the block — the token budget ran out
    mid-thought — the partial reasoning goes in `reasoning' and `answer' comes
    back empty, rather than passing the chain of thought off as the reply.
    Leaking it as content is exactly what made this endpoint's answers unusable
    before; `closed' is False so the caller reports finish_reason "length"
    instead of pretending the response completed.
    """
    if THINK_CLOSE in text:
        think, answer = text.split(THINK_CLOSE, 1)
        return think.strip(), answer.strip(), True
    return text.strip(), "", False


def _finish_and_usage(res, cfg, closed, tool_calls=()):
    """Real finish_reason and usage, from the result's own PerfMetrics.

    Both used to be hardcoded ("stop" and all-zero usage), so a client could
    not tell a completed answer from one cut off mid-sentence, and pi's
    context accounting had nothing to work with.
    """
    n_in = n_out = 0
    pm = getattr(res, "perf_metrics", None)
    if pm is not None:
        try:
            n_in = int(pm.get_num_input_tokens())
            n_out = int(pm.get_num_generated_tokens())
        except Exception as exc:
            print(f"note: perf_metrics unavailable ({exc})", file=sys.stderr)
    limit = getattr(cfg, "max_new_tokens", 0) or 0
    # Prefer the runtime's own verdict over inferring one from token counts:
    # DecodedResults carries finish_reasons, and GenerationFinishReason.LENGTH
    # is unambiguous where "generated >= limit" could also mean it stopped exactly
    # at the cap by coincidence.
    reasons = getattr(res, "finish_reasons", None)
    hit_limit = None
    if reasons:
        try:
            hit_limit = "LENGTH" in str(reasons[0]).upper()
        except Exception:
            hit_limit = None
    if hit_limit is None:
        hit_limit = bool(limit) and n_out >= limit
    # A turn that ends in a tool call is complete even though it carries no
    # prose, so it must not be reported as truncated.
    if tool_calls:
        finish = "tool_calls"
    elif closed and not hit_limit:
        finish = "stop"
    else:
        finish = "length"
    return finish, {"prompt_tokens": n_in, "completion_tokens": n_out,
                    "total_tokens": n_in + n_out}


def _think_char_budget(requested_tokens):
    """Char budget for ONE pass's reasoning, or None to disable the guard.

    Applies to pass 1 and to every recovery pass alike, with `requested_tokens'
    being that pass's own budget: the client's max_tokens for pass 1, whatever
    is left of it afterwards for the rest.  The effective cap is
    min(THINK_BUDGET, requested - ANSWER_RESERVE) tokens, converted to chars at
    CHARS_PER_TOKEN, so ANSWER_RESERVE is room for the answer WITHIN the same
    pass -- a pass that closes its block still has to produce the reply, and a
    pass whose thinking is allowed to reach its own ceiling cannot.  If a pass's
    budget is at or below ANSWER_RESERVE there is nothing left to guard, so the
    guard switches off and that pass spends everything trying to close and
    answer, which is the right last resort.  Any knob set to 0 disables it too.

    The subtraction is what a halving rule got wrong in production.  An earlier
    revision capped recovery passes at HALF the tokens left, "so each leaves the
    next some room"; measured on comfyui at 15:40 and 15:41 on 2026-11-24 that
    starved the ladder instead -- pass 1 had already taken 6664 of pi's 8192, so
    the two recovery passes got 764 tokens each, less than the 1300-2000 a
    healthy think block on this box closes in, and both were cut before they
    could answer (passes=3, pass2_tokens=1528, content_chars=0).  Reserving a
    fixed ANSWER_RESERVE scales with the pass instead of halving it away.
    """
    if (THINK_BUDGET <= 0 or ANSWER_RESERVE <= 0 or CHARS_PER_TOKEN <= 0
            or MAX_THINK_PASSES < 2):
        return None
    if not requested_tokens or requested_tokens <= ANSWER_RESERVE:
        return None
    cap_tokens = min(THINK_BUDGET, requested_tokens - ANSWER_RESERVE)
    if cap_tokens <= 0:
        return None
    return int(cap_tokens * CHARS_PER_TOKEN)


def _input_tokens(res):
    """perf_metrics.get_num_input_tokens(), or 0 when unavailable."""
    pm = getattr(res, "perf_metrics", None)
    if pm is None:
        return 0
    try:
        return int(pm.get_num_input_tokens())
    except Exception:
        return 0


def _generated_tokens(res, text=""):
    """perf_metrics.get_num_generated_tokens(), with a char-derived floor.

    The loop's whole "never exceed the client's max_tokens" guarantee rests on
    this count shrinking `remaining' after every pass, and 0 is the one value
    that breaks it: `remaining' would never move, so each further pass would be
    issued at the FULL requested budget.  perf_metrics did survive a streamer
    abort when measured against this build (openvino_genai 2026.4.1: an aborted
    pass reported get_num_generated_tokens()=65, and a three-pass turn summed
    107+215 correctly), but the binding promises nothing about the aborted case,
    so a 0 is replaced by the text length converted at CHARS_PER_TOKEN.  The
    error is deliberately one-sided: over-estimating ends recovery early and
    costs an answer, while under-estimating spends budget the client did not
    grant.
    """
    pm = getattr(res, "perf_metrics", None)
    n = 0
    if pm is not None:
        try:
            n = int(pm.get_num_generated_tokens())
        except Exception:
            n = 0
    if n <= 0 and text and CHARS_PER_TOKEN > 0:
        n = max(1, int(len(text) / CHARS_PER_TOKEN))
    return max(0, n)


class _PerfShim:
    """perf_metrics stand-in carrying token counts we computed ourselves."""

    def __init__(self, n_in, n_out):
        self._n_in = n_in
        self._n_out = n_out

    def get_num_input_tokens(self):
        return self._n_in

    def get_num_generated_tokens(self):
        return self._n_out


class _ThinkGuard:
    """Bounds one thinking block, in characters, as it streams in.

    Fed every decoded piece; returns True once the accumulated reasoning
    overruns the budget without having closed the block.  The OpenVINO streamer
    treats a truthy return as StreamingStatus::STOP (text_streamer.cpp:106),
    and the stop is clean: handle->stop() defaults to GenerationFinishReason::
    STOP (generation_handle.hpp:131), so the tokens produced so far are kept and
    the caller can force the block shut and re-prompt for the answer.  This is
    the only behaviour change for turns that close their think block normally --
    for those feed() never returns True and there is no recovery pass.

    .raw         accumulated pass text -- used only to count chars and spot
                 the close.  The recovery prompt is built from _result_text(res),
                 NOT from .raw: a live probe measured the returned text a couple
                 of chars shorter than the accumulator (327 vs 329), and the
                 prompt must match what the model actually produced.
    .closed      THINK_CLOSE appeared -> the block ended on its own
    .overflowed  the char budget fired -> we aborted it, as distinct from the
                 model emitting EOS mid-thought, which leaves .overflowed False
                 and .closed False (both still trigger the recovery loop)
    .limit_chars None when the guard is disabled for this pass
    """

    def __init__(self, requested_tokens):
        # The cap is always derived from THIS pass's own budget, pass 1 and
        # recovery passes alike -- see _think_char_budget.
        self.limit_chars = _think_char_budget(requested_tokens)
        self.raw = ""
        self.closed = False
        self.overflowed = False

    def feed(self, piece):
        """Accumulate a decoded piece; -> True to stop generation."""
        if self.closed:
            return False
        self.raw += piece
        if THINK_CLOSE in self.raw:
            self.closed = True
            return False
        if self.limit_chars is not None and len(self.raw) >= self.limit_chars:
            self.overflowed = True
            return True
        return False


class _Recovery:
    """Outcome of the bounded think-recovery loop.

    Reasoning and answer are tracked SEPARATELY: a single _split_thinking over
    the concatenated passes would split at the FIRST `</think>` and leak a later
    re-opened `<think>…</think>` into the answer (measured: a recovery pass
    began with a literal `<think>`).  Exposes perf_metrics and finish_reasons
    shaped like a DecodedResults so _finish_and_usage works unchanged --
    prompt_tokens is pass 1's (the client's real prompt; the re-prompts are our
    internal trick), completion_tokens is the sum across passes, finish_reasons
    is the LAST pass's.  The caller passes closed=bool(answer), because a
    guard-aborted pass reads STOP even though it produced no answer: the guard's
    own state, not finish_reasons, is the authority on whether we recovered.
    """

    def __init__(self, reasoning, answer, res1, last_res, total_generated):
        self.reasoning = reasoning
        self.answer = answer
        self.last_res = last_res
        self.total_generated = total_generated
        self.finish_reasons = getattr(last_res, "finish_reasons", None)
        self.perf_metrics = _PerfShim(_input_tokens(res1), total_generated)


def _run_think_recovery(prompt, res1, requested, body, has_tools, info):
    """Bounded buffered loop that recovers an answer from an unclosed think block.

    Called once pass 1 has ended inside `<think>` -- the guard aborted it at the
    char budget (info['overflowed']) or the model emitted EOS mid-thought.  Each
    pass appends the previous raw text plus FORCED_CLOSE_SUFFIX and regenerates
    from the result TEXT, not the streamer's accumulator (a live probe measured
    the two differing by a couple of characters, 329 streamed vs 327 returned,
    and the prompt must match what the model actually produced).  A pass that
    closes the block yields the answer and ends the loop; a pass that re-opens
    `<think>` contributes more reasoning and, budget and MAX_THINK_PASSES
    permitting, another forced close follows.  Every pass is guarded by the same
    rule as pass 1 -- _think_char_budget(remaining), which reserves
    ANSWER_RESERVE tokens for the answer WITHIN that pass -- and switches itself
    off once the remainder is too small to reserve from, so the last pass spends
    everything trying to close and reply.

    Returns a _Recovery.  When the loop gives up with no answer, answer is ""
    and the caller reports finish_reason "length" plus the EMPTY-ANSWER flag;
    reasoning is NEVER leaked as the answer (see _split_thinking's docstring --
    that was already tried and made the endpoint unusable).  Must be called with
    _lock held; the passes then share the one GPU trial slot, exactly as a single
    generation does under _stream_worker / run().
    """
    reasoning_parts = []
    answer = ""
    last_res = res1
    passes = 1
    overflowed = bool(info.get("overflowed"))

    prev_prompt = prompt
    prev_text = _result_text(res1)
    pass1_generated = _generated_tokens(res1, prev_text)
    total_generated = pass1_generated
    prev_closed = THINK_CLOSE in prev_text
    if prev_closed:
        head, tail = prev_text.split(THINK_CLOSE, 1)
        reasoning_parts.append(head)
        answer = tail.strip()
    else:
        reasoning_parts.append(prev_text)

    while answer == "" and passes < MAX_THINK_PASSES:
        remaining = requested - total_generated
        if remaining <= 0:
            break
        passes += 1
        # previous prompt + previous raw text + a forced close (only when the
        # previous pass left the block open) + newline.  FORCED_CLOSE_SUFFIX is
        # the single place that sequence lives.
        next_prompt = (prev_prompt + prev_text
                       + ("\n" if prev_closed else FORCED_CLOSE_SUFFIX))
        cfg_i = gen_config(body, has_tools)
        cfg_i.max_new_tokens = remaining
        # Same rule as pass 1, on this pass's own budget: reserve
        # ANSWER_RESERVE of it for the answer, and switch the guard off when
        # there is not that much left.  See _think_char_budget for the
        # production measurement behind using a fixed reserve here rather than
        # a fraction of what is left.
        guard = _ThinkGuard(remaining)
        try:
            res_i = PIPELINE.generate([next_prompt], generation_config=cfg_i,
                                      streamer=guard.feed)
        except Exception as exc:
            # The exception TYPE only, not its text.  This is the one new place
            # that reports a failure from a call carrying the client's whole
            # conversation, and `exc' is an unbounded string composed by a
            # third-party pybind/C++ layer; shepherd appends stderr to the same
            # log the counts-only _log lines are careful about.  Realistic
            # messages are config/device errors that quote no input, but the
            # type name carries the same diagnostic value at no disclosure risk.
            print(f"note: think-recovery pass {passes} failed "
                  f"({type(exc).__name__}); stopping recovery", file=sys.stderr)
            # Flagged in the _log line too: a stderr note is easy to miss, and
            # without a marker a systematic failure here (KV cache refusing the
            # longer re-prompt, say) reads as "the guard did not help".
            info["error"] = True
            break
        prev_prompt = next_prompt
        prev_text = _result_text(res_i)
        spent = _generated_tokens(res_i, prev_text)
        if spent <= 0:
            # Cannot account for what that pass cost, so there is no honest
            # `remaining' to hand the next one.  Stop instead of risking a pass
            # issued at the client's full budget.
            info["error"] = True
            break
        total_generated += spent
        last_res = res_i
        if guard.overflowed:
            overflowed = True
        prev_closed = THINK_CLOSE in prev_text
        if prev_closed:
            head, tail = prev_text.split(THINK_CLOSE, 1)
            reasoning_parts.append(head)
            answer = tail.strip()
        else:
            reasoning_parts.append(prev_text)

    info["forced"] = passes > 1
    info["overflowed"] = overflowed
    info["passes"] = passes
    info["pass2_tokens"] = total_generated - pass1_generated
    reasoning = "".join(part for part in reasoning_parts if part)
    return _Recovery(reasoning, answer, res1, last_res, total_generated)


class _ThinkSplitter:
    """Incremental `<think>…</think>` splitter for the streaming path.

    `_split_thinking` needs the whole text; a stream does not have it.  Feed
    decoded pieces in and it returns (reasoning_delta, content_delta) pairs,
    holding back only the handful of trailing characters that could still turn
    out to be the start of the closing tag.

    Returning reasoning strictly before content is the whole point: the model
    generates the thinking block first, and a response assembled after the
    fact cannot preserve that order because both halves land in one chunk --
    which is exactly why clients used to render the answer and then dump the
    chain of thought underneath it.
    """

    def __init__(self):
        self._pending = ""
        self._closed = False
        self._holdback = len(THINK_CLOSE) - 1
        self._content_started = False

    def _content(self, text):
        """Drop the whitespace the template puts between `</think>` and the answer.

        Without this every streamed reply opened with a stray newline, because
        the model emits `</think>\n' and the split hands the `\n' to the
        content side.
        """
        if self._content_started:
            return text
        stripped = text.lstrip()
        if not stripped:
            return ""
        self._content_started = True
        return stripped

    def feed(self, piece):
        """-> (reasoning_delta, content_delta); either may be empty."""
        self._pending += piece
        if self._closed:
            out, self._pending = self._pending, ""
            return "", self._content(out)
        idx = self._pending.find(THINK_CLOSE)
        if idx >= 0:
            self._closed = True
            reasoning, self._pending = (self._pending[:idx],
                                        self._pending[idx + len(THINK_CLOSE):])
            return reasoning, ""
        # No closing tag yet, so everything is still reasoning -- except a tail
        # short enough to be a partial `</think>`, which has to wait.
        safe = len(self._pending) - self._holdback
        if safe <= 0:
            return "", ""
        out, self._pending = self._pending[:safe], self._pending[safe:]
        return out, ""

    def flush(self):
        """End of stream -> (reasoning, content, closed), draining the buffer.

        `closed' False means the token budget ran out mid-thought, so what is
        left is reasoning and there is no answer to show; the caller turns that
        into finish_reason "length" rather than passing the chain of thought
        off as the reply.
        """
        rest, self._pending = self._pending, ""
        if self._closed:
            return "", self._content(rest), True
        return rest, "", False


_STREAM_DONE = object()           # sentinel: the worker's generate() returned


def _stream_worker(prompt, cfg, out_q, holder, guard=None):
    """Run one generation on a thread, pushing decoded pieces onto out_q.

    `generate(..., streamer=...)` is the incremental API this build exposes --
    verified against the running openvino_genai, whose signature is
    `generate(inputs, generation_config=None, streamer: Callable[[str],
    int | None] | StreamerBase | None = None, **kwargs)`.  The callback fires
    per decoded piece; returning None means "keep generating".  Without it the
    call blocks until the entire answer exists, so an SSE response can only
    ever arrive in one burst at the end -- which is what this server did
    before, while claiming to stream.

    `_lock` is taken here rather than in the caller so it is held for the whole
    generation, streaming included: the queue decouples the producer from the
    HTTP write, but only one trial may use the GPU at a time.

    When `guard' is supplied every piece is also fed to it and its verdict
    returned to the runtime, so a runaway think block is stopped at the char
    budget instead of running to the token cap (see _ThinkGuard).  The piece is
    queued either way -- the guard only decides whether generation continues.
    """
    def streamer(piece):
        out_q.put(piece)
        # True stops generation (StreamingStatus::STOP), False/None keeps it
        # going; pybind coerces the returned bool into the callback variant.
        return guard.feed(piece) if guard is not None else None

    try:
        with _lock:
            holder["res"] = PIPELINE.generate([prompt], generation_config=cfg,
                                              streamer=streamer)
    except Exception as exc:            # surfaced to the client below
        holder["error"] = exc
    finally:
        out_q.put(_STREAM_DONE)


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


def _chat_input(messages, tools=None):
    """Wrap messages in the shape GenAI's chat path asks for.

    2026.4.1 exposes `ChatHistory(messages: list)' and generate() takes a
    ChatHistory; older bindings took the bare dict list, oldest took only text.
    Returns (input, label) so the caller can fall through in that order.
    """
    factory = getattr(ov_genai, "ChatHistory", None)
    if factory is not None:
        try:
            history = factory(list(messages))
            # Handing the tool schemas over is what makes the template emit
            # "List of tools: [...]" into the system prompt, and without that
            # the model never produces a tool call at all.
            if tools and hasattr(history, "set_tools"):
                history.set_tools(list(tools))
            return history, "chathistory"
        except Exception as exc:
            # Type name only -- this call takes the client's messages, and the
            # chat template's own errors quote values back.  See _log's docstring.
            print(f"note: ChatHistory construction failed "
                  f"({type(exc).__name__})", file=sys.stderr)
    return list(messages), "message-dicts"


# The template renders assistant tool calls as
#   <|tool_call_start|>[name(arg="value", ...)]<|tool_call_end|>
# i.e. a Python-expression call list (render_tool_calls in the export's
# chat_template.jinja).  GenAI ships Llama3PythonicToolParser for exactly this,
# but measured against 2026.4.1 it silently returns only the LAST call when a
# turn emits several -- asked for the weather in Paris and Lyon, it parsed back
# Lyon alone -- so the markers are parsed here with `ast' instead.
TOOL_CALL_RE = re.compile(r"<\|tool_call_start\|>(.*?)<\|tool_call_end\|>", re.S)


def _literal(node):
    """Best-effort AST node -> Python value, falling back to source text."""
    try:
        return ast.literal_eval(node)
    except Exception:
        try:
            return ast.unparse(node)
        except Exception:
            return None


def _parse_pythonic_calls(body):
    """Parse `[fn(a=1), fn2(b="x")]' into [{name, arguments}, ...]."""
    calls = []
    try:
        tree = ast.parse(body.strip(), mode="eval")
    except SyntaxError:
        return calls
    node = tree.body
    elts = node.elts if isinstance(node, (ast.List, ast.Tuple)) else [node]
    for elt in elts:
        if not isinstance(elt, ast.Call) or not isinstance(elt.func, ast.Name):
            continue
        args = {}
        # The template only ever emits keyword arguments, but a model is free to
        # improvise, so positionals are kept rather than dropped.
        for i, positional in enumerate(elt.args):
            args[f"arg{i}"] = _literal(positional)
        for kw in elt.keywords:
            if kw.arg is not None:
                args[kw.arg] = _literal(kw.value)
        calls.append({"name": elt.func.id, "arguments": args})
    return calls


def _extract_tool_calls(text):
    """Split tool calls out of a completion.

    Returns (text_without_markers, openai_shaped_tool_calls).  Prose and tool
    calls can coexist in one turn, so the markers are excised rather than the
    whole text discarded.
    """
    bodies = TOOL_CALL_RE.findall(text)
    if not bodies:
        return text, []
    parsed = []
    for body in bodies:
        parsed.extend(_parse_pythonic_calls(body))
    cleaned = TOOL_CALL_RE.sub("", text).strip()
    calls = [{"id": f"call_{i}", "type": "function",
              "function": {"name": c["name"],
                           "arguments": json.dumps(c["arguments"])}}
             for i, c in enumerate(parsed)]
    return cleaned, calls


def _normalise_messages(messages):
    """Incoming OpenAI messages -> what GenAI's chat template will accept.

    The template raises outright when an assistant tool_call's `arguments' is a
    JSON-encoded string ("Tool call arguments must be a mapping ... parse
    arguments with json.loads() before applying the chat template"), and every
    OpenAI client sends them as strings.  So decode them back to a mapping on
    the way in, or the second turn of any agent loop dies.
    """
    out = []
    for m in messages:
        if not isinstance(m, dict):
            continue
        m = dict(m)
        calls = m.get("tool_calls")
        if isinstance(calls, list):
            fixed = []
            for tc in calls:
                if not isinstance(tc, dict):
                    continue
                tc = dict(tc)
                fn = tc.get("function")
                if isinstance(fn, dict):
                    fn = dict(fn)
                    args = fn.get("arguments")
                    if isinstance(args, str):
                        try:
                            fn["arguments"] = json.loads(args) if args.strip() else {}
                        except Exception:
                            fn["arguments"] = {"_raw": args}
                    tc["function"] = fn
                fixed.append(tc)
            m["tool_calls"] = fixed
        out.append(m)
    return out


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


CFM_TAG = "CONTINUE_FINAL_MESSAGE_TAG "
THINK_OPEN = "<think>"
TOOL_CALL_START = "<|tool_call_start|>"
TOOL_CALL_END = "<|tool_call_end|>"


def _parse_content(content):
    """Mirror the template's `parse_content' macro."""
    if content is None:
        return ""
    if isinstance(content, str):
        return content
    if isinstance(content, dict):
        return json.dumps(content)
    if isinstance(content, (list, tuple)):
        parts = []
        for item in content:            # OpenAI multimodal part list
            if isinstance(item, str):
                parts.append(item)
            elif isinstance(item, dict):
                if item.get("type") == "image":
                    parts.append("<image>")
                else:
                    parts.append(str(item.get("text") or ""))
        return "".join(parts)
    return str(content)


def _format_arg_value(value):
    """Mirror the template's `format_arg_value' macro.

    Strings become Python single-quoted literals (not JSON), mappings and
    sequences become JSON, everything else is str()'d -- so True renders as
    "True", matching Jinja's `| string' filter rather than json's "true".
    """
    if isinstance(value, str):
        escaped = (value.replace("\\", "\\\\").replace("'", "\\'")
                        .replace("\n", "\\n").replace("\r", "\\r"))
        return "'" + escaped + "'"
    if isinstance(value, (dict, list, tuple)):
        return json.dumps(value)
    return str(value)


def _render_tool_calls(tool_calls):
    """Mirror `render_tool_calls': `[fn(a='b'), g(c=1)]' between the markers."""
    rendered = []
    for tc in tool_calls:
        if not isinstance(tc, dict):
            continue
        func = tc.get("function") if isinstance(tc.get("function"), dict) else tc
        name = func.get("name")
        if not name:
            continue
        args = func.get("arguments")
        if isinstance(args, str):
            # The template raises on a JSON-encoded string here, so decode it.
            try:
                args = json.loads(args) if args.strip() else {}
            except Exception:
                args = {"_raw": args}
        if not isinstance(args, dict):
            args = {}
        pairs = [f"{k}={_format_arg_value(v)}" for k, v in args.items()]
        rendered.append(f"{name}(" + ", ".join(pairs) + ")")
    return TOOL_CALL_START + "[" + ", ".join(rendered) + "]" + TOOL_CALL_END


def _flatten_tools(tools):
    """OpenAI's nested {"type":"function","function":{...}} -> the flat
    {"name","description","parameters"} form the model card documents and the
    template json-dumps into the system prompt verbatim.
    """
    out = []
    for t in tools or []:
        if not isinstance(t, dict):
            continue
        fn = t.get("function")
        out.append(fn if isinstance(fn, dict) else t)
    return out


def _render_chatml(messages, tools=None):
    """Render the conversation exactly as the export's chat_template.jinja does.

    This is the PRIMARY path, not a fallback.  GenAI's ChatHistory cannot carry
    a tool conversation: measured on 2026.4.1 it JSON-dumps an assistant
    message's tool_calls straight into the content, and rewrites `role: "tool"'
    into a `user' message carrying a {"tool_response": ...} blob.  The model
    therefore never sees its own prior calls, nor their results, in the format
    it was trained on.  In pi that showed up as the agent re-running the same
    `ls -la' eight times while every result correctly listed the file it had
    been asked to read.

    Rendering the template here keeps the documented wire format --
    `<|tool_call_start|>[fn(arg='v')]<|tool_call_end|>' for calls and
    `<|fim_prefix|>tool' for results -- and follows the export's own
    chat_template.jinja clause for clause, including its asymmetry (assistant
    turns end `</tool_response>\n', every other role ends `</tool_response>'
    with no newline), the single leading system block with the tool list
    appended to it, and the preserve_thinking=False default that drops
    reasoning from all but the current turn.
    """
    bos = _special("bos_token", "<|startoftext|>")
    msgs = [m for m in messages if isinstance(m, dict)]

    system_prompt = ""
    if msgs and msgs[0].get("role") == "system":
        system_prompt = _parse_content(msgs[0].get("content"))
        msgs = msgs[1:]

    flat = _flatten_tools(tools)
    if flat:
        listing = ", ".join(json.dumps(t) for t in flat)
        system_prompt += (("\n" if system_prompt else "")
                          + "List of tools: [" + listing + "]")

    out = [bos]
    if system_prompt:
        out.append(f"<|fim_prefix|>system\n{system_prompt}</tool_response>\n")

    last_user = -1
    for i, m in enumerate(msgs):
        if m.get("role") == "user":
            last_user = i

    for i, m in enumerate(msgs):
        role = m.get("role") or "user"
        out.append(f"<|fim_prefix|>{role}\n")
        if role == "assistant":
            keep_thinking = i > last_user
            thinking = (m.get("thinking") or m.get("reasoning")
                        or m.get("reasoning_content"))
            if not isinstance(thinking, str):
                thinking = ""
            if thinking and keep_thinking:
                out.append(f"<think>{thinking}</think>")
            content = _parse_content(m.get("content")) if m.get("content") else ""
            has_cfm = content.endswith(CFM_TAG)
            if has_cfm:
                content = content[:len(content) - len(CFM_TAG)]
            if not keep_thinking and THINK_CLOSE in content:
                content = content.split(THINK_CLOSE, 1)[-1].strip()
            out.append(content)
            calls = m.get("tool_calls")
            if isinstance(calls, list) and calls:
                out.append(_render_tool_calls(calls))
            if has_cfm:
                out.append(CFM_TAG)
            out.append("</tool_response>\n")
        else:
            if m.get("content"):
                out.append(_parse_content(m.get("content")))
            out.append("</tool_response>")

    out.append(f"<|file_sep|>assistant\n{THINK_OPEN}")
    return "".join(out)


def _chat_to_text(messages):
    """Last-resort flattening, reached only if _render_chatml AND ChatHistory
    both raise.

    Approximate on purpose: it uses the bos token as the per-message opener and
    carries no tool calls, so it is strictly worse than _render_chatml.  It
    exists so a request degrades to plain text instead of 500-ing.
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
    raw_tools = body.get("tools")
    tools = ([t for t in raw_tools if isinstance(t, dict)]
             if isinstance(raw_tools, list) else None) or None
    messages = _normalise_messages(_with_default_system(body.get("messages") or []))
    cfg = gen_config(body, bool(tools))
    name = os.path.basename(MODEL_DIR.rstrip("/"))

    # Per-request think-guard state, read back by every _log line below.  Set by
    # _run_think_recovery (via _guarded_generate on the buffered path, directly
    # on the streaming path).  Never carries prompt or completion text.
    guard_info = {"forced": False, "overflowed": False, "passes": 1,
                  "pass2_tokens": 0, "error": False}

    def _guard_log_suffix():
        """`think_overflow=1 passes=N pass2_tokens=M THINK-OVERFLOW(forced-close)'
        when the guard had to force the block shut and re-prompt, else ""."""
        if not guard_info["forced"]:
            return ""
        kind = "think_overflow=1" if guard_info["overflowed"] else "think_unclosed=1"
        flag = ("THINK-OVERFLOW(forced-close)" if guard_info["overflowed"]
                else "THINK-UNCLOSED(forced-close)")
        # Recovery dying on an exception, or on a pass whose token count could
        # not be read, is its own marker: it means the ladder stopped early for
        # a reason that has nothing to do with the model's reasoning length.
        error = " THINK-RECOVERY-ERROR" if guard_info["error"] else ""
        return (f" {kind} passes={guard_info['passes']}"
                f" pass2_tokens={guard_info['pass2_tokens']}"
                f" {flag}{error}")

    def _guarded_generate(prompt):
        """Pass 1 (+ bounded recovery) for the rendered-prompt path.

        Returns pass 1's DecodedResults when the think block closed on its own
        or the guard is off -- the common case, with no second pass and no extra
        cost -- or a _Recovery when pass 1 ended inside `<think>` and had to be
        forced shut and re-prompted.  Must be called with _lock held, so the
        recovery passes share pass 1's one GPU trial slot.
        """
        guard = _ThinkGuard(cfg.max_new_tokens)
        # A counting streamer: the buffered path has no queue to push to, so
        # pieces are only accumulated for the guard's char budget.  The
        # ONE-ELEMENT LIST stays -- generate([prompt], cfg, streamer=...) still
        # returns DecodedResults with perf_metrics and finish_reasons, both
        # valid after a streamer abort (a live probe confirmed the abort reads
        # STOP, not LENGTH, so the guard's own state decides, not finish_reasons).
        res1 = PIPELINE.generate([prompt], generation_config=cfg,
                                 streamer=guard.feed)
        # The result TEXT is authoritative (the streamer's accumulator runs a
        # couple of chars long).  Guard off, or the block closed: single pass.
        if guard.limit_chars is None or THINK_CLOSE in _result_text(res1):
            return res1
        guard_info["overflowed"] = guard.overflowed
        return _run_think_recovery(prompt, res1, cfg.max_new_tokens, body,
                                   bool(tools), guard_info)

    def run():
        with _lock:
            # Render the chat template ourselves rather than handing message
            # dicts to ChatHistory: ChatHistory JSON-dumps assistant tool_calls
            # into the content and rewrites the `tool' role to `user', which
            # breaks every agent loop after its first turn.  See _render_chatml.
            try:
                # Pass a ONE-ELEMENT LIST, not a bare string.  Both go down the
                # same tokenization path (which, unlike Tokenizer.encode, does
                # recognise the ChatML special tokens), but generate(str)
                # returns a bare `str' with no perf_metrics or finish_reasons,
                # while generate([str]) returns DecodedResults carrying both --
                # which is where usage and an honest finish_reason come from.
                prompt = _render_chatml(messages, tools)
                return _guarded_generate(prompt)
            except Exception as exc:
                print(f"note: rendered-prompt generation failed "
                      f"({type(exc).__name__}); falling back to ChatHistory",
                      file=sys.stderr)
            # The recovery loop needs a *string* prompt to append the forced
            # close to, so every fallback below skips the guard and runs one
            # generation exactly as before.
            candidate, how = _chat_input(messages, tools)
            try:
                return PIPELINE.generate(candidate, generation_config=cfg)
            except TypeError as exc:
                # Neither ChatHistory nor the bare dict list took it: flatten here.
                # Type name only -- `how' is one of two fixed labels, but this
                # call carries the client's messages.  See _log's docstring.
                print(f"note: {how} input rejected ({type(exc).__name__}); "
                      f"using flattened text", file=sys.stderr)
            # Last resort: the approximate flattening in _chat_to_text.
            return PIPELINE.generate(_chat_to_text(messages),
                                     generation_config=cfg)

    def shape(res):
        """Result object -> (content, reasoning, tool_calls, finish, usage)."""
        if isinstance(res, _Recovery):
            # The recovery loop already tracked reasoning and answer separately;
            # re-splitting their concatenation would leak a re-opened `<think>`
            # into the answer.  Tool markers, if any, ride in the answer.
            content, tool_calls = _extract_tool_calls(res.answer)
            finish, usage = _finish_and_usage(res, cfg, bool(res.answer),
                                              tool_calls)
            return content, res.reasoning, tool_calls, finish, usage
        no_markers, tool_calls = _extract_tool_calls(_result_text(res))
        reasoning, content, closed = _split_thinking(no_markers)
        finish, usage = _finish_and_usage(res, cfg, closed, tool_calls)
        return content, reasoning, tool_calls, finish, usage

    if body.get("stream"):
        started = time.monotonic()

        def _chunk(delta, finish=None, usage=None):
            payload = {"id": "1", "object": "chat.completion.chunk",
                       "model": name,
                       "choices": [{"index": 0, "delta": delta,
                                    "finish_reason": finish}]}
            # usage rides on the final chunk (OpenAI's include_usage shape).
            # It was absent from the streaming path entirely, which is why pi
            # recorded input/output 0 for every streamed turn.
            if usage is not None:
                payload["usage"] = usage
            return f"data: {json.dumps(payload)}\n\n"

        def buffered(why):
            """Whole-turn fallback: one content chunk, then finish+usage.

            Used for tool-bearing turns and whenever the incremental path
            cannot start.  Emits well-formed `chat.completion.chunk' objects --
            an earlier rewrite yielded the bare 200-character text slice as the
            whole `data:' payload and dropped `delta.content' entirely, which
            no OpenAI-shaped client can read.
            """
            content, reasoning, tool_calls, finish, usage = shape(run())
            delta = {}
            if content:
                delta["content"] = content
            if reasoning:
                delta["reasoning_content"] = reasoning
            if tool_calls:
                delta["tool_calls"] = tool_calls
            yield _chunk(delta)
            yield _chunk({}, finish, usage)
            yield "data: [DONE]\n\n"
            _log(f"stream={why} finish={finish}"
                 f" prompt_tokens={usage['prompt_tokens']}"
                 f" completion_tokens={usage['completion_tokens']}"
                 f" reasoning_chars={len(reasoning)}"
                 f" content_chars={len(content)}"
                 f"{' TRUNCATED' if finish == 'length' else ''}"
                 f"{' EMPTY-ANSWER(think-block-never-closed)' if not content and finish == 'length' else ''}"
                 f"{_guard_log_suffix()}"
                 f" latency_s={time.monotonic() - started:.2f}")

        def events():
            # A tool call only parses once its `<|tool_call_start|>…` block is
            # complete (_extract_tool_calls runs ast.literal_eval over the
            # arguments), so streaming it as text would show the client raw
            # template markup.  Buffer those turns instead.
            if tools:
                yield from buffered("buffered-tools")
                return
            try:
                prompt = _render_chatml(messages, tools)
            except Exception as exc:
                print(f"note: rendered-prompt generation unavailable "
                      f"({type(exc).__name__}); falling back to a buffered "
                      f"stream", file=sys.stderr)
                yield from buffered("buffered-render-failed")
                return

            out_q = queue.Queue()
            holder = {}
            # Pass 1 is guarded: the streamer both pushes pieces to the SSE
            # queue and feeds the guard, so a runaway think block is stopped at
            # the char budget instead of running to the token cap.
            guard = _ThinkGuard(cfg.max_new_tokens)
            worker = threading.Thread(target=_stream_worker,
                                      args=(prompt, cfg, out_q, holder),
                                      kwargs={"guard": guard},
                                      daemon=True)
            worker.start()
            splitter = _ThinkSplitter()
            reasoning_chars = content_chars = 0
            # role arrives once, on the first chunk, per OpenAI's convention.
            # A bare role delta and no empty `content' key: pi skips
            # zero-length content deltas, but a client that does not would
            # open a text block before the thinking block and reorder the two.
            yield _chunk({"role": "assistant"})
            while True:
                item = out_q.get()
                if item is _STREAM_DONE:
                    break
                r_delta, c_delta = splitter.feed(item)
                if r_delta:
                    reasoning_chars += len(r_delta)
                    yield _chunk({"reasoning_content": r_delta})
                if c_delta:
                    content_chars += len(c_delta)
                    yield _chunk({"content": c_delta})
            worker.join(timeout=30)
            error = holder.get("error")
            if error is not None:
                # The headers are already on the wire, so a JSON error body is
                # not an option; log it and close the stream honestly.
                print(f"error: streaming generation failed "
                      f"({type(error).__name__})", file=sys.stderr)
                _log(f"stream=true ERROR={type(error).__name__}"
                     f" reasoning_chars={reasoning_chars}"
                     f" content_chars={content_chars}"
                     f" latency_s={time.monotonic() - started:.2f}")
                yield _chunk({}, "stop")
                yield "data: [DONE]\n\n"
                return
            tail_reasoning, tail_content, closed = splitter.flush()
            if tail_reasoning:
                reasoning_chars += len(tail_reasoning)
                yield _chunk({"reasoning_content": tail_reasoning})
            if tail_content:
                content_chars += len(tail_content)
                yield _chunk({"content": tail_content})
            res1 = holder.get("res")
            # Think-guard recovery.  Only pass 1 streams live: teaching
            # _ThinkSplitter about repeated think blocks is not worth it for a
            # path that fires on ~12% of turns and is already slow, so when
            # pass 1 ends inside `<think>` the bounded recovery loop runs
            # BUFFERED and its answer lands as one content chunk.  The loop's
            # extra reasoning stays internal -- pass 1's reasoning already went
            # to the client live -- and is never leaked as content.
            if not closed and guard.limit_chars is not None:
                guard_info["overflowed"] = guard.overflowed
                with _lock:
                    rec = _run_think_recovery(prompt, res1, cfg.max_new_tokens,
                                              body, bool(tools), guard_info)
                if rec.answer:
                    content_chars += len(rec.answer)
                    yield _chunk({"content": rec.answer})
                finish, usage = _finish_and_usage(rec, cfg, bool(rec.answer))
                closed = bool(rec.answer)   # a recovered turn is not truncated
            else:
                finish, usage = _finish_and_usage(res1, cfg, closed)
            yield _chunk({}, finish, usage)
            yield "data: [DONE]\n\n"
            _log(f"stream=true finish={finish}"
                 f" prompt_tokens={usage['prompt_tokens']}"
                 f" completion_tokens={usage['completion_tokens']}"
                 f" reasoning_chars={reasoning_chars}"
                 f" content_chars={content_chars}"
                 f"{' TRUNCATED' if finish == 'length' else ''}"
                 f"{' EMPTY-ANSWER(think-block-never-closed)' if closed is False else ''}"
                 f"{_guard_log_suffix()}"
                 f" latency_s={time.monotonic() - started:.2f}")

        return StreamingResponse(events(), media_type="text/event-stream")

    started = time.monotonic()
    content, reasoning, tool_calls, finish, usage = shape(run())
    _log(f"stream=false finish={finish}"
         f" prompt_tokens={usage['prompt_tokens']}"
         f" completion_tokens={usage['completion_tokens']}"
         f" tool_calls={len(tool_calls)}"
         f" reasoning_chars={len(reasoning)} content_chars={len(content)}"
         f"{' TRUNCATED' if finish == 'length' else ''}"
         f"{' EMPTY-ANSWER(think-block-never-closed)' if not content and finish == 'length' else ''}"
         f"{_guard_log_suffix()}"
         f" latency_s={time.monotonic() - started:.2f}")
    message = {"role": "assistant", "content": content}
    if reasoning:
        # Hand the chain of thought back separately instead of buried in
        # content: OpenAI-shaped clients that understand reasoning_content can
        # display or discard it, and everyone else simply gets a clean answer.
        message["reasoning_content"] = reasoning
    if tool_calls:
        message["tool_calls"] = tool_calls
    return JSONResponse({
        "id": "1", "object": "chat.completion", "model": name,
        "choices": [{"index": 0, "message": message, "finish_reason": finish}],
        "usage": usage})


@app.post("/v1/completions")
async def completions(request: Request):
    body = await request.json()
    prompt = body.get("prompt") or ""
    if isinstance(prompt, list):
        prompt = "".join(prompt)
    cfg = gen_config(body)
    name = os.path.basename(MODEL_DIR.rstrip("/"))
    with _lock:
        res = PIPELINE.generate(prompt, generation_config=cfg)
    raw = _result_text(res)
    reasoning, answer, closed = _split_thinking(raw)
    # A raw-completion endpoint has no reasoning channel, so an unclosed think
    # block falls back to the literal text rather than an empty string.
    text = answer if closed else raw
    finish, _usage = _finish_and_usage(res, cfg, closed)
    _log(f"endpoint=completions finish={finish}"
         f" prompt_chars={len(prompt)}"
         f" completion_tokens={_usage['completion_tokens']}"
         f" answer_chars={len(text)}"
         f"{' TRUNCATED' if finish == 'length' else ''}"
         f"{' THINK-BLOCK-UNCLOSED' if not closed else ''}")
    return JSONResponse({
        "id": "1", "object": "text_completion", "model": name,
        "choices": [{"index": 0, "text": text, "finish_reason": finish}]})


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host=os.environ.get("OPENVINO_HOST", "127.0.0.1"),
                port=int(os.environ.get("OPENVINO_PORT", "8093")),
                log_level="info")
