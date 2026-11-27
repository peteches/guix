# OpenVINO think budget: what was tried, measured, and abandoned

> **POSTSCRIPT (2026-11-26) — the shim this document analyses is retired.**
> `peteches/services/openvino-server.py`, `peteches/services/openvino.scm`
> (`openvino-service-type`) and `peteches/services/openvino-test.py` were all
> **deleted** on 2026-11-26. The fleet's `llm` provider is now configured as
> `(peteches services ovms)` serving **Qwen3.6-35B-A3B INT4** on comfyui's Arc
> Pro B70.
>
> **The think-budget problem was a property of LFM2.5, and it did not carry
> over.** On the agentic benchmark that retired that model, Qwen3.6 measured an
> **8-gram repeat count of 2** and a **largest thinking block of 2 972
> characters**, on the same task where LFM2.5 scored **47–1428** on the repeat
> metric and **40 121–94 722 characters** for its largest thinking block
> (across continuous batching, the 2026.5.0 weekly build, `--pipeline_type LM`,
> and the official non-abliterated INT4 export, which spiralled worst — one line
> repeated 1430×).
>
> So there is no think-budget mechanism to build for the current backend, and
> the two mechanisms analysed below were never going to generalise: both were
> attempts to cap a reasoning block that only LFM2.5 grew to that size.
>
> **This document is retained as a historical record. The body is unchanged** —
> its measurements of the shim, of the streamer-abort and forced-close
> re-prompt, and of xgrammar-constrained decoding are still accurate as a
> description of what that code did. Its classification recipe (how to read a
> `stream=true` compaction turn out of the log) is still cited by
> `peteches/home/modules/pi.scm`'s 2026-11-25 livelock post-mortem, which is
> also retained as history. Note the log it reads,
> `/var/log/openvino/openvino-lfm2.log`, no longer exists and is no longer
> shipped to Loki.

Status: **no think-budget mechanism is deployed.** `openvino-lfm2` on comfyui runs
one generation per request with no reasoning cap, which is what it did before any
of this work. This file exists so the next person does not spend a day
rediscovering that the two obvious mechanisms both fail, and why.

Everything here was measured on comfyui against `LFM2.5-24B-A2B-int8-ov` on the
Arc Pro B70, or against `openvino-genai` 2026.4.1 source in the Guix store, on
2026-11-24. Commits: `7fa63fd` (guard), `c8a95e0` (recalibration), `c9983c7`
(disable + post-mortem).

## The problem

The 24B export's `chat_template.jinja` ends every assistant turn with a literal
`<think>`, so completions always open inside a thinking block and the answer only
starts after `</tool_response>`. `max_new_tokens` has to cover **both**. When the
block never closes, `openvino-server.py`'s `_split_thinking()` returns an empty
answer — not a truncated one, an *empty* one — with `finish_reason: "length"`.
pi renders that as "Response was truncated before completion." and stalls.

Two signatures reach that state:

- budget exhausted mid-thought — `completion_tokens=8192`, `reasoning_chars=30368`
- EOS emitted **inside** the block — `completion_tokens=6399` of an 8192 budget

Baseline rate before any change: **11 empty answers in 96 generations (~11%)**,
concentrated in one hard session.

## Mechanism 1: abort the streamer, force-close, re-prompt

`openvino_genai`'s streamer callback can stop generation: returning a truthy
value maps to `StreamingStatus::STOP` (`text_streamer.cpp:106`), which calls
`handle->stop()` (`generation_handle.hpp:131`, default finish reason `STOP`) and
keeps the tokens produced so far. Verified against the source checkout before
building anything.

The guard counted reasoning characters, aborted at a budget, appended
`</tool_response>` to the prompt and re-generated to collect the answer — with a
bounded loop, separate reasoning/answer tracking (the model re-opens `<think>`
after a forced close, so naive concatenation leaks markup into `content`), and a
char-derived token fallback so a client's `max_tokens` could never be exceeded.

**It failed twice, for two different reasons.**

First calibration, `think-budget 6144` with the script's `chars-per-token` 4.0:
```
15:40:03 finish=length completion_tokens=8192 reasoning_chars=30936 content_chars=0
         think_overflow=1 passes=3 pass2_tokens=1528 THINK-OVERFLOW(forced-close)
```
The model's reasoning runs 3.69 chars/token, not 4.0, so a 24576-char cap was
really 6664 tokens — pass 1 ate 81% of pi's 8192 before being cut, leaving 1528
tokens for the ladder, and the recovery rule halved that to 764 per pass. A
healthy think block on this box closes in 1300–2000 tokens, so both recovery
passes were cut before either could answer.

Second calibration, `think-budget 4096` / `answer-reserve 1024` /
`chars-per-token 3.7`, chosen so pass 1 would leave ~4096 tokens:
```
16:20:22 finish=length completion_tokens=8192 reasoning_chars=30936 content_chars=0
         think_overflow=1 passes=3 pass2_tokens=3488 THINK-OVERFLOW(forced-close)
```
Six consecutive failures with 3488 and 3274 tokens of recovery available — three
to four times what a healthy block needs — every one `content_chars=0`.
Re-prompting mid-turn does not break a spiral, it restarts one.

And the cap landed **inside** the healthy distribution. Every turn that succeeded
reasoned below the 15155-char cap; every turn that failed reasoned above it, with
no overlap:
```
16:19:06 reasoning_chars=6597    -> tool_calls
16:28:39 reasoning_chars=14850   -> stop, content_chars=4072
16:30:09 reasoning_chars=14638   -> tool_calls
16:20:22 reasoning_chars=15168   -> passes=3 content_chars=0
16:29:33 reasoning_chars=15165   -> passes=3 content_chars=0
16:21:42 reasoning_chars=30577   -> passes=3 content_chars=0
16:36:27 reasoning_chars=27443   -> passes=3 content_chars=0
```
Pre-guard history has healthy turns reasoning to 24201 chars and still answering.

Hourly outcome, which is the number that decided it:

| window | generations | empty answers | rate |
|---|---|---|---|
| guard absent (10h–14h) | 96 | 11 | ~11% |
| guard enabled (15h–16h) | 46 | 14 | **~30%** |
| guard disabled (17h–22h) | 53 | **0** | 0% |

The guard roughly tripled the failure rate. It is removed, not disabled behind a
flag: a config value that re-introduces a measured 30% failure rate is a loaded
gun. The instrumentation stayed — `finish_reason`, token/char counts,
`TRUNCATED`, `EMPTY-ANSWER` — and the log now ships to Loki via comfyui's alloy
`log-files`, which is the one durable gain from all of it.

## Mechanism 2: grammar-constrained decoding (xgrammar)

`site/docs/guides/structured-output.mdx` documents the supported way to blend
free-form generation with structural constraints in a single `generate()` call,
and the binding's own `Tag` docstring uses
`Tag("<think>", AnyText(), "</tool_response>")` as its example — "represents
thinking portion of the model output". Structured output works under
`ContinuousBatchingPipeline` too, not just `LLMPipeline`: the sampler is shared
and keys a per-request `LogitProcessor` on `request_id`
(`sampler.cpp:1729-1740`), and `LLMPipeline` on the default PA backend *is* a CBP
(`llm/pipeline.cpp:269-274`).

Requires `ENABLE_XGRAMMAR=ON`, which this repo builds OFF. Two traps, both silent:

1. genai rewrites `${xgrammar_SOURCE_DIR}/cmake/config.cmake` **in place** to flip
   `XGRAMMAR_BUILD_PYTHON_BINDINGS` OFF (load-bearing — xgrammar's own
   `config.cmake` uses a plain `set()` that shadows genai's `FORCE`'d cache
   variable). So `FETCHCONTENT_SOURCE_DIR_XGRAMMAR` cannot point at a read-only
   store path; copy the tree into the writable build dir first.
2. xgrammar builds a **static** archive with `-flto=auto`, while
   `libopenvino_genai.so` links without `-flto`. GCC's default is thin
   bytecode-only objects, so the linker drops every archive member and the build
   **succeeds** with xgrammar's symbols missing. It fails at `dlopen` with
   `undefined symbol: _ZN8xgrammar14GrammarMatcherC1E…`. Strip `-flto=auto`.

Vendoring is otherwise trivial: v0.1.31 = commit
`aa44ded202fdaa2cb55f48deb31d17c9822a8a1f`, `(recursive? #t)`, `guix hash -r` of
the `.git`-stripped tree = `07nsh6xbzdmilkcvk9phd52izy7049mzm0j3w4pw4db84h7bz9h5`.
3.9 MB, C++17, **no pcre2 dependency** (`grep -ril pcre2` over the tree is empty
— it compiles regexes into its own grammar), only the dlpack submodule. Build
takes ~29 minutes offloaded to `guix-build`.

Measured, on the real 24B on the B70 and on the 8B on CPU:

| grammar | cost | result |
|---|---|---|
| `json_schema` (tool call) | 0.78 s, 12 tokens | valid JSON, `finish=STOP` ✓ |
| `Tag(Regex{0,200}, end=close) + AnyText` | 53.5 tok/s | forced close at exactly 200 chars, 1182-char answer ✓ |
| `Regex{0,2000}` | **2.27 s/token GPU**, 3.0 s/token CPU | unusable |
| `Tag(AnyText, end=close) + AnyText` | 125 tok/s (free) | ran to the cap, never emitted the close tag ✗ |

Bounded repetition costs **quadratically in the bound**: 200 → 53 tok/s,
2000 → 0.44 tok/s. Each distinct automaton state costs a fresh token-mask
computation over the ~125k vocabulary. It is host-side work, so the GPU does not
help (2.27 s/token there vs 3.0 on CPU). A usable think budget needs 15–25k
chars, which extrapolates to ~100 s/token.

The unbounded form is free but useless: forbidding EOS before the close tag does
not make the model close it, it just converts a 3000-token empty answer into an
8192-token empty answer.

So the feature that motivated the rebuild cannot be built from it. `json_schema`
for guaranteed-shape tool calls does work and is cheap, but no malformed tool
call has ever been observed here — the existing `ast.literal_eval` parsing got
every call right across 39 tests. Package left OFF; recipe recorded in
`peteches/packages/openvino.scm`'s `ENABLE_XGRAMMAR=OFF` comment.

## Ruled out along the way

Worth recording because each looked like the answer at the time:

- **`apply_chat_template` double-wrapping.** `openvino-server.py` renders ChatML
  itself and passes it to `generate([prompt])`, which re-wraps it as one user
  message because `cfg.apply_chat_template` defaults to `True` (measured: 58 vs
  49 prompt tokens). Real oddity, **not the cause**: on the same 16k prompt the
  model failed identically with the flag on and off.
- **Sampling.** Five settings × 2 reps at 16k prompt tokens, including the exact
  production tool preset (`temperature 0.6 / top_p 0.95 / repetition_penalty
  1.15`): **10/10 produced answers**.
- **Prompt size.** 9k → 64 456 prompt tokens at the production preset: **9/9
  produced answers**, all `finish=stop`.
- **Multi-turn structure.** Synthetic conversations with 2–8 tool round-trips,
  16k–52k prompt tokens, with and without `reasoning_content` on assistant
  messages: **8/8 correct answers**. This exercises the `keep_thinking` /
  `<tool_response>tool` paths in `_render_chatml`.
- **The failing conversation itself.** Replayed from pi session `01a20d9e`'s
  JSONL — same system prompt sections, same compaction summary, kept entries
  31–73 including the 42 149-char and 12 989-char tool results — at 45 466
  prompt tokens, plus five bisect variants: **6/6 returned a sensible tool
  call** in 114–758 tokens.

33 synthetic runs and 6 replays, zero failures. Those failures were the guard's.

## What is left

The original ~11% is unexplained and unreproduced. One clue: several pre-guard
failures were `stream=true` at only **3177 prompt tokens with no tools** — the
shape of pi's auto-compaction summarisation request, not of an agent turn.
`peteches/home/modules/pi.scm` already records that this model "does not
summarise an 80k context reliably" and sizes the llm provider's `reserveTokens`
against a measured 1.43× under-estimate.

If empty answers reappear, read the Loki log first and classify by shape before
touching the server:

```bash
ssh comfyui.spaniel-cordylus.ts.net \
  'sudo grep -E "EMPTY-ANSWER" /var/log/openvino/openvino-lfm2.log | tail -20'
```

- small prompt, `stream=true`, no `tool_calls=` field → compaction path; fix in
  `peteches/home/modules/pi.scm`, not here
- large prompt, `stream=buffered-tools` → agent turn; check what the tool results
  in that conversation look like before assuming the model is at fault
- anything with `THINK-OVERFLOW` in it → the guard is back somehow; it should not
  exist any more
