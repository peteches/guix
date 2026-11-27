;;; peteches/home/modules/pi.scm — home service type for the `pi` coding
;;; agent (https://pi.dev, packaged in peteches/packages/pi-coding-agent.scm).
;;;
;;; Mirrors (peteches home modules claude)'s config-directory mechanism:
;;; symlinks each child of CONFIG-DIRECTORY into ~/.pi/agent/ -- pi's
;;; getAgentDir() (dist/config.js) resolves to join(homedir(), ".pi",
;;; "agent"), NOT ~/.pi/ itself; auth.json, models-store.json and
;;; models.json all live there. Currently that's just models.json
;;; (configs/pi/defaults/models.json), which declares
;;; custom OpenAI-completions-compatible providers/models per pi's
;;; core/model-config.js schema — pi loads this file itself at startup, so
;;; unlike Claude Code's ~/.claude.json there is no runtime-mutated state
;;; here to protect and no activation script is needed.
;;;
;;; configs/pi/defaults/agents/ carries user-level overrides for the
;;; pi-interactive-subagents bundled agent definitions (researcher.md,
;;; scout.md, worker.md). The extension resolves each agent name in order:
;;; <cwd>/.pi/agents/<name>.md, then <agent-dir>/agents/<name>.md, then its
;;; own store-bundled agents/ — so the files here shadow the packaged ones
;;; for every account. They are byte-identical to the bundled definitions
;;; except for the `model:'/`thinking:' frontmatter lines: the fork pins
;;; openrouter/z-ai/glm-5.3, and none of the workstation accounts have an
;;; openrouter API key, so pinned spawns died with "No API key found for
;;; openrouter". With the pin removed the spawned pi gets no --model flag
;;; and pi's startup model selection (findInitialModel in pi's
;;; core/model-resolver.js) falls through to the saved default from
;;; settings.json -- which for these accounts is the llm (OVMS/Qwen3.6)
;;; model, since
;;; configs/pi/defaults/settings.json declares defaultProvider/defaultModel
;;; and the static apiKey "local" in models.json counts as configured auth
;;; for that provider.
;;;
;;; DO NOT re-add a `model:' pin here without checking that the provider is
;;; actually declared in configs/pi/defaults/models.json AND that its backend
;;; is running.  It has now been done twice and broken both times: the fork's
;;; openrouter/z-ai/glm-5.3 (no API key on any workstation account), then
;;; strata/qwen3.8-flash-next in commit 36790e3 -- which was reasonable when
;;; settings.json still defaulted to a dead koboldcpp, but by 2026-11-24
;;; `strata' was neither a provider in models.json (it declares only llm and
;;; koboldcpp) nor a running service (herd showed it stopped on comfyui), so
;;; every scout/worker/researcher spawn died with exit code 1 in about a
;;; second.  Inheriting the settings.json default is the intended behaviour and
;;; is what the parent session itself uses; the pin only ever buys a way to
;;; diverge from it.  A per-agent pin is still legitimate for an agent that
;;; genuinely needs a different backend -- just declare that provider first.
;;;
;;; Keep the bodies in sync with the fork when the package pin is bumped.
;;;
;;; configs/pi/defaults/settings.json (if present) is NOT symlinked like
;;; every other config-directory child -- pi's SettingsManager actively
;;; rewrites ~/.pi/agent/settings.json at runtime (defaultProvider,
;;; defaultModel, lastChangelogVersion, compaction, etc. all get saved
;;; there), same category of problem as Claude Code's ~/.claude.json (see
;;; (peteches home modules claude)'s module docstring). A hard symlink into
;;; the store would either fight every runtime save (if pi writes through
;;; it) or -- what actually happened on the peteches account the first time
;;; this was tried, 2026-09-27 -- get silently skipped by Guix Home's
;;; file-conflict handling on any account that already has a real
;;; settings.json, so the declared defaults never reach it and the mistake
;;; is invisible (no error, no backup file, just silence). Instead
;;; HOME-PI-SETTINGS-ACTIVATION below jq-merges configs/pi/defaults/
;;; settings.json into ~/.pi/agent/settings.json on every activation
;;; (creating it if absent), with the declared defaults taking precedence
;;; on any overlapping key -- the same "shell out at activation, merge with
;;; jq instead of symlinking" idiom claude.scm uses for ~/.claude.json's
;;; oauth.scopes patch.  That precedence is what makes a fleet default like
;;; defaultProvider/defaultModel or httpIdleTimeoutMs survive pi's own
;;; runtime writes until the next model switch rewrites them, and it only
;;; works if the merge actually runs: the one-shot guix-home-service-type
;;; starts with hands /activate an environment of HOME plus one marker and
;;; no PATH, so the script has to carry its own -- see
;;; %HOME-PI-SYSTEMS-MERGE-SCRIPT.
;;;
;;; The default models.json wires up koboldcpp.ts.peteches.co.uk (Caddy's
;;; reverse proxy onto the comfyui VM's koboldcpp instance) and
;;; llm.ts.peteches.co.uk (Caddy's reverse proxy onto the same VM's
;;; OpenVINO Model Server — Qwen3.6-35B-A3B INT4 on the Intel Arc Pro B70,
;;; see peteches/systems/comfyui.scm) as custom providers. configs/pi/
;;; defaults/settings.json declares llm as the default model
;;; (defaultProvider/defaultModel), so pi's startup selection lands there
;;; on every account that uses this config.  The llm provider replaced
;;; strata.ts.peteches.co.uk (Qwen3.8-Flash-Next IQ2_XS on the same B70)
;;; when strata was stopped; Caddy's route was renamed in the same change,
;;; so the old hostname no longer resolves to anything.
;;;
;;; BACKEND HISTORY, because a lot of the measurement below is attached to
;;; the older ones.  llm.ts.peteches.co.uk:8093 has had three backends:
;;;   1. a 1095-line hand-written OpenAI shim over openvino-genai
;;;      (peteches/services/openvino-server.py) serving LFM2.5-24B-A2B int8;
;;;   2. Intel's OpenVINO Model Server (peteches/services/ovms.scm) serving
;;;      the same LFM2.5 export;
;;;   3. OVMS serving Qwen3.6-35B-A3B INT4 — CURRENT, since 2026-11-26.
;;; The shim, its service type and its test probe were deleted at (3);
;;; LFM2.5 was retired because it degenerated on multi-turn agent work under
;;; two independent serving stacks (8-gram repeat 47-407 and thinking blocks
;;; to 89 199 chars on OVMS continuous batching, and tool markup leaking as
;;; text under koboldcpp), while Qwen3.6 on the same OVMS scored 8-gram
;;; repeat 2, largest thinking block 2 972 chars, 6 clean tool calls and 3/3
;;; correct in 62 s.  The full table and the ruled-out causes are in
;;; peteches/systems/comfyui.scm's ovms instance comment and
;;; (peteches services ovms)'s header.  Anything below measured "on this box"
;;; before 2026-11-26 was measured against LFM2.5 and is labelled as such.
;;;
;;; ── THREE COMPAT FLAGS THAT ARE LIVE REQUIREMENTS, NOT PREFERENCES ──
;;;
;;; models.json is plain JSON and cannot carry comments, so this module is
;;; where the client-side flags the backend's correctness depends on get
;;; their rationale.
;;;
;;; 1. `"supportsDeveloperRole": false` on the llm provider is MANDATORY.
;;;    pi computes the instruction role as
;;;      instructionRole = model.reasoning && compat.supportsDeveloperRole
;;;                            ? "developer" : "system"
;;;    (dist/bundle/chunks/openai-completions-*.js, in convertMessages).  Our
;;;    llm provider sets `"reasoning": true`, and pi's own detectCompat()
;;;    DEFAULTS supportsDeveloperRole TO TRUE for a custom provider — the
;;;    expression is `isOpenRouterDeveloperRoleModel || !isNonStandard &&
;;;    !isOpenRouter`, and a plain custom baseUrl is neither non-standard nor
;;;    OpenRouter.  So omitting the key silently opts INTO `developer`.
;;;    Qwen3's chat template validates message roles; measured against this
;;;    endpoint: role=system OK, role=user OK, role=developer HTTP 400
;;;    "Unexpected message role."  Without the explicit false, EVERY pi
;;;    request 400s in under a second.  LFM2.5's template tolerated
;;;    `developer`, which is why six hours of LFM2.5 testing never surfaced
;;;    this.  Every one of pi's built-in model definitions sets
;;;    `supportsDeveloperRole: !1` explicitly, so the trap only bites custom
;;;    providers declared here.  This is invisible from the server side:
;;;    (peteches services ovms) logs a clean 400 and nothing else.
;;;
;;; 2. `"supportsUsageInStreaming": true` on llm, and it has to be explicit.
;;;    THIS FLAG IS THE SINGLE POINT OF FAILURE FOR THE ENTIRE reserveTokens
;;;    DERIVATION BELOW, and it is invisible from either side alone: the
;;;    server looks perfectly healthy whether or not the client asks, and the
;;;    client has no way to notice that it stopped asking.
;;;
;;;    Client half, verbatim from pi's buildParams
;;;    (dist/bundle/chunks/openai-completions-*.js):
;;;      compat.supportsUsageInStreaming !== !1 &&
;;;        (params.stream_options = {include_usage: !0})
;;;    i.e. pi only REQUESTS streaming usage when the flag is not explicitly
;;;    false.  Leaving it false drops pi back onto its chars/4 context
;;;    estimate, which is precisely the 1.43x-low heuristic the sizing below
;;;    exists to survive.
;;;
;;;    Server half, measured 2026-11-26 on OVMS 2026.4.1 / comfyui / Arc Pro
;;;    B70, always with `stream_options: {"include_usage": true}` in the
;;;    request.  Both models and both servable shapes return a real usage
;;;    object in the final chunk, including on a truncation:
;;;
;;;      Qwen3.6-35B-A3B int4 (the current backend)
;;;        stream + tools : order=R T F=tool_calls
;;;                         usage={'prompt_tokens': 289,
;;;                                'completion_tokens': 82,
;;;                                'total_tokens': 371}
;;;        stream truncate: order=R F=length
;;;                         usage={'prompt_tokens': 16,
;;;                                'completion_tokens': 8,
;;;                                'total_tokens': 24}
;;;      LFM2.5-24B-A2B int8 (the retired backend, same server)
;;;        stream + tools : final chunk usage={'prompt_tokens': 83,
;;;                                'completion_tokens': 144,
;;;                                'total_tokens': 227}
;;;        stream prose   : final chunk usage={'prompt_tokens': 21,
;;;                                'completion_tokens': 132,
;;;                                'total_tokens': 153}
;;;        n chunks w/ usage: yes
;;;
;;;    HISTORY OF THE `false`, stated no more strongly than the evidence
;;;    allows: it appears to predate the shim's streaming-usage fix and was
;;;    never revisited.  What is confirmable is that the shim's `_chunk`
;;;    helper attached `usage` to the final chunk UNCONDITIONALLY -- not gated
;;;    on `stream_options.include_usage` -- with a comment noting that usage
;;;    had been "absent from the streaming path entirely, which is why pi
;;;    recorded input/output 0 for every streamed turn".  So the flag looks
;;;    stale for the shim too.  The shim is deleted in this change and can no
;;;    longer be re-measured, so no causal claim is made here.
;;;
;;; 3. `"supportsUsageInStreaming": false` on the koboldcpp provider, which
;;;    is the opposite and was MEASURED, not assumed: koboldcpp 1.118.1
;;;    returns `usage: None` in the stream even with
;;;    `stream_options: {"include_usage": true}` set, while non-streaming
;;;    usage works fine.  It previously declared true here, which would
;;;    silently push pi onto a chars/4 estimate it believed was
;;;    provider-accurate.  Declaring false at least makes pi's accounting
;;;    honestly heuristic for that provider.
;;;
;;; ── contextWindow AND maxTokens ──
;;;
;;; Both must track the koboldcpp service on comfyui
;;; (peteches/systems/comfyui.scm):
;;;
;;;   contextWindow = --contextsize - --genlimit   (163840 - 32768 = 131072)
;;;   maxTokens     = --genlimit                                      (32768)
;;;
;;; koboldcpp reserves the generation budget *inside* the context window
;;; rather than adding to it, so the prompt space a client may actually use
;;; is that difference, not the raw --contextsize. Declaring contextWindow as
;;; --contextsize itself (196608, as it was until 2026-10-04) makes pi
;;; compact against a window the server cannot accept: sessions grew past
;;; the real 163840 limit, every subsequent turn context-shifted and
;;; re-prefilled ~160k tokens, and koboldcpp's log showed a 924-request
;;; pile-up at exactly 163840 tokens. Measured over one week that was 31.4
;;; GPU-hours of prefill -- 45% of all request time, from 7.4% of requests.
;;;
;;; The llm entry's contextWindow is the MODEL's own window, not a server
;;; flag, and OVMS enforces it: `max_position_embeddings: 262144` in the
;;; Qwen3.6 IR's config.json, confirmed empirically rather than trusted --
;;; a 224 017-token prompt was accepted, a 280 017-token prompt returned
;;; `HTTP 400 ... Number of prompt tokens: 280017 exceeds model max length:
;;; 262144`.  So contextWindow is 262144 and there is a hard server-side
;;; ceiling behind it.  Unlike koboldcpp, OVMS reserves nothing inside that
;;; window, so the prompt space a client may use is contextWindow minus its
;;; own generation budget: 262144 - 24576 = 237568.
;;;
;;; That is the ONE structural improvement over the shim worth naming here:
;;; openvino-server.py never checked
;;; prompt + max_new_tokens <= max_position_embeddings, so an overshoot came
;;; back as a 500 that pi retried three times.  OVMS returns a precise HTTP
;;; 400 naming both numbers, so the same overshoot is loud instead of
;;; silent.  It is still a failed turn -- which is exactly why the compaction
;;; trigger below has to leave room for a full maxTokens generation -- but
;;; it now says why.
;;;
;;; maxTokens is 24576, unchanged across the backend move, because it is
;;; derived from pi's own compaction arithmetic rather than from the model:
;;; the SAME field is the per-turn generation cap and the compaction summary
;;; budget, and maxTokens is a permanent reservation against prompt space
;;; whether or not a turn generates it.  M must be >= the largest summary a
;;; long session produces, and summaries ACCUMULATE -- the
;;; UPDATE_SUMMARIZATION_PROMPT pi sends on every compaction after the first
;;; says "PRESERVE all existing information from the previous summary, ADD
;;; new progress".  The 15210/15156-token checkpoints recorded in the
;;; livelock post-mortem below prove the accumulated case reaches ~15k on a
;;; long session, and that session's thinking blocks ran to ~8500 tokens, so
;;; 15000 + 8500 does not fit in 16384 and M = 24576 is the smallest round
;;; number that covers it.  (Those two token counts are LFM2.5-era
;;; measurements; nothing equivalent has been measured for Qwen3.6, whose
;;; largest observed thinking block on the agentic benchmark was 2 972
;;; chars -- an order of magnitude smaller.  M is left at 24576 rather than
;;; shrunk on that basis, because the summary half of the budget is a
;;; pi-side property and the accumulated-checkpoint evidence has not been
;;; re-taken.)
;;;
;;; ── compaction.modelOverrides: DERIVED, NOT TUNED ──
;;;
;;; reserveTokens in configs/pi/defaults/settings.json is derived rather than
;;; tuned, because pi's context accounting is provider-accurate only while
;;; the last assistant usage entry is newer than the newest context_edit or
;;; compaction entry -- `estimateProjectedContextTokens()`
;;; (dist/bundle/chunks/chunk-OJP47DM6.js) throws the usage figure away as
;;; soon as one of those is appended after it, and every later turn is
;;; measured by the chars/4 heuristic that replaces it. Measured on pi
;;; session 01a188e2 (2026-10-30, koboldcpp provider): that heuristic put
;;; the context at 161700 tokens when the provider had just reported 198345
;;; -- 1.22x low -- and a koboldcpp request that cleared the pre-prompt
;;; threshold check carried 163839 real tokens, one token of room at
;;; --contextsize 163840, and came back as one token of text with
;;; `finish_reason: length'. That is at least 1.43x low.
;;;
;;; That 1.43x is a koboldcpp/Qwen measurement and does NOT carry over to
;;; llm -- and the llm-side figure that used to sit here (LFM2.5 decoded at
;;; 4.4 chars/token, 36236 reasoning chars over 8192 generated tokens,
;;; openvino-lfm2.log 2026-11-25, so pi's chars/4 slightly OVER-estimated
;;; it) is a measurement of a retired model, not of Qwen3.6.  No
;;; chars/token ratio has been measured for Qwen3.6.  That is a reason to
;;; keep llm's `supportsUsageInStreaming` true rather than to relax the
;;; sizing: with real usage figures the heuristic is only a fallback for the
;;; turns immediately after a compaction, which is where the slack matters.
;;;
;;; TWO numbers come out of reserveTokens and they must be held apart.
;;; Write M = model.maxTokens and R = reserveTokens:
;;;
;;;   trigger         contextTokens > contextWindow - R
;;;   summary budget  min(floor(0.8 * R), M)
;;;
;;; (shouldCompact and generateSummaryWithUsage, both in pi's
;;; dist/core/compaction/compaction.js.)  While 0.8 * R exceeds M the second
;;; is decided by models.json ALONE, and moving R moves only the trigger.
;;; Hence the sizing rule, worth stating once because the numbers before
;;; 2026-11-25 violated it by a factor of twelve:
;;;
;;;   R > 1.25 * M   budget is M, but the trigger sits LOWER than it could --
;;;                  window given up and nothing bought with it
;;;   R = 1.25 * M   budget is exactly M and the trigger is as high as the
;;;                  window allows: the optimum
;;;   R < 1.25 * M   budget becomes 0.8 * R and SHRINKS below M -- the
;;;                  generation cap is now the wasted half
;;;
;;; CURRENT ARITHMETIC for llm/Qwen3.6-35B-A3B-int4-ov, at the optimum:
;;;
;;;   M            = 24576                      (models.json)
;;;   R            = 1.25 * M = 30720           (settings.json)
;;;   trigger      = 262144 - 30720 = 231424
;;;   summary budget = min(floor(0.8 * 30720), 24576)
;;;                  = min(24576, 24576) = 24576
;;;   worst case   = a prompt sitting exactly at the trigger plus a full
;;;                  generation = 231424 + 24576 = 256000 < 262144.  That FITS
;;;                  **only if the trigger figure is accurate**, and it is an
;;;                  ESTIMATE: this same file documents pi's chars/4 fallback as
;;;                  measured up to 1.43x LOW, and 191424 * 1.43 is ~274k, past
;;;                  the ceiling.  Two things BOUND that risk without removing
;;;                  it -- `supportsUsageInStreaming: true' means the provider's
;;;                  real token counts are used except immediately after a
;;;                  compaction or context_edit entry (see the
;;;                  estimateProjectedContextTokens note above), and OVMS
;;;                  answers an over-ceiling prompt with a precise HTTP 400
;;;                  naming both numbers instead of the shim's silent 500 that
;;;                  pi retried three times.  So the residual failure mode is a
;;;                  LOUD 400 on post-compaction turns, not a silent one-token
;;;                  reply -- acceptable, but do not read 256000 < 262144 as a
;;;                  guarantee.
;;;   summarised prefix = 231424 - 40000 (keepRecentTokens) = 191424
;;;                  estimated, against a server ceiling verified to accept
;;;                  224 017 real tokens
;;;
;;; The previous R was 40960, sized against a 131072 window where it
;;; deliberately OVERSHOT the 1.25 * M optimum to buy 16384 tokens of slack
;;; for two shim-era risks: openvino-server.py never checked the prompt
;;; ceiling (so an overshoot was a silent 500 pi retried three times), and
;;; the chars/4 estimate was measured 1.43x LOW on koboldcpp's tokenizer.
;;; With a 262144 window, a server that returns a precise 400 at the
;;; ceiling, and provider-accurate usage restored, the optimum is both
;;; correct and safe -- 256000 of 262144 used in the worst case, with
;;; 6144 tokens of margin.  Do not "restore slack" by raising R again
;;; without re-deriving it: R = 40960 against this window would put the
;;; trigger at 221184 and the worst case at 245760, buying nothing but a
;;; lower usable window, since the budget is M either way.
;;;
;;; koboldcpp's override (reserveTokens 49152) is unchanged and is sized by
;;; the other half of the rule: it sets that model's summarisation output
;;; budget, min(floor(0.8 * 49152), 32768) = 32768, which lands on the
;;; generation budget that server already accepts (--genlimit 32768) instead
;;; of the 13107 the 16384 default yields -- not cosmetic, since the two
;;; largest checkpoints this box produced came back at 15210 and 15156
;;; tokens, over the budget pi had asked for.  Its trigger is
;;; 131072 - 49152 = 81920 estimated, and 81920 * 1.43 < 131072.
;;;
;;; ── THE 2026-11-25 AUTO-COMPACTION LIVELOCK (HISTORY, LFM2.5) ──
;;;
;;; Kept in full because it is the evidence for the sizing rule above and
;;; the failure mode is silent.  Measured from
;;; /var/log/openvino/openvino-lfm2.log -- the shim's log, which no longer
;;; exists; the classification recipe is at the end of
;;; docs/openvino-think-budget.md (retained as history).  The compaction path
;;; is the `stream=true' shape: small prompt, no tool_calls field.
;;;
;;;   - 56 of 155 such generations ended finish=length: a 36% failure rate
;;;     on the one path already known to be fragile.
;;;   - The failures are an unclosed `<think>': completion_tokens=8192 (the
;;;     whole budget), reasoning_chars 34564-37575, content_chars=0.  At
;;;     LFM2.5's measured 4.4 chars/token that is ~8500 tokens of thinking
;;;     and nothing left for the summary -- pi's "generation hit the token
;;;     cap and the summary is incomplete", verbatim.
;;;   - The SUCCESSES are pinned to the same ceiling: 8104, 8012, 7984,
;;;     7375, 7178 completion tokens out of 8192.  The budget was not
;;;     occasionally tight, it was the operating point.
;;;   - One prompt (prompt_tokens=4884) was regenerated five times at ~65 s
;;;     intervals with byte-identical reasoning_chars.  That is the
;;;     livelock rather than a coincidence: on a length stop pi throws,
;;;     _runAutoCompaction persists NOTHING and keeps no cooldown, so the
;;;     next turn re-triggers on an unchanged context; and a tool-less
;;;     request kept the shim's own model temperature (its TOOL_TEMPERATURE
;;;     warm-up applied only when `tools' was present), so the retry was
;;;     deterministic and failed identically.  Every turn then paid 8192
;;;     tokens at ~136 tok/s = the 60 s stall, forever, at 25% of a 131k
;;;     window that was never reached.
;;;
;;; The R = 98304 / M = 8192 pair then in force was the `R > 1.25 * M' case
;;; taken to an extreme: 1.25 * 8192 = 10240 would have triggered at 120832
;;; (92% of the window) with the identical 8192 budget, but R was 98304 and
;;; triggered at 32768 (25%).  Two thirds of a 131k window was discarded for
;;; free.  That is the whole answer to "what is the point of a 131k context"
;;; -- nothing was enforcing 25%, it was a mis-derived number that the old
;;; commentary here then rationalised.  It was dropped 49152 -> 98304 in
;;; commit 8becf7b for exactly the wrong reason ("compact earlier and there
;;; will be less to summarise"), which kept shrinking the conversation being
;;; summarised into the same 8192-token box -- and the box was the thing
;;; overflowing.
;;;
;;; WHICH PARTS OF THAT ARE MODEL-SPECIFIC.  The livelock MECHANISM (pi
;;; throws on a length stop, persists nothing, keeps no cooldown, so an
;;; unchanged context re-triggers forever) is a pi-side property and applies
;;; to any backend.  The TRIGGER for it here was LFM2.5's think-first
;;; behaviour spending the whole budget before emitting content; whether
;;; Qwen3.6 has the same exposure was not measured directly, but its largest
;;; observed thinking block was 2 972 chars against LFM2.5's
;;; 40 121-89 199, and M = 24576 now gives the summary 3x the box it was
;;; failing in.
;;;
;;; What the wider window cost under LFM2.5 was prefill, and that objection
;;; has largely evaporated: the cold-prefill figures recorded then (4000-5500
;;; tok/s at 52-64k prompts on the B70) are LFM2.5-era.  Qwen3.6 on OVMS
;;; measures 1453 T/s cold on a 44 817-token prompt (30.84 s) but 11 850 T/s
;;; effective (3.78 s) when the same prompt repeats, because
;;; --enable_prefix_caching is on -- an 8.2x speedup, and multi-turn agent
;;; work re-sends almost the whole conversation every turn.  That flag is
;;; what makes a 231 424 trigger affordable at all; koboldcpp on the same
;;; card, which has no GPU-resident prefix cache, spent 99 minutes and 48
;;; requests without finishing the task OVMS finished in 62 seconds.  See
;;; (peteches services ovms).
;;;
;;; The LFM2.5-era KV arithmetic recorded here (10 of 40 layers full
;;; attention, 20 KiB/token, 2.5 GiB for the whole 131072 window) does not
;;; describe Qwen3.6 and is not replaced by an equivalent calculation.  What
;;; was observed instead: OVMS allocated 1.0 GB of KV cache dynamically and
;;; peaked at 67.6% of it during the agentic test.
;;;
;;; If length-stops on the compaction path return despite the headroom, the
;;; next lever is NOT another trigger move -- it is a `session_before_compact'
;;; extension (pi's docs/compaction.md, shippable through
;;; EXTRA-EXTENSION-FILES below) that summarises with its own budget and
;;; RETURNS a compaction result instead of throwing, so a cap-hit degrades
;;; to a partial checkpoint rather than stalling every subsequent turn.
;;;
;;; keepRecentTokens must never be LOWERED below the point where a
;;; truncated-response recovery still finds something older than the cut
;;; point to summarize -- a compact-and-retry recovery that finds nothing
;;; returns early without retrying, which is the "Response was truncated
;;; before completion." stall that session hit four times.  llm RAISES it
;;; from pi's 20000 default to 40000, for two reasons that both point the
;;; same way: it keeps twice as much of the agent's recent verbatim work
;;; (large tool output is what eats a session here, and that is exactly what
;;; a lower value would hand to the summariser), and it shrinks the
;;; serialised prefix the summary request has to carry.  The cost is a higher
;;; post-compaction floor -- context lands at roughly keepRecentTokens +
;;; summary, so up to ~64k of the 262144 window -- which is the point: fewer,
;;; larger compactions rather than free ones.
;;;
;;; INVARIANT, and the failure mode is worse than the one being fixed:
;;; keepRecentTokens must stay comfortably below contextWindow -
;;; reserveTokens.  If it ever approaches the trigger, findProjectedCutPoint
;;; keeps everything, prepareCompaction finds nothing to summarise and
;;; returns undefined, and _runAutoCompaction returns false WITHOUT an error
;;; -- shouldCompact stays true, so every turn silently re-attempts a
;;; compaction that can never do anything.  No message, no log line, just a
;;; session that has stopped making progress.  40000 against a 231424
;;; trigger leaves 5.8x of room (it was 2.25x against the old 90112); do not
;;; close that gap without re-checking it.
;;;
;;; The koboldcpp provider entry and its `pi-koboldcpp' shell function below
;;; (a NODE_TLS_REJECT_UNAUTHORIZED=0 wrapper) replaced nug's own koboldcpp
;;; instance and its Tailscale IPv6-literal baseUrl following nug's
;;; decommission; the wrapper predates that move and may no longer be
;;; necessary now that the baseUrl is a plain domain behind Caddy's own
;;; cert -- left in place, unverified, since it's harmless if unneeded.
;;;
;;; EXTENSIONS (a list of packages, e.g. pi-mcp-adapter from peteches
;;; packages pi-coding-agent) symlinks each package's own
;;; lib/node_modules/<package-name> output to
;;; ~/.pi/agent/extensions/<package-name> -- pi's extension loader
;;; (dist/core/extensions/loader.js:discoverAndLoadExtensions) walks
;;; <agent-dir>/extensions/ for exactly this shape: a subdirectory with a
;;; package.json declaring a "pi.extensions" field. Declarative equivalent
;;; of `pi install npm:<name>', without a runtime npm/network step.
;;;
;;; EXTRA-EXTENSION-FILES (a list of (NAME . FILE) pairs) places FILE at
;;; ~/.pi/agent/extensions/NAME for single-file extensions -- pi's loader
;;; discovers loose .ts files in extensions/ as well as package
;;; directories (a typical install carries auto-continue.ts). This is how
;;; the repo ships herdr's bundled pi integration (herdr-agent-state.ts,
;;; the file `herdr integration install pi' would otherwise write by
;;; hand): committed under configs/pi/ so the integration is declarative
;;; and survives a fresh machine. The content is versioned with the herdr
;;; binary (HERDR_INTEGRATION_VERSION in its header; `herdr integration
;;; status' checks it), so re-copy it from a running install when herdr
;;; updates the integration.
;;;
;;; MCP-SERVERS reuses <home-claude-mcp-server> from (peteches home modules
;;; claude) rather than a parallel record type -- pi-mcp-adapter's
;;; mcp.json has the same {name: {command, args, env}} shape Claude Code's
;;; mcp-servers already carry, so the exact list built for an account's
;;; Claude config (anvil bridges, graphify, comfyui, …) is passed straight
;;; through here too; only NAME/COMMAND/ARGS/ENV are read for each entry
;;; actually rendered -- TRANSPORT is also read, but only to filter: an
;;; http-transport server (e.g. ygo's hosted Linear/Notion/Granola/Better
;;; Stack servers, see claude-workstation-ygo.scm) has no COMMAND to
;;; invoke, so it's dropped from mcp.json rather than rendered
;;; (pi-mcp-adapter has no http-transport support wired up here).
;;; URL/OAUTH-SCOPES/SCOPE are unused. Written
;;; to ~/.pi/agent/mcp.json, the adapter's own global-override file (see
;;; its README's file-layout precedence table) -- a fully static file,
;;; unlike claude.scm's activation-time `claude mcp add', because
;;; pi-mcp-adapter reads mcp.json directly at startup with no analogous
;;; runtime-mutated state to protect.

(define-module (peteches home modules pi)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages web) #:select (jq))
  #:use-module ((peteches home modules claude)
                #:select (home-claude-mcp-server-name
                          home-claude-mcp-server-command
                          home-claude-mcp-server-args
                          home-claude-mcp-server-env
                          home-claude-mcp-server-transport))
  #:export (home-pi-service-type
            home-pi-configuration))

(define-record-type* <home-pi-configuration>
  home-pi-configuration make-home-pi-configuration
  home-pi-configuration?
  (config-directory home-pi-configuration-config-directory
                    (default #f))
  (extensions       home-pi-configuration-extensions
                    (default '()))
  (extra-extension-files home-pi-configuration-extra-extension-files
                         (default '()))
  (mcp-servers      home-pi-configuration-mcp-servers
                    (default '())))

(define (directory-children directory)
  "Return the non-special immediate children of DIRECTORY."
  (filter (lambda (e) (not (member e '("." ".."))))
          (scandir directory)))

(define (home-pi-entry dir entry)
  (let ((src (string-append dir "/" entry)))
    (list (string-append ".pi/agent/" entry)
          (local-file src #:recursive? (file-is-directory? src)))))

(define (home-pi-extension-entry pkg)
  "Symlink PKG's global npm-install output (lib/node_modules/<name>) to
~/.pi/agent/extensions/<name>, where pi's extension loader discovers it."
  (list (string-append ".pi/agent/extensions/" (package-name pkg))
        (file-append pkg "/lib/node_modules/" (package-name pkg))))

;; Render one (NAME . FILE) pair from EXTRA-EXTENSION-FILES as a home-files
;; entry placing FILE at ~/.pi/agent/extensions/NAME. pi's extension loader
;; discovers loose files as well as package directories (a typical install
;; carries auto-continue.ts), so a single-file extension such as herdr's
;; bundled herdr-agent-state.ts (`herdr integration install pi') can be
;; committed to the repo and placed here without the runtime step.
(define (home-pi-extra-extension-entry pair)
  (list (string-append ".pi/agent/extensions/" (car pair))
        (cdr pair)))

;; Interleave SEP between the elements of LST -- used below to join JSON
;; fragment-lists with "," without a trailing/leading comma.
(define (intersperse sep lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list (car lst)))
        (else (cons (car lst) (cons sep (intersperse sep (cdr lst)))))))

;; Render one <home-claude-mcp-server> as a {"name":{"command":…,"args":
;; […],"env":{…}}} JSON fragment -- a list of strings/file-like objects
;; for MIXED-TEXT-FILE, since COMMAND is itself typically a file-like
;; object (e.g. (file-append bash "/bin/bash")) whose real store path is
;; only known once built, not a literal string this module could splice
;; by hand. None of the servers built by (peteches home modules claude-
;; workstation) use characters needing JSON escaping (paths, flag names).
(define (json-string-list strs)
  (apply append (intersperse (list ",") (map (lambda (s) (list "\"" s "\"")) strs))))

(define (home-pi-mcp-json-entry server)
  (let ((name (home-claude-mcp-server-name server))
        (cmd  (home-claude-mcp-server-command server))
        (args (home-claude-mcp-server-args server))
        (env  (home-claude-mcp-server-env server)))
    (append
     (list "\"" name "\":{\"command\":\"") (list cmd) (list "\"")
     (list ",\"args\":[") (json-string-list args) (list "]")
     (if (null? env)
         '()
         (append
          (list ",\"env\":{")
          (apply append
                 (intersperse (list ",")
                               (map (lambda (pair)
                                      (list "\"" (car pair) "\":\"" (cdr pair) "\""))
                                    env)))
          (list "}")))
     (list "}"))))

;; SERVERS is shared verbatim with this account's Claude Code config (see
;; MCP-SERVERS' docstring above), so it can include http-transport entries
;; (e.g. ygo's Linear/Notion/Granola/Better Stack servers -- see
;; claude-workstation-ygo.scm) whose COMMAND is #f. home-pi-mcp-json-entry
;; only knows how to render a stdio COMMAND/ARGS invocation, so those are
;; dropped here rather than crashing mixed-text-file on a #f command.
(define (home-pi-mcp-json servers)
  (let ((stdio-servers
         (filter (lambda (s)
                   (string=? (home-claude-mcp-server-transport s) "stdio"))
                 servers)))
    (apply mixed-text-file "mcp.json"
           (append
            (list "{\"mcpServers\":{")
            (apply append
                   (intersperse (list ",") (map home-pi-mcp-json-entry stdio-servers)))
            (list "}}")))))

;; settings.json is excluded here -- it is runtime-mutated by pi itself, so
;; it is jq-merged at activation (HOME-PI-SETTINGS-ACTIVATION below)
;; instead of symlinked. See the module docstring.
(define (home-pi-files-service config)
  (let ((dir        (home-pi-configuration-config-directory config))
        (extensions (home-pi-configuration-extensions config))
        (extra      (home-pi-configuration-extra-extension-files config))
        (servers    (home-pi-configuration-mcp-servers config)))
    (append
     (if dir
         (map (lambda (entry) (home-pi-entry dir entry))
              (remove (lambda (entry) (string=? entry "settings.json"))
                      (directory-children dir)))
         '())
     (map home-pi-extension-entry extensions)
     (map home-pi-extra-extension-entry extra)
     (if (null? servers)
         '()
         (list (list ".pi/agent/mcp.json" (home-pi-mcp-json servers)))))))

(define %pi-koboldcpp-bashrc "\
# Predates the move to koboldcpp.ts.peteches.co.uk (Caddy proxy, own cert) --
# originally worked around nug's cert not covering the Tailscale IPv6
# literal models.json pointed pi at. Wraps invocations rather than
# disabling TLS verification for the whole shell.
pi-koboldcpp() {
  NODE_TLS_REJECT_UNAUTHORIZED=0 command pi --provider koboldcpp \"$@\"
}
")

(define-public home-pi-koboldcpp-bashrc
  (plain-file "pi-koboldcpp.bash" %pi-koboldcpp-bashrc))

;; Positional-arg bash script merging DEFAULTS into ~/.pi/agent/settings.json
;; -- $1 is the defaults file, $2 the jq binary. Declared defaults win on any
;; overlapping key (jq's `*' operator merges right-biased, recursively for
;; nested objects), which is what lets a fleet-wide default like
;; httpIdleTimeoutMs stay enforced across reconfigures without clobbering
;; pi's own runtime keys (defaultProvider, defaultModel, …) that aren't
;; present in the defaults file. Creates the file (a plain copy, not
;; a symlink, so pi can keep rewriting it afterwards) if it doesn't exist
;; yet, e.g. on a brand new account.
;;
;; The defaults file does now declare one branch of pi's runtime state,
;; compaction.modelOverrides (2026-10-30), so those two per-model
;; reserveTokens are fleet-enforced: an account-local override of the same
;; key is rewritten here at activation. Sibling keys pi may write —
;; compaction.enabled, or an override for a model not listed here — are
;; different leaf paths and survive, since `*' merges nested objects
;; recursively and only clobbers keys that actually collide.
(define %home-pi-settings-merge-script "\
set -eu
defaults=\"$1\"; jq_bin=\"$2\"
# PATH is passed in as $3 -- the coreutils bin directory, spliced by
# HOME-PI-SETTINGS-ACTIVATION.  This script runs `dirname', `mkdir' and
# `mv' by name, so it needs a PATH: when Guix Home's activation is started
# by guix-home-service-type (the shepherd one-shot that every `guix deploy'
# or system reconfigure triggers), the environment is exactly what that
# service passes -- HOME plus GUIX_SYSTEM_IS_RUNNING_HOME_ACTIVATE and
# nothing else, so PATH is absent and bash falls back to its compiled-in
# default (/usr/local/bin:/bin:/usr/bin), which on a Guix system holds no
# coreutils.  Without this line the merge dies in `mkdir -p \"$(dirname …)\"'
# / `mv', and because HOME-PI-SETTINGS-ACTIVATION used to ignore `system*'
# exit statuses, nothing said so: observed on agents 2026-11-20, where two
# activations (Nov 18, Nov 20) had run =/activate= -- update-symlinks placed
# every symlink, ~/.pi/agent/models.json included -- while this one step
# silently skipped, leaving pi's runtime-written defaultProvider/defaultModel
# in place instead of the fleet defaults.
export PATH=\"$3\"
target=\"$HOME/.pi/agent/settings.json\"
mkdir -p \"$(dirname \"$target\")\"
# Both branches go through a `> tmp && mv` redirection rather than `cp`, so
# the result always gets a fresh mode from the account's umask instead of
# inheriting the store defaults file's read-only 444 -- a plain `cp` here
# left pi unable to write its own runtime state back to this file on any
# account without a pre-existing settings.json.
[ -f \"$target\" ] || echo '{}' > \"$target\"
tmp=\"$target.merge.tmp\"
\"$jq_bin\" -s '.[0] * .[1]' \"$target\" \"$defaults\" > \"$tmp\" && mv \"$tmp\" \"$target\"
")

(define (home-pi-settings-activation config)
  (let ((dir (home-pi-configuration-config-directory config)))
    (if (and dir (file-exists? (string-append dir "/settings.json")))
        (let ((defaults       (local-file (string-append dir "/settings.json")))
              (bash-bin       (file-append bash "/bin/bash"))
              (jq-bin         (file-append jq "/bin/jq"))
              (coreutils-bin  (file-append coreutils "/bin")))
          ;; The exit status is checked on purpose: `system*' returns it rather
          ;; than raising, which is what made the PATH failure above invisible.
          ;; A failed merge now aborts home activation instead of drifting in
          ;; silence -- see %HOME-PI-SETTINGS-MERGE-SCRIPT's PATH note.
          #~(let ((status (system* #$bash-bin "-c"
                                   #$%home-pi-settings-merge-script
                                   "home-pi-settings-merge"
                                   #$defaults #$jq-bin #$coreutils-bin)))
              (unless (zero? status)
                (error "home-pi: settings.json merge failed" status))))
        #~(begin))))

(define-public home-pi-service-type
  (service-type
   (name 'home-pi)
   (description "Manage the pi coding-agent CLI's ~/.pi config: models.json,
MCP-adapter extensions, and mcp.json.")
   (extensions
    (list (service-extension home-files-service-type
                             home-pi-files-service)
          (service-extension home-activation-service-type
                             home-pi-settings-activation)))
   (default-value (home-pi-configuration))))
