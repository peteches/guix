;; peteches/services/ovms.scm — Intel OpenVINO Model Server (OVMS) as a shepherd
;; service.
;;
;; WHAT THIS IS.  Runs one OpenVINO IR model behind OVMS's own OpenAI-compatible
;; HTTP endpoint (`/v3/chat/completions`), driven by the prebuilt vendor binary
;; packaged in (peteches packages ovms).  It is model-agnostic — nothing in the
;; service type knows or cares which family the IR belongs to — and the fleet
;; currently serves **Qwen3.6-35B-A3B INT4** through it on comfyui's Arc Pro
;; B70.  Shape: record type, account folding, log-dir activation, forkexec
;; shepherd service, source-narrowed firewall rules, five service extensions.
;;
;; WHAT IT REPLACED, AND WHY.  (History: the shim is DELETED as of 2026-11-26,
;; not merely superseded, so this can no longer be checked against a live
;; file.)  peteches/services/openvino-server.py was a 1095-line hand-written
;; OpenAI shim over the openvino-genai Python bindings, of which ~512 lines
;; existed purely to work around four upstream defects —
;;   * ChatHistory mangling tool conversations (the shim keeps its own transcript
;;     instead of handing the request's messages to the pipeline);
;;   * Llama3PythonicToolParser silently returning only the LAST of several tool
;;     calls in one turn;
;;   * the always-open `<think>` block, which the shim has to close by hand;
;;   * binding drift between openvino-genai releases.
;; OVMS ships native, *streaming* tool and reasoning parsers for a dozen model
;; families — `lfm2` for the LFM markup the shim hand-implemented, and
;; `qwen3coder` + `qwen3` for the family it serves now — in C++, with tests.
;; Streaming was the whole argument; see the next two sections.
;;
;; DELETED ALONGSIDE IT: peteches/services/openvino.scm
;; (`openvino-service-type`) and peteches/services/openvino-test.py (the
;; out-of-band inference probe for the shim).  **peteches/packages/openvino.scm
;; was deliberately KEPT** even though nothing imports it any more: it is this
;; repo's only from-source Guix packaging of OpenVINO + openvino-genai (with
;; every FetchContent dependency vendored), it is not LFM2-specific, and it is
;; the fallback if OVMS's bundled runtime ever becomes a problem.
;;
;; MEASURED, ON LFM2.5: THE SHIM'S BLOCK ORDER WAS WRONG, AND OVMS'S IS RIGHT.
;; (Kept as the reasoning that got OVMS adopted; the model is retired but the
;; mechanism is not — pi still processes `delta.content` before the reasoning
;; fields for any provider.)  The shim could not stream a partial tool-call
;; block (it could not tell markup from prose until the block was closed), so it
;; buffered every tool-bearing turn and emitted it as a single SSE delta
;; carrying both `content` and `reasoning_content`.  That was 662 of the 920
;; generations logged from the live instance.  pi therefore opened a text block
;; first and rendered **thinking after the answer** — visible, persistent
;; corruption of every tool-using turn, not a cosmetic ordering nit.  OVMS
;; streamed tool turns properly: measured block order `R T F=tool_calls` with 4
;; incremental tool-call deltas, and `R C F=stop` for prose turns — reasoning
;; strictly before content in both cases.  Qwen3.6 on OVMS measures the same
;; ordering (see "QWEN3.6-35B-A3B, MEASURED" below).
;;
;; MEASURED, ON LFM2.5: TWO TOOL CALLS, REAL IDS.  In a two-call turn (`bash ls`
;; and `bash wc -l README.md`) OVMS parsed **both**, where GenAI's own
;; Llama3PythonicToolParser returns only the last — and it emitted real
;; tool-call ids rather than the shim's positional `call_0`/`call_1`.
;;
;; `pipeline_type`: AN EARLIER REVISION OF THIS HEADER GOT THE REASONING WRONG.
;; It asserted that because `--pipeline_type AUTO` logs `Initializing Language
;; Model Continuous Batching servable`, Intel KB 000103073 ("LFM2 does not
;; currently support Continuous Batching", reviewed 2026-10-14, tells you to
;; force `--pipeline_type LM`) must be "out of date", and comfyui.scm said "Do
;; NOT fix this to LM".  **That inference concluded correctness from a
;; successful startup, which is not evidence of correct output.**  What was
;; subsequently measured on the retired blaj LFM2.5-24B-A2B INT8 graph:
;;
;;   * AUTO → Continuous Batching: severe repetition degeneration on multi-turn
;;     agent work.  8-gram repeat count 47 (best of four sampling configs) and
;;     407 on OVMS 2026.5.0 weekly; largest thinking block 40 121 and 89 199
;;     characters; 1/3 and 0/3 correct.
;;   * `--pipeline_type LM` → the stateful/Legacy servable: CLEAN output
;;     (8-gram repeat 3, largest thinking block 3 664 chars) — and unusable.
;;     It re-prefills the whole conversation every turn (no prefix caching),
;;     per-turn latency escalated 7.8 → 15.2 → 12.4 → 18.8 → 42.8 s, and then
;;     it **hung indefinitely** (30 min timeout).  The wedged process survived
;;     SIGTERM (`STAT=Ssl`, `wchan=futex_do_wait`) and held the GPU as a DRM
;;     client until SIGKILL — which matters operationally because shepherd's
;;     `make-kill-destructor`, used below, sends SIGTERM first.
;;
;; So for LFM2.5 both pipeline types were wrong in different ways, and that is
;; part of why the model was retired rather than re-flagged.  `pipeline-type`
;; still defaults to `"AUTO"`, and that default is now justified by measurement
;; on the model actually served: for the official Qwen3.6-35B-A3B INT4 IR,
;; AUTO → Continuous Batching is verified good end-to-end (3/3 correct on the
;; agentic benchmark, 8-gram repeat 2, 6 clean tool calls, 62 s wall).
;;
;; Two doc-reliability notes worth keeping.  (1) A servable reaching
;; `AVAILABLE` and answering `/v1/models` says nothing about output quality —
;; that is exactly how the original AUTO-is-fine claim got made.  (2) The
;; Legacy/stateful servable DID return streaming `usage` and
;; `finish_reason=length` in that test, even though OVMS documentation says
;; stateful servables cannot, so that document is unreliable in the other
;; direction too.  The earlier claim here — that forcing LM would "silently
;; cost us streaming `usage` and `finish_reason=length`" — was never tested and
;; is recorded as false.
;;
;; DEFAULT PORT 8094; THE PRODUCTION INSTANCE USES 8093.  8093 is the port
;; peteches/systems/caddy.scm proxies as llm.ts.peteches.co.uk, and it was the
;; retired openvino-server.py shim's port before that.  The default differs so a
;; trial instance cannot collide with the live endpoint — that is how this
;; service was first validated here (8B int4 on CPU, then on GPU, shim still
;; serving).
;;
;; The production instance in peteches/systems/comfyui.scm sets `(port 8093)`
;; explicitly.  Keeping the port across the shim → OVMS → Qwen3.6 moves is what
;; let the caddy vhost stay untouched; the client-visible thing that DOES have
;; to move with the model is the `model-name`, because
;; configs/pi/defaults/models.json pins it (see that field).
;;
;; UNAUTHENTICATED BY DEFAULT.  OVMS reads `--api_key_file` (or the `API_KEY`
;; environment variable, reachable through extra-environment-variables); with
;; neither set it logs `API key not provided … Authentication will be disabled.`
;; and serves anyone who can reach the socket.  Hence: bind address defaults to
;; 127.0.0.1, `open-firewall?` defaults to #f, and when it is turned on
;; `firewall-sources` should narrow the accept to the tailnet — the same
;; reasoning as strata-firewall-rules and the deleted openvino-firewall-rules.
;; Since that combination used to be
;; expressible by accident, ovms-firewall-rules now *raises* at
;; configuration-evaluation time on open-firewall? #t + api-key-file #f + empty
;; firewall-sources rather than emitting a bare `tcp dport 8094 accept` that is
;; reachable from the whole 192.168.50.0/23 LAN.
;;
;; NOT IN THE SYSTEM PROFILE BY DEFAULT.  `add-to-profile?' is #f: ovms bundles
;; an Ubuntu-built oneTBB **12.13** at lib/libtbb.so.12, and while
;; `(peteches services openvino)` was still wired on comfyui Guix's `onetbb`
;; **12.19** was in the system profile too (propagated by
;; peteches/packages/openvino-genai, which that service's profile extension
;; merged) — a silent file-level collision.  Guix cannot warn about that; see
;; ovms-packages for the union-build void-port detail and for what is and is not
;; still true about that collision now the openvino service is gone.  The
;; service execs $out/bin/ovms by absolute store path and does not need a
;; profile entry, which is why the default stays #f either way.
;;
;; FLAGS THIS SERVICE NEVER PASSES.  `--configure` writes a `graph.pbtxt` *into
;; the model directory*; serve mode does not (it logs `Graph config created in
;; memory from model_path: …`), so the model dir only has to be readable by the
;; service user and is never mutated.  `--pull` / `--source_model` would download
;; from Hugging Face at boot — exactly the crash-loop failure mode the deleted
;; peteches/services/openvino.scm warned about.  `--add_to_config`,
;; `--remove_from_config` and `--list_models` are one-shot CLI modes, not serve
;; modes.  None of them are exposed as fields, so they cannot be reached by
;; accident; `--log_path` is likewise not exposed, because shepherd's
;; `#:log-file` already captures stdout/stderr and pointing OVMS at its own file
;; would split one log across two places.  `--max_num_batched_tokens` and
;; `--model_version_policy` are genuine serve-mode flags that are simply not
;; worth a field each yet — pass them through `extra-args`.
;;
;; RUNTIME ENVIRONMENT.  A bare make-forkexec-constructor shepherd process gets
;; *only* the variables listed in ovms-environment below, so three are mandatory:
;;   * PYTHONPATH=$out/lib/python:/run/current-system/profile/lib/python3.12/site-packages
;;     — TWO entries, and the second one is load-bearing.  The bundled CPython
;;     packages (openvino, openvino_genai, openvino_tokenizers, pyovms.so) live
;;     in $out/lib/python, which is not a site-packages path any profile merge
;;     looks at; the same arrangement the deleted openvino.scm documented for
;;     OpenVINO's own bindings in <out>/python.
;;
;;     python_on is required, not optional: the C++ minja chat-template engine
;;     does not implement every template, and OVMS's docs state that `tools` is
;;     unsupported without Python.  peteches/packages/ovms.scm's header records
;;     the upstream list of minja exceptions as exactly three: `lfm2`,
;;     miniCPMv5 and Qwen3-Coder.  `qwen3coder` is the tool parser OVMS
;;     auto-detects for the Qwen3.6 model this fleet serves, so do NOT assume
;;     the jinja2/python_on requirement died with LFM2 — it has not been
;;     re-measured for Qwen3.6 with jinja2 absent, and the failure it produces
;;     is silent (below).
;;
;;     BUT OVMS DOES NOT BUNDLE jinja2 OR numpy — lib/python holds only
;;     openvino, openvino_genai, openvino_tokenizers and pyovms.so, and its own
;;     requirements.txt lists numpy and openvino-telemetry as things the CALLER
;;     must supply, not jinja2.  Both are therefore declared by ovms-packages
;;     rather than left to whatever else happens to be in the host profile:
;;     numpy does resolve on comfyui today, but only because an unrelated
;;     package installs it, which is precisely the accident that broke jinja2.
;;     Without
;;     the profile's site-packages on PYTHONPATH the embedded interpreter cannot
;;     import jinja2, OVMS falls back to minja, and minja cannot render the
;;     template.  MEASURED (on the retired LFM2.5 export), and it is a silent
;;     failure — the servable
;;     still reaches AVAILABLE and /v1/models still answers; only these lines
;;     give it away, at startup:
;;
;;       Warning: Chat template has not been loaded properly. Servable will not
;;                respond to /chat/completions endpoint.
;;       Dry-run probe: template silently failed to render tool calls
;;       Minja cannot render this template's tool calls correctly
;;
;;     and then every request 400s with `Chat template not loaded correctly, so
;;     it cannot be applied'.  With the profile site-packages added, the log
;;     instead shows `[INFO] Reading chat template from chat_template.jinja file
;;     in model directory.' and requests work (verified under `env -i`, i.e. with
;;     no inherited environment at all: finish=stop with reasoning_content and
;;     usage, and a tool turn returning finish=tool_calls with BOTH calls
;;     parsed).
;;
;;     WHY THIS WAS MISSED: an interactive SSH shell exports GUIX_PYTHONPATH
;;     (the profile's python search-path spec) pointing at exactly that
;;     site-packages directory, so every hand-run test imported jinja2 fine and
;;     looked healthy.  Shepherd passes only the variables listed here, so
;;     GUIX_PYTHONPATH is absent.  This is the same trap as LD_LIBRARY_PATH
;;     below, one variable over: validate the service environment with `env -i`,
;;     never from a login shell.  python-jinja2 and python-numpy are therefore
;;     both added to the system profile by ovms-packages, so the dependency is
;;     declared here rather than incidentally satisfied by whatever else happens
;;     to pull them into the host's profile.
;;   * OCL_ICD_VENDORS — needed for the GPU path, because oneDNN's Level Zero
;;     backend enumerates devices through OpenCL.  It must be a **directory** of
;;     `.icd` files containing *only* the Intel descriptor; this service
;;     synthesises that directory itself in activation rather than pointing at
;;     the profile's vendors merge.  See "A DIRECTORY OF ONE, NOT A FILE, AND
;;     NOT THE WHOLE PROFILE MERGE" in ovms-environment for the two measurements
;;     that force both halves of that sentence.
;;   * LD_LIBRARY_PATH=/run/current-system/profile/lib[:lib64] — mandatory for
;;     the *Intel* GPU path even though the package is fully patchelf'd.  See
;;     "RUNPATH COVERS OVMS, NOT THE HOST'S ICD CHAIN" below.
;;
;; The Intel ICD descriptor, and the libraries behind it, are host properties
;; rather than properties of one model instance, so this service deliberately
;; does not install any of them.  They must be present in the **system profile**
;; via `#:extra-packages` — peteches/systems/comfyui.scm lines ~207-208 and
;; ~230 install the full set: `intel-opencl-icd`, `intel-igc-opencl`,
;; `intel-igc-core`, `intel-level-zero-gpu`, `libigdgmm`, `opencl-icd-loader`,
;; `(list zstd "lib")` and `(list gcc "lib")`.  The four that matter here are
;; `intel-opencl-icd` (libigdrcl.so + the intel.icd descriptor),
;; `intel-igc-opencl` (libigdfcl.so.2), **`libigdgmm`** (libigdgmm.so.12) and
;; **`zstd:lib`** (libzstd.so.1).  The last two are the ones everybody forgets,
;; because neither is an OpenCL package by name — they are *transitive dlopen*
;; dependencies of the Intel ICD that are reachable from nowhere else in ovms's
;; closure.  Omitting either produces zero Intel GPUs with no error message.
;;
;; RUNPATH COVERS OVMS, NOT THE HOST'S ICD CHAIN.  An earlier revision of this
;; header claimed no LD_LIBRARY_PATH was needed, on the grounds that the package
;; patchelf's the interpreter and the RUNPATH of bin/ovms *and* of every bundled
;; ELF object.  **That claim is true and irrelevant.**  It is true of ovms's own
;; closure: all 51 shared objects in bin/ovms's DT_NEEDED/RUNPATH closure
;; resolve on their own — only ~21 of the 51 resolve inside $out/lib; the rest
;; come from the glibc/gcc/python/curl/openssl/util-linux/zlib/libxml2 store
;; paths on that RUNPATH (see the long DT_RUNPATH note in
;; peteches/packages/ovms.scm).  It says nothing about the
;; host's OpenCL ICD chain, which is dlopened *by absolute store path* and is
;; not in ovms's closure at all:
;;
;;   libopenvino_intel_gpu_plugin.so  (bundled, patchelf'd, fine)
;;     └─ dlopens the OpenCL ICD loader
;;          └─ reads intel.icd, which names an ABSOLUTE store path:
;;             …-intel-opencl-icd-26.35.39758.10/lib/libigdrcl.so
;;               ├─ no DT_RPATH, no DT_RUNPATH  ← nothing we patch helps here
;;               └─ DT_NEEDED libigdgmm.so.12   ← libigdgmm package, profile-only
;;                  (and NEO's libigdfcl.so.2 → libigc.so.2 → libzstd.so.1,
;;                   also profile-only)
;;
;; glibc's search order for a dlopened object's own dependency is DT_RPATH
;; (none) → LD_LIBRARY_PATH (unset) → DT_RUNPATH (none) → /etc/ld.so.cache
;; (Guix generates no cache) → /lib, /usr/lib (absent).  So libigdgmm.so.12 is
;; not found, the Intel ICD fails to load **silently**, and no Intel GPU
;; enumerates.  LD_LIBRARY_PATH is the only hook in that chain we control.
;;
;; MEASURED on comfyui (i9-14900KS, Intel Arc Pro B70 + NVIDIA RTX 4090,
;; OVMS 2026.4.1), using OVMS's own bundled `openvino` python to enumerate:
;;
;;   A) OCL_ICD_VENDORS=<profile>/etc/OpenCL/vendors/intel.icd , no  LDP -> ['CPU']
;;   B) OCL_ICD_VENDORS=<profile>/etc/OpenCL/vendors/intel.icd , WITH LDP -> ['CPU']
;;   C) OCL_ICD_VENDORS=<profile>/etc/OpenCL/vendors (whole dir), WITH LDP
;;                                                            -> ['CPU','GPU.0','GPU.1']
;;        GPU.0 = Intel Arc Pro B70 (30.3 GiB), GPU.1 = NVIDIA RTX 4090 (23.5 GiB)
;;      same whole dir, WITHOUT LD_LIBRARY_PATH -> GPU = NVIDIA RTX 4090 only
;;   D) OCL_ICD_VENDORS=<a directory containing only a SYMLINK to intel.icd>,
;;      WITH LDP -> devices: ['CPU','GPU']
;;        GPU = Intel(R) Arc(TM) Pro B70 Graphics (dGPU), 30.3 GiB
;;
;; (LDP = LD_LIBRARY_PATH=/run/current-system/profile/lib:/run/…/lib64.)  Rows A
;; and B prove "OCL_ICD_VENDORS must be a directory, not a file"; the C pair
;; proves "LD_LIBRARY_PATH is mandatory" — note that without it the Intel card
;; vanishes and the *NVIDIA* ICD, which is self-contained and loads fine, is
;; the only GPU left — and C also proves "must be narrowed, not the whole
;; merge".  That is the dangerous failure, because it does not look like
;; a failure: OVMS reports `GPU FULL_DEVICE_NAME = NVIDIA GeForce RTX 4090
;; (dGPU)` with 23.5 GiB, loads the model onto a card that is already full of
;; ComfyUI, and dies in ~3.3 s with
;;   Exception from src/plugins/intel_gpu/src/runtime/ocl/ocl_memory.cpp:74:
;;   [GPU] clEnqueueMapBuffer, error code: -4 CL_MEM_OBJECT_ALLOCATION_FAILURE
;;   → LOADING_PRECONDITION_FAILED → ovms::ModelManager::Start() Error.
;; Row D is the only one proving that the symlink-in-a-synthesised-directory
;; form enumerates the card — which is exactly what ovms-instance-activation
;; builds; A/B/C alone do not validate the implemented design.
;;
;; WHY THIS WAS MISSED INITIALLY, AND HOW TO REPRODUCE IT.  The first GPU
;; attempt did fail — with CL_MEM_OBJECT_ALLOCATION_FAILURE — and was initially
;; misattributed; a later interactive probe that enumerated the Intel GPU
;; successfully did so because *that probe exported LD_LIBRARY_PATH explicitly
;; on the command line*, not because the login shell supplied it.  Guix login
;; shells do NOT carry LD_LIBRARY_PATH:
;; /run/current-system/profile/etc/profile and
;; ~/.guix-home/profile/etc/profile export PATH, MANPATH, INFOPATH,
;; GUILE_LOAD_PATH, GIT_EXEC_PATH, SSL_CERT_DIR etc. and no *LIBRARY_PATH at
;; all, and `guix shell --search-paths glibc` exports none — Guix deliberately
;; keeps library paths out of the login environment.  So a hand-test from a
;; login shell *would* have reproduced the failure, as long as the test
;; command did not itself export LD_LIBRARY_PATH.  The derived rule stands,
;; with the true mechanism attached: shepherd execs with exactly the variables
;; ovms-environment lists, so any future GPU regression check must verify the
;; GPU environment under `env -i` or via the lowered shepherd gexp (or as
;; `sudo herd status`/`herd start` against the real service) — never by
;; assuming what a shell's environment contains.
;;
;; AVX2 IS REQUIRED AT RUNTIME.  The bundled oneDNN CPU plugin has an AVX2
;; floor; on a CPU without it the process SIGILLs (exit 132).  comfyui
;; (i9-14900KS) has AVX2 and no AVX-512, which is fine.
;;
;; QWEN3.6-35B-A3B, MEASURED — 2026-11-26, comfyui (i9-14900KS, Intel Arc Pro
;; B70 with 31.9 GiB usable, plus an RTX 4090 reserved for ComfyUI and never
;; used for LLM inference), OVMS 2026.4.1, `--pipeline_type AUTO`.  This is the
;; model the production instance serves; everything below was measured against
;; the real service on the real card, not a CPU fallback.
;;
;;   * Source: HuggingFace `OpenVINO/Qwen3.6-35B-A3B-int4-ov`, an official Intel
;;     export created 2026-05-20, 19.43 GB total (`openvino_language_model.bin`
;;     17.99 GB, plus `openvino_mtp_model.bin` 0.44 GB,
;;     `openvino_text_embeddings_model.bin` 0.51 GB and
;;     `openvino_vision_embeddings_merger_model.bin` 0.44 GB).  Intel publishes
;;     **only INT4** for this model — there is no int8 or fp16 export to weigh,
;;     so unlike LFM2.5 there is no precision trade-off to re-litigate.  Local
;;     path on comfyui: /media/models/openvino/Qwen3.6-35B-A3B-int4-ov.
;;   * config.json: `model_type: qwen3_5_moe`, `max_position_embeddings: 262144`.
;;   * CONTEXT CEILING VERIFIED EMPIRICALLY, not read from config: 224 017
;;     prompt tokens → OK; 280 017 → `HTTP 400 … Number of prompt tokens: 280017
;;     exceeds model max length: 262144`.  So the usable window is 262144 and
;;     OVMS enforces it.  That is the number configs/pi/defaults/models.json
;;     declares and peteches/home/modules/pi.scm sizes against.
;;   * Load ~35 s to AVAILABLE; with AUTO the servable is
;;     `Initializing Language Model Continuous Batching servable`.
;;   * Parsers are AUTO-DETECTED from the chat template: the startup log reads
;;     `Auto-detected tool_parser: qwen3coder` and `Auto-detected
;;     reasoning_parser: qwen3`.  No `Minja cannot render` failure — that
;;     failure mode was specific to LFM2, whose template the C++ minja engine
;;     cannot render.  This is why the instance pins neither parser field.
;;   * Decode **101.4–101.9 T/s**, measured over 1500-token completions and
;;     stable across `temperature` 1.0 / model-default / 0.
;;   * Prefix caching, cold vs warm on the SAME 44 817-token prompt: **30.84 s
;;     (1 453 T/s) cold, 3.78 s (11 850 T/s effective) warm — an 8.2×
;;     speedup.**  See the `enable-prefix-caching?' field.
;;   * Streaming: prose turns give block order `R C F=stop` (reasoning strictly
;;     before content); tool turns give `R T F=tool_calls` with incremental
;;     tool-call deltas; a deliberate `max_tokens=8` truncation returns
;;     `finish_reason: length`; streaming `usage` is present in the final chunk
;;     when `stream_options.include_usage` is set.
;;   * KV cache: 1.0 GB allocated dynamically, peaked at 67.6% during the
;;     agentic test.
;;   * Sampling: `generation_config.json` ships `temperature 1.0, top_k 20,
;;     top_p 0.95`.  pi sends no sampling parameters, so those defaults are what
;;     production runs on and what every number here was taken at.
;;   * END-TO-END AGENCY — the thing the LFM2.5 protocol round-trip below never
;;     demonstrated.  On the agentic benchmark recorded in
;;     peteches/systems/comfyui.scm this stack scored 3/3 correct in 1 m 2 s
;;     with 6 clean tool calls, an 8-gram repeat count of 2 and a largest
;;     thinking block of 2 972 characters, against a remote `qwen3.8-max`
;;     control at 3/3 in 1 m 9 s and the retired LFM2.5 at 1/3 in 7.5 m.
;;
;; MTP / SPECULATIVE DECODING: TESTED AND REJECTED.  Do not add
;; `--draft_model_path`.  The Qwen3.6 IR ships `openvino_mtp_model.xml/.bin` and
;; config.json carries `"mtp_num_hidden_layers": 1`, so
;; `--draft_model_path <same model dir> --draft_device GPU` does start — it logs
;; `Draft model strategy: MTP (Multi-Token Prediction)` and switches the servable
;; to `Initializing Visual Language Model Continuous Batching servable`, because
;; the IR also contains a vision embeddings merger.  Everything past that point
;; is a reason not to:
;;   * It is MUTUALLY EXCLUSIVE WITH PREFIX CACHING.  With both enabled the
;;     first request aborts the executor:
;;       Check '!m_scheduler->get_config().enable_prefix_caching' failed at
;;       continuous_batching/pipeline_impl.cpp:388: Linear-attention verifier
;;       speculative decoding requires enable_prefix_caching=false (not yet
;;       supported).
;;     and OVMS then **SEGFAULTS** rather than failing gracefully — dmesg:
;;       ovms[6824]: segfault at 55ec78d2662e ip 000055e8f4e31173 … in
;;       ovms[3c3173,55e8f4a6e000+21be000]
;;   * Measured with prefix caching OFF: decode 104–110 T/s against the
;;     101.4–101.9 T/s baseline, but the repeated 44 817-token prompt cost
;;     **31.92 s instead of 3.78 s** — no reuse at all.  Net: +5–10% decode in
;;     exchange for 8.4× worse prefill on every turn after the first.  Over a
;;     20-turn agent session that is roughly +560 s of prefill to save ~18 s of
;;     decode; prefix caching wins by ~30×.
;;   * It forces greedy decoding — the binary contains `WARNING: MTP greedy
;;     decoding enforced: setting do_sample to false and num_beams to 1.` and
;;     `Overriding num_assistant_tokens to default value of 5 for MTP decoding`
;;     — AND IT CHANGES GREEDY OUTPUT: baseline greedy produced 6 100 chars of
;;     reasoning and 1 077 of content, MTP greedy produced 6 627 and 50.
;;     Greedy speculative decoding should be token-exact, so that is a
;;     correctness concern independent of speed.
;;
;; THE `developer` ROLE: A CLIENT-SIDE FLAG THIS SERVICE'S CORRECTNESS DEPENDS
;; ON.  It is invisible from the server, so it is recorded here as well as in
;; peteches/home/modules/pi.scm.  pi computes the instruction role as
;;
;;   instructionRole = model.reasoning && compat.supportsDeveloperRole
;;                       ? "developer" : "system"
;;
;; (dist/bundle/chunks/openai-completions-*.js).  The `llm` provider in
;; configs/pi/defaults/models.json sets `"reasoning": true`, and Qwen3's chat
;; template validates message roles.  Measured against this endpoint:
;;
;;   role=system     -> OK
;;   role=developer  -> HTTP 400  "Unexpected message role."
;;   role=user       -> OK
;;
;; `"supportsDeveloperRole": false` in that provider is therefore MANDATORY:
;; without it every single pi request 400s in under a second.  LFM2.5's template
;; tolerated `developer`, which is why six hours of LFM2.5 testing never surfaced
;; this.  Every one of pi's built-in model definitions sets
;; `supportsDeveloperRole: !1` explicitly, so the trap only bites custom
;; providers declared in models.json.
;;
;; VERIFIED ON THE ARC PRO B70, LFM2.5 — HISTORY.  2026-11-26, comfyui, OVMS
;; 2026.4.1, `LFM2.5-24B-A2B-int8-ov`.  The model and the shim are both retired,
;; but this is what established the GPU device path the service still uses, and
;; the pipeline-type measurements above are what retired the model.
;;
;;   * 24B INT8 loaded on **GPU** in ~49 s, with
;;     `Initializing Language Model Continuous Batching servable` and
;;     `state changed to: AVAILABLE`.  No `ScatterNDUpdate` / `index_put_`
;;     conv-layer error — so the openvino.genai #4292 / #4997 bug class, which is
;;     export-shape-dependent and has hit *official* LFM2.5 exports on GPU, was
;;     not present in that export.  That is a per-export property: re-check the
;;     startup log for any new one.
;;   * Streaming + tools: block order `R T F=tool_calls` — reasoning strictly
;;     before content — with first token at 0.91 s, 4 incremental tool-call
;;     deltas, and streaming `usage` present.  That is exactly the ordering the
;;     deleted openvino-server.py shim could not produce (see "MEASURED, ON
;;     LFM2.5: THE SHIM'S BLOCK ORDER WAS WRONG" above).
;;   * Throughput **140.6 tok/s** end-to-end, against the shim's recorded
;;     **127 tok/s** single stream — i.e. the native server was faster *and*
;;     correct about block order.  (Both figures are LFM2.5-24B INT8; they say
;;     nothing about Qwen3.6, whose decode is the 101.4–101.9 T/s above.)
;;   * Deliberate `max_tokens=8` truncation returns `finish_reason: length`
;;     together with a populated `usage`, which is what
;;     peteches/home/modules/pi.scm's context accounting needs.
;;   * Multi-turn agent loop — PROTOCOL ROUND-TRIPPING ONLY: turn 1 returned a
;;     `read_file` tool call carrying a real (non-positional) id and valid JSON
;;     arguments; feeding back a `role: tool` result *synthesised by the test
;;     script* (not real command output) produced `finish_reason: stop` and a
;;     plausible answer, with `prompt_tokens` growing 149 → 291.  That growth
;;     is the actual evidence: it proves the tool result re-entered the prompt
;;     rather than being dropped, i.e. that the ChatML round-trip works.  It
;;     did NOT demonstrate end-to-end agency — and when real pi sessions were
;;     finally run against it, the model degenerated (see the pipeline-type
;;     section).  This was precisely the flow that had needed the shim's
;;     hand-written `_render_chatml` (77 lines, or 108 counting its two
;;     fallback helpers).
;;
;;   * CAVEAT, NOT A DEFECT: a 600-token prose request ended
;;     `finish_reason: length` with 2516 characters of reasoning and **zero**
;;     content.  That was LFM2.5's documented think-first behaviour
;;     (docs/openvino-think-budget.md, retained as history) spending the whole
;;     budget before it started answering.  OVMS reports it honestly as `length`
;;     rather than inventing an empty completion, so size `max_tokens` for the
;;     thinking block as well as the reply.  Whether Qwen3.6 has the same
;;     exposure was not measured directly; its largest observed thinking block
;;     on the agentic benchmark was 2 972 chars, an order of magnitude below
;;     LFM2.5's 40 121–89 199.
;;
;; `target-device` still defaults to AUTO, which on a box with a visible Arc card
;; means OVMS picks GPU — so a deliberate CPU-only trial must say "CPU"
;; explicitly rather than rely on the default.
;;
;; CURRENT SHAPE, as wired in peteches/systems/comfyui.scm.  THREE instances,
;; declared in ONE `(service ovms-service-type (list cfg1 cfg2 cfg3))' value so
;; ovms-accounts' dedup-by-user-name runs (separate service forms would each
;; contribute an `ovms' account, and Guix only warns).  All three are
;; `(host "0.0.0.0")`, `open-firewall? #t` with the three tailnet CIDRs
;; (`100.64.0.0/10`, `fd7a:115c:a1e0::/48`, `fd7a:115c:a6e0::/48`) — caddy
;; dials comfyui over the tailnet, not over loopback, so all three are
;; required — and all three are `auto-start? #t`:
;;
;;   * `ovms-qwen36`, port **8093**, generative (`text_generation`): serves
;;     Qwen3.6-35B-A3B-int4-ov, caddy-proxied as `llm.ts.peteches.co.uk`.
;;     Its `model-name' is pinned to the IR name because
;;     configs/pi/defaults/models.json depends on that exact string.
;;
;;   * `ovms-embed`, port **8094**, `--task embeddings': serves
;;     OpenVINO/Qwen3-Embedding-0.6B-int8-ov (dim 1024, 32768 context, LAST
;;     pooling, Apache-2.0), caddy-proxied as `embeddings.ts.peteches.co.uk`.
;;     Its client-visible `model-name' is the stable alias **"embeddings"**,
;;     deliberately NOT the IR name — embedding clients ask for a *role*, so
;;     the underlying checkpoint can be swapped without touching them;
;;     qwen36 pins the IR name only because pi's model id already is that
;;     string.  Has a KNOWN DEFECT: an input containing "" wedges it
;;     permanently and silently — see the block in comfyui.scm.
;;
;;   * `ovms-rerank`, port **8095**, `--task rerank': serves
;;     OpenVINO/bge-reranker-v2-m3-int8-ov
;;     (XLMRobertaForSequenceClassification, 8194 context, Apache-2.0) under
;;     the client-visible alias **"reranker"**.  Second stage of
;;     retrieve-then-rerank.  NO caddy route, deliberately: its only clients
;;     are fleet-internal bots, which reach
;;     comfyui.spaniel-cordylus.ts.net:8095 directly over the tailnet (already
;;     encrypted), and a new *.ts.peteches.co.uk vhost would add a
;;     deSEC-CNAME/ACME dependency for nothing.  Its API is COHERE-shaped
;;     (POST /v3/rerank), NOT OpenAI-shaped — see comfyui.scm.
;;
;; Measured VRAM on the Arc, all three resident: qwen36 alone 17.88 GiB; +668
;; MiB for embed; +633 MiB for rerank; 19.22 GiB of 31.9 GiB total, 12.7 GiB
;; free.  `intel_gpu_top' does NOT work on the xe driver; read
;;     /sys/kernel/debug/dri/0000:02:00.0/tile0/vram_mm instead.
;;
;; `service-name` drives the log path (/var/log/ovms/ovms-qwen36.log,
;; ovms-embed.log, ovms-rerank.log) and the state dir (/var/lib/ovms/<service-
;; name>), which is also where the synthesised opencl-vendors directory
;; lives.  comfyui's alloy log-files entry ships the glob
;; /var/log/ovms/*.log — covering all three — and that Loki stream is the only
;; fleet-visible signal for them: this service type exposes no metrics endpoint
;; (`GET :8093/metrics` returns HTTP 400) and prometheus.scm has no
;; HTTP-probe/blackbox job.  NOTE that "ships to Loki" means QUERYABLE, not
;; ALERTING: nothing in this fleet routes alerts — no alertmanager is deployed
;; anywhere, prometheus.scm's `rule-files' field is supported but unset, and
;; there is no Loki ruler.  A wedged ovms-embed is therefore something a human
;; has to go and look for, not something that pages anyone.
;;
;; **No FUNCTIONAL caddy change was part of the Qwen3.6 cutover.**
;; peteches/systems/caddy.scm keeps proxying `llm.ts.peteches.co.uk` →
;; `comfyui.spaniel-cordylus.ts.net:8093`.  That file IS edited in the same
;; commit, but only its comment block, which described the now-deleted shim;
;; the edit is comment-only and provably derivation-neutral — HEAD's caddy.scm
;; and the edited copy compute a byte-identical system.drv — so caddy is not
;; owed a deploy.  What DID have to move is the client
;; config: configs/pi/defaults/models.json pins the model id
;; `Qwen3.6-35B-A3B-int4-ov`, which must equal the instance's `model-name`
;; exactly (OVMS routes by `--model_name`; a mismatch 404s every pi request),
;; and configs/pi/defaults/settings.json keys both `defaultModel` and its
;; compaction override off the same string.  Note that ovms-firewall-rules
;; *refuses* to compile open-firewall? #t with no api-key-file and no
;; firewall-sources, so the CIDRs are not optional.
;;
;; THERE IS NO IN-CONFIG ROLLBACK ANY MORE.  The previous revision also declared
;; an `openvino-lfm2` instance at `auto-start? #f` as a one-command fallback
;; (`herd stop ovms-lfm2 && herd start openvino-lfm2`).  That instance, its
;; service type and its shim are deleted, so rolling back now means `git revert`
;; plus a reconfigure.  That is deliberate: LFM2.5 degenerated on multi-turn
;; agent work under two independent serving stacks, so falling back to it is not
;; a recovery path anyone should want.
;;
;; Ordering still matters when deploying, but NOT for the reason an earlier
;; revision of this comment claimed.  It asserted that shepherd applies
;; auto-start? to new services without stopping a running one, so a rename
;; would start the new instance into a port and VRAM conflict.  That is false,
;; verified against the pinned guix (c947335) and shepherd 1.0.9:
;; `upgrade-shepherd-services' computes `to-unload' from shepherd-service-upgrade
;; and the generated program runs (for-each unload-service to-unload) BEFORE
;; (for-each start-service to-start); shepherd's root `unload' action stops a
;; service before deregistering it; and make-kill-destructor SIGTERMs then
;; SIGKILLs after default-process-termination-grace-period (5 s), which also
;; clears a wedged process.  So renaming `ovms-lfm2' -> `ovms-qwen36' does
;; unload the old service first.  Two real hazards remain:
;;   * A HAND-LAUNCHED ovms is not a shepherd service, so nothing in the
;;     reconfigure can see or stop it.  It keeps 0.0.0.0:8093 and the GPU as a
;;     DRM client, the new service cannot bind or allocate, shepherd respawns
;;     until it disables the service, and /v1/models keeps answering with the
;;     OLD model id — a green deploy report and a healthy-looking port check
;;     over an endpoint that 404s every pi request.  Find it with `pgrep -af
;;     ovms', compare against the shepherd child, and `kill -KILL' it directly:
;;     SIGTERM has been observed leaving a wedged ovms in futex_do_wait still
;;     holding the card.
;;   * If the deploy ends in the docs/fleet-deployment.org §4 ACL gap
;;     (`unauthorized public key'), the generation already switched but
;;     upgrade-shepherd-services never ran, so NOTHING was unloaded or started.
;;     Finish by hand and record it.
;; A `herd stop' of the old name first is still cheap insurance.  Note also that
;; a 5 s SIGKILL of a process holding ~18 GB of VRAM can race the immediately
;; following start into a transient CL_MEM_OBJECT_ALLOCATION_FAILURE plus
;; respawn churn, so leave a few seconds between them.  Then verify per
;; docs/fleet-deployment.org that the service's "running since" is AFTER the
;; generation-switch time — a generation bump alone does not prove the new
;; process is the one serving.
;;
;; COEXISTENCE WITH (peteches packages openvino).  OVMS bundles its own OpenVINO
;; 2026.4.1 and GenAI 2026.4.1.0 and does not use this repo's from-source build.
;; The two do not conflict (the package puts $out/lib first in RUNPATH so the
;; bundled runtime always wins for its own process).  That package is now
;; imported by NO system config — its only consumer was the deleted
;; (peteches services openvino) — and is kept anyway; see "DELETED ALONGSIDE IT"
;; above.
;;
;; auto-start? defaults to #f: the model directory is a large out-of-band
;; artifact that has to be downloaded or exported first (`optimum-cli export
;; openvino`, or a vendor IR repo), and a boot-time crash loop over an absent
;; model helps no one.  `herd start <service-name>` for a trial run, then flip
;; auto-start? on the instance once the model is proven — which the production
;; Qwen3.6 instance has done, and why it sets `#t`.

(define-module (peteches services ovms)
  #:use-module (guix gexp)
  #:use-module (guix packages)              ;package-version
  #:use-module (guix records)
  #:use-module (gnu packages python)        ;python
  #:use-module ((gnu packages python-xyz) #:select (python-jinja2 python-numpy))
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (srfi srfi-1)                ;append-map, delete-duplicates, fold
  #:use-module (srfi srfi-13)               ;string-any, string-join
  #:use-module (peteches packages ovms)
  #:use-module (peteches services firewall)
  #:export (ovms-server-configuration
            ;; Generated by define-record-type* below; re-exported explicitly so
            ;; callers can (use-modules …) it like any other binding.  Note it is
            ;; a *syntax transformer*, not a procedure — see ovms-configs.
            ovms-server-configuration?
            ovms-service-type))

;;; ── Utilities ────────────────────────────────────────────────────────────

;; Whitelist for `service-name', which is spliced by raw string-append into nft
;; `comment "…"' strings (rendered verbatim by peteches/services/firewall.scm),
;; into the shepherd `provision' symbol via string->symbol, and into the log
;; file and state directory paths.  A whitelist rather than a blacklist because
;; the empty string passes every blacklist check and is the WORST case: it makes
;; provision the empty symbol #{}#, the log file /var/log/ovms/.log, and the
;; state dir /var/lib/ovms/ -- the parent of every other instance's state dir,
;; which activation would then chown.
(define %ovms-service-name-chars
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._-")

(define (maybe-option flag value)
  "Return (FLAG VALUE) when VALUE is set, and '() when it is #f.

Every string and numeric field goes through this, so `#f' means \"leave OVMS's
own default alone\" rather than \"pass an empty value\": an omitted flag is the
only way to inherit an upstream default, and a stray #f in the argument list
would reach execve as the string \"#f\" and be rejected by cxxopts.  Numbers are
rendered with number->string so a field can be written as 256 or \"256\".
Mirrors maybe-option in peteches/services/vllm.scm."
  (if value
      (list flag (if (number? value) (number->string value) value))
      '()))

(define (bool->ovms value)
  "Render a Scheme boolean as OVMS's cxxopts boolean string.

OVMS's bool options are value-taking (`--enable_prefix_caching true`), not bare
switches, so #t/#f must become the literal strings \"true\"/\"false\" — that is
what its cxxopts parser accepts, and passing \"#t\" is a config error."
  (if value "true" "false"))

(define (ovms-configs config-or-configs)
  "Return the list of configurations held in CONFIG-OR-CONFIGS.

Guix calls a service extension with the *value* of one instance at a time — see
gnu/services.scm, which does (compute (service-value service)) per dependent and
then composes the results across instances.  This repo uses that single value two
ways: transcriber's instances can carry a LIST of configurations,
koboldcpp's and strata's carry one record (the deleted openvino service took
either, and its instances were written as single records).  Accept both shapes
so an instance can be written whichever reads better in a system config; a
service type should not have opinions about that.

Dispatch is on `pair?', not on the generated predicate: define-record-type* makes
its predicate a syntax transformer, and using it in ordinary procedure position
fails with \"wrong type to apply\" — which is also why nothing in this repo's
services calls a *-configuration? at run time.  A wrong value still gets caught,
just by the first accessor that needs a configuration field rather than here.

The `null?' arm is explicit because the empty list is not a `pair?'.  Without it,
an instance whose value is `'()' — a perfectly reasonable way to write \"this
host runs no ovms\", and what a conditional `(if gpu? (list cfg) '())' in a
system config produces on the non-GPU branch — becomes `(list '())', i.e. a
one-element list whose only element is `'()'.  Every accessor then dies deep
inside Guix's record machinery with \"Wrong type to apply\" / \"expecting
struct\", blaming whichever extension happened to run first instead of the empty
list.  A service with no instances should contribute nothing to any extension,
which is exactly what returning `'()' here gives all five of them for free.

NOTE: the deleted peteches/services/openvino.scm's openvino-configs carried the
identical latent bug (same `pair?' dispatch, no `null?' arm); it was never fixed
and the module is gone, so there is nothing left to reconcile.  If another
service in this repo ever copies this shape, factor the helper out rather than
copying the `null?' arm again."
  (cond
   ((null? config-or-configs) '())
   ((pair? config-or-configs) config-or-configs)
   (else (list config-or-configs))))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <ovms-server-configuration> ovms-server-configuration
                     make-ovms-server-configuration
  ovms-server-configuration?
  (service-name ovms-server-configuration-service-name
                (default "ovms"))
  (documentation ovms-server-configuration-documentation
                 (default "Intel OpenVINO Model Server (OpenAI-compatible REST)."))
  ;; See the module header: the model directory is an out-of-band artifact, and
  ;; a boot-time crash loop over an absent model helps no one.
  (auto-start? ovms-server-configuration-auto-start?
               (default #f))

  ;; The OVMS package.  It bundles its own OpenVINO/GenAI, so there is no
  ;; interpreter to point at — contrast the package/genai/python trio the
  ;; deleted openvino-server-configuration needed.  The `python' field below is NOT an interpreter
  ;; either: it exists only so ovms-profile-site-packages can derive the
  ;; python3.X minor version for the merged site-packages path.
  (package ovms-server-configuration-package
           (default ovms))

  ;; Used solely to compute the profile's site-packages directory name
  ;; (/run/current-system/profile/lib/python3.12/site-packages), which has to go
  ;; on PYTHONPATH so the embedded interpreter can import jinja2 — see the
  ;; RUNTIME ENVIRONMENT section of the module header.  Must match the python
  ;; that the system profile actually merges, which on this fleet is (gnu
  ;; packages python)'s `python'.
  (python ovms-server-configuration-python
          (default python))

  ;; Whether to merge `package' into the **system profile** (i.e. whether
  ;; ovms-packages contributes anything to profile-service-type).
  ;;
  ;; DEFAULT #f, AND DO NOT FLIP IT CASUALLY.  The service execs
  ;; $out/bin/ovms by absolute store path, so a profile entry is not what makes
  ;; the server work — it only puts `ovms' and `convert_tokenizer' on the
  ;; operator's PATH for interactive debugging.  The cost is a silent file-level
  ;; collision inside the profile: ovms bundles an Ubuntu-built oneTBB **12.13**
  ;; at lib/libtbb.so.12, and Guix's `onetbb` **12.19** was in comfyui's system
  ;; profile while `(peteches services openvino)` was wired there, because
  ;; peteches/packages/openvino-genai has `(propagated-inputs (list onetbb))`
  ;; and that service's profile extension merged genai.  THAT SERVICE IS NOW
  ;; DELETED, so whether onetbb is still in the profile depends on the remaining
  ;; packages and has NOT been re-verified — treat the collision as latent
  ;; rather than absent, and re-check before flipping this field.
  ;; Guix reports *neither* half of that:
  ;;   * `&profile-collision-error` matches on manifest entry **name + output**,
  ;;     and `ovms' vs `opencl-icd-loader'/`onetbb' differ, so it cannot fire;
  ;;   * file-level collisions inside a profile are settled by union-build's
  ;;     `resolve-collision/default`, which picks `(first files)` — and
  ;;     `build-profile` passes `#:log-port (%make-void-port "w")`, so the
  ;;     warning it would print goes to /dev/null.
  ;; The winner is therefore a coin-flip on manifest input order with zero build
  ;; output, while strata, koboldcpp and vllm all point LD_LIBRARY_PATH at
  ;; /run/current-system/profile/lib (verified: strata.scm's wrapper exports it,
  ;; koboldcpp.scm and vllm.scm set it in the service environment), so anything
  ;; they dlopen can resolve libtbb from the profile if one is there.  If the
  ;; vendor's older 12.13 wins that coin-flip, those services can break on any
  ;; oneTBB symbol added after 12.13 and nothing in the failure points back at
  ;; ovms.
  ;;
  ;; Do NOT "fix" this by deleting libtbb.so* from the package instead: keeping
  ;; the vendor tree intact is deliberate, and because $out/lib is first in
  ;; RUNPATH ovms always resolves its own 12.13 correctly regardless of what the
  ;; profile holds.  Turn this on only for a debugging session, on a host where
  ;; nothing else resolves libtbb from the profile.
  (add-to-profile? ovms-server-configuration-add-to-profile?
                   (default #f))

  (create-account? ovms-server-configuration-create-account?
                   (default #t))
  (user ovms-server-configuration-user
        (default "ovms"))
  (group ovms-server-configuration-group
         (default "ovms"))

  ;; The model OVMS serves.  `model-name' is what clients put in the request's
  ;; `model' field (--model_name); `model-path' is the directory on disk
  ;; (--model_path).  Measured working against plain IR export directories, i.e.
  ;; the layout `optimum-cli export openvino' produces and the layout Intel
  ;; publishes on HuggingFace: /media/models/openvino/LFM2.5-8B-A1B-int4-ov
  ;; (CPU and GPU), the retired /media/models/openvino/LFM2.5-24B-A2B-int8-ov
  ;; (GPU), and the current production
  ;; /media/models/openvino/Qwen3.6-35B-A3B-int4-ov (GPU, 2026-11-26 — see the
  ;; header).  If an export is instead laid out as a classic OVMS model
  ;; repository, point model-path at the repository root and let OVMS pick the
  ;; version.  OVMS only *reads* it in serve mode (see
  ;; "FLAGS THIS SERVICE NEVER PASSES" above), so the service user needs read
  ;; access and nothing more — worth remembering on a box where /media/models is
  ;; group-restricted.
  ;;
  ;; COUPLING TRAP: `model-name' is CLIENT-VISIBLE, not a label.  OVMS routes
  ;; requests by --model_name and /v1/models returns that string verbatim as the
  ;; model id (the deleted openvino-server.py shim returned basename(MODEL_DIR)
  ;; instead, so the two are not interchangeable by habit).
  ;; configs/pi/defaults/models.json pins the id, and
  ;; configs/pi/defaults/settings.json keys both `defaultModel` and its
  ;; compaction.modelOverrides entry off the same string.  For the production
  ;; instance all three must be exactly "Qwen3.6-35B-A3B-int4-ov"; any mismatch
  ;; 404s every pi request.  During the Qwen3.6 shakedown the served name was
  ;; temporarily spoofed as "LFM2.5-24B-A2B-int8-ov" so the then-live client
  ;; config kept working — a useful trick, and one that must never be committed.
  (model-name ovms-server-configuration-model-name)
  (model-path ovms-server-configuration-model-path)

  ;; OVMS task string.  text_generation selects the LLM/Continuous-Batching
  ;; servable, which is the only one this repo serves.
  (task ovms-server-configuration-task
        (default "text_generation"))

  ;; REST bind address and port (--rest_bind_address / --rest_port).  Loopback by
  ;; default; see the port-choice note in the module header for why this is 8094
  ;; and not the shim's 8093.
  (host ovms-server-configuration-host
        (default "127.0.0.1"))
  (port ovms-server-configuration-port
        (default 8094))

  ;; gRPC port (--port).  0 means "do not start the gRPC server" — OVMS logs
  ;; `Port was not set. GRPC server will not be started.'  CHOSEN: when the field
  ;; is 0 this service emits **no** --port flag at all rather than `--port 0`, so
  ;; the REST-only configuration is expressed by OVMS's own default instead of by
  ;; an explicit zero that a future release could reinterpret.  Set a real port
  ;; number to turn gRPC on.
  ;;
  ;; NIT: ovms-firewall-rules only ever opens `port' (REST).  So a non-zero
  ;; grpc-port together with open-firewall? #t yields a gRPC server that is
  ;; listening, healthy, and unreachable from off-box — fail-closed by design
  ;; (nobody has asked for remote gRPC, and an unauthenticated second listener is
  ;; not something to open implicitly), but it will read as "gRPC is broken".
  ;; If remote gRPC is ever genuinely wanted, add a matching dport rule here
  ;; rather than debugging the server.
  (grpc-port ovms-server-configuration-grpc-port
             (default 0))

  ;; OV device string (--target_device).  AUTO lets OVMS choose from what it
  ;; enumerates (`Available devices for Open VINO: CPU, GPU` on comfyui), which
  ;; in practice means the Arc card — the path verified on 2026-11-26 (see the
  ;; header).  Say "CPU" explicitly for a deliberate CPU trial, and be aware
  ;; that on a host with more than one GPU `AUTO` can pick *either*: see
  ;; opencl-icd below for why this service exposes only the Intel one.
  (target-device ovms-server-configuration-target-device
                 (default "AUTO"))

  ;; THE SINGLE OpenCL ICD DESCRIPTOR TO EXPOSE.  Activation symlinks this file
  ;; into opencl-icd-dir and OCL_ICD_VENDORS points at that directory — so the
  ;; ocl-icd loader sees exactly one platform, the Intel one.  Default is the
  ;; intel.icd that `intel-opencl-icd' merges into the system profile; a
  ;; file-like object (a store path) also works, since it is spliced into the
  ;; activation gexp with #$.
  ;;
  ;; Why restrict it at all: pointing OCL_ICD_VENDORS at the whole profile merge
  ;; exposes intel.icd, pocl.icd **and** nvidia.x86_64.icd, and with
  ;; target-device defaulting to "AUTO" OVMS is then free to select the RTX
  ;; 4090 (already full of ComfyUI → CL_MEM_OBJECT_ALLOCATION_FAILURE in ~3.3 s)
  ;; or a CPU-backed `pocl' pseudo-GPU — a ~20x throughput cliff on a server that
  ;; reports itself perfectly healthy.  See ovms-environment for the full
  ;; measurement table.
  (opencl-icd ovms-server-configuration-opencl-icd
              (default "/run/current-system/profile/etc/OpenCL/vendors/intel.icd"))

  ;; Where the synthesised vendors directory lives.  #f resolves to
  ;; <state-dir>/opencl-vendors (i.e. /var/lib/ovms/<service-name>/
  ;; opencl-vendors), which activation mkdir-p's and populates before the
  ;; service starts — same pattern as log-file / ovms-resolved-log-file.  Per
  ;; service-name, so two instances never race over one symlink.
  (opencl-icd-dir ovms-server-configuration-opencl-icd-dir
                  (default #f))

  ;; --pipeline_type.  Defaults to "AUTO", which for the Qwen3.6-35B-A3B INT4
  ;; IR this fleet serves is MEASURED GOOD: it selects the Continuous Batching
  ;; servable and scored 3/3 on the agentic benchmark (8-gram repeat 2, largest
  ;; thinking block 2 972 chars, 6 clean tool calls, 62 s).
  ;;
  ;; An earlier revision of this comment said "keep AUTO, because forcing LM on
  ;; the strength of Intel KB 000103073 would silently lose streaming `usage'
  ;; and `finish_reason=length'".  Both halves were wrong and the second was
  ;; never tested: the Legacy/stateful servable DID return both.  What was
  ;; actually measured on the retired LFM2.5 INT8 graph is that AUTO → CB
  ;; degenerated badly (8-gram repeat 47–407, thinking blocks to 89 199 chars)
  ;; while LM produced clean output (repeat 3, 3 664 chars) but re-prefilled the
  ;; whole conversation every turn, escalated 7.8 → 42.8 s per turn, then hung
  ;; indefinitely — and the wedged process survived SIGTERM and held the GPU as
  ;; a DRM client until SIGKILL, which matters because shepherd's
  ;; make-kill-destructor below sends SIGTERM first.  Full detail and the
  ;; general lesson (a servable reaching AVAILABLE is NOT evidence of correct
  ;; output) are in the module header.  Choose this field from measured output
  ;; quality per model, not from a successful startup or a vendor KB article.
  ;;
  ;; SILENTLY IGNORED FOR NON-GENERATIVE TASKS: ovms-serve-args omits this flag
  ;; entirely unless ovms-generative-task? holds, because OVMS rejects it at the
  ;; option-parse phase under `embeddings' and `rerank'.  So setting
  ;; `pipeline-type' on such an instance does nothing at all -- it is dropped,
  ;; not passed and not an error.  That is a deliberate exception to this
  ;; module's usual refusal to ignore configuration quietly (contrast
  ;; ovms-firewall-rules, which RAISES); it is acceptable here only because
  ;; there is no sensible value for a non-generative task.  Do not "fix" it by
  ;; rendering the flag unconditionally.
  (pipeline-type ovms-server-configuration-pipeline-type
                 (default "AUTO"))

  ;; Native markup parsers (--tool_parser / --reasoning_parser).  BOTH DEFAULT
  ;; TO #f, i.e. the flags are OMITTED and OVMS auto-detects from the model's
  ;; chat template.  GENERATION-SIDE FIELDS, and unlike pipeline-type and the
  ;; CB booleans they are NOT task-gated -- `maybe-option' already omits them
  ;; while they are #f, which is the only state a non-generative instance
  ;; should leave them in.  Setting either on an `embeddings' or `rerank'
  ;; instance would render it and hit the same "unmatched arguments" parse
  ;; failure the gate exists to prevent; neither task has a chat template to
  ;; parse.  Auto-detection is the default because the fleet no longer
  ;; serves a single model family: it measured correct for both models this
  ;; service has run — Qwen3.6-35B-A3B logs `Auto-detected tool_parser:
  ;; qwen3coder' and `Auto-detected reasoning_parser: qwen3', and the retired
  ;; LFM2.5 export auto-detected `lfm2'.  Set either field to a string to pin a
  ;; parser instead (the previous LFM2.5 instance pinned both to "lfm2", and
  ;; measured `Using manually configured tool_parser: lfm2'); pinning a value
  ;; that is wrong for the served model is a hard startup failure, not a
  ;; fallback.
  ;;
  ;; HISTORY, kept because it is a live trap for anyone who ever serves an LFM
  ;; model again: `lfm2' is the ONLY LFM-family parser name OVMS 2026.4.1
  ;; accepts, for BOTH fields — there is no `lfm25' value, even though the
  ;; upstream reasoning parser's source file is named
  ;; src/llm/io_processing/lfm2/lfm25_reasoning_parser.hpp (that is where the
  ;; LFM2.5-specific <think>/</think> handling lives, with both delimiters
  ;; registered as special tokens): the parser is REGISTERED UNDER THE NAME
  ;; `lfm2'.  Passing `--reasoning_parser lfm25' makes OVMS refuse to start.
  ;; Do NOT "upgrade" a pinned value on the strength of the upstream filename.
  ;; The supported lists below were obtained in OVMS 2026.4.1 by passing each
  ;; flag a deliberately invalid value and reading the error verbatim:
  ;;
  ;;   Unsupported tool_parser: "bogus-xyz". Supported tool parsers are:
  ;;     llama3, hermes3, phi4, mistral, gptoss, qwen3coder, devstral, lfm2,
  ;;     gemma4, onyx, minicpm5
  ;;   Unsupported reasoning_parser: "lfm25". Supported reasoning parsers are:
  ;;     qwen3, gemma4, gptoss, onyx, minicpm5, lfm2, devstral
  (tool-parser ovms-server-configuration-tool-parser
               (default #f))
  (reasoning-parser ovms-server-configuration-reasoning-parser
                    (default #f))

  ;; Continuous-Batching knobs.  For a GENERATIVE task the three booleans are
  ;; always rendered as `true'/'false' — never omitted — so the command line
  ;; is a complete statement of the configuration and cannot change meaning
  ;; when an OVMS release moves an upstream default.  For a non-generative
  ;; task they are omitted ENTIRELY, because OVMS rejects them at the option
  ;; parse; see %ovms-generative-tasks.  max-num-seqs (#f = OVMS's 256) caps
  ;; concurrent sequences;
  ;; cache-size is the KV cache budget in GB (#f = OVMS's own default, 0 = grow
  ;; dynamically) and is the knob to reach for when this VM's other GPU tenants
  ;; (ComfyUI, and the ovms retrieval instances) need the memory back.
  ;;
  ;; max-num-seqs, cache-size and kv-cache-precision below are ALSO
  ;; generation-side and ALSO not task-gated -- same reasoning as the parser
  ;; fields above: `maybe-option' omits them while they are #f, which is the
  ;; only state any non-generative instance should leave them in.  Setting one
  ;; on an `embeddings' or `rerank' instance risks the same parse failure.
  (enable-tool-guided-generation?
   ovms-server-configuration-enable-tool-guided-generation?
   (default #f))
  (max-num-seqs ovms-server-configuration-max-num-seqs
                (default #f))
  (cache-size ovms-server-configuration-cache-size
              (default #f))
  ;; NEVER TURN THIS OFF.  Measured on the Arc Pro B70 with the served
  ;; Qwen3.6-35B-A3B INT4 IR: a cold 44 817-token prompt prefills in 30.84 s
  ;; (1 453 T/s); the SAME prompt repeated with prefix caching on takes 3.78 s
  ;; (11 850 T/s effective) — an **8.2× speedup**.  Multi-turn agent work
  ;; re-sends almost the entire conversation on every turn, so this flag is what
  ;; makes it viable at all rather than a tuning nicety: koboldcpp on the same
  ;; card, which has no GPU-resident prefix cache, spent 99 minutes and 48
  ;; requests without finishing the task OVMS finished in 62 seconds.
  ;;
  ;; It is also the reason MTP/speculative decoding is rejected — the two are
  ;; mutually exclusive and enabling both aborts the executor and then
  ;; segfaults OVMS.  See "MTP / SPECULATIVE DECODING: TESTED AND REJECTED" in
  ;; the module header before touching either.
  (enable-prefix-caching? ovms-server-configuration-enable-prefix-caching?
                          (default #t))
  (dynamic-split-fuse? ovms-server-configuration-dynamic-split-fuse?
                       (default #t))

  ;; --plugin_config: a JSON string handed to the OpenVINO plugin verbatim, e.g.
  ;; "{"NUM_STREAMS": "1"}".  Passed through unmodified — quoting it is the
  ;; system config's job, and this service does not try to build JSON.
  (plugin-config ovms-server-configuration-plugin-config
                 (default #f))

  ;; --kv_cache_precision: "u8" to halve KV cache memory at some quality cost,
  ;; or #f to leave the plugin's default (f16/bf16) alone.
  (kv-cache-precision ovms-server-configuration-kv-cache-precision
                      (default #f))

  ;; --log_level: one of TRACE, DEBUG, INFO, WARNING, ERROR.
  ;;
  ;; KEEP THIS AT "INFO" IN PRODUCTION.  Measured on comfyui 2026-11-26 against
  ;; the then-serving LFM2.5-8B-A1B-int4-ov export (the log level is not
  ;; model-specific, but the measurement is of that run): at INFO the log
  ;; carries only request counts and KV
  ;; cache utilisation (`All requests: 1; Scheduled requests: 1; Cache type:
  ;; dynamic, cache usage: 69.8% of 9.9 MB') and a grep for six distinct strings
  ;; from the test prompts found NONE of them.  At DEBUG the prompt text is
  ;; logged -- a request whose user message contained a unique sentinel had that
  ;; sentinel appear twice in the log.
  ;;
  ;; That matters because this file is shipped to Loki by comfyui's alloy
  ;; log-files entry, so anything in it is readable by whoever can query Loki,
  ;; not just whoever can ssh to the box.  The shim this service replaced had an
  ;; explicit version of this policy (its _log docstring promised never prompt or
  ;; completion text, and only exception TYPE NAMES for calls carrying a client
  ;; conversation, because third-party Jinja/pybind errors quote the offending
  ;; value back).  DEBUG or
  ;; TRACE here silently drops that guarantee for every client of the endpoint.
  ;; Use them for a bounded local investigation, then set it back.
  ;;
  ;; Related, and parity with today rather than a regression: drogon's access-log
  ;; format is `$request_date $method $url [$body_bytes_received] ($remote_addr -
  ;; $local_addr) $status $body_bytes_sent $processing_time', so if that logger is
  ;; ever enabled the CLIENT'S TAILNET ADDRESS lands in the Loki-shipped file
  ;; too.  uvicorn's access lines did this for the shim.  Do not "turn on the
  ;; access log for observability" without noticing that.
  (log-level ovms-server-configuration-log-level
             (default "INFO"))

  ;; shepherd's #:log-file, i.e. where stdout/stderr land.  #f resolves to
  ;; /var/log/ovms/<service-name>.log, whose directory ovms-activation creates
  ;; and chowns before the service starts — shepherd creates the *file* (O_CREAT)
  ;; but not its parent directory, so without activation the constructor fails.
  ;; This is not OVMS's own --log_path, which is deliberately not exposed.
  (log-file ovms-server-configuration-log-file
            (default #f))

  ;; --rest_workers.  #f leaves OVMS's default, which is the CPU count (observed:
  ;; "Will start 24 REST workers", "24 unary threads and 24 streaming
  ;; threads").  Lower it if the worker threads' CPU use shows up against
  ;; ComfyUI or AllTalk on the same box.
  (rest-workers ovms-server-configuration-rest-workers
                (default #f))

  ;; --api_key_file: path to a file of accepted API keys.  #f leaves the endpoint
  ;; **unauthenticated** (OVMS logs `Authentication will be disabled.'), which is
  ;; only acceptable because the default bind address is loopback and the default
  ;; firewall policy opens nothing.
  ;;
  ;; If you do add a key, NEVER inline it as `API_KEY=…' in
  ;; extra-environment-variables.  A system config is committed to this repo, and
  ;; the value would also be baked into the world-readable shepherd gexp under
  ;; /gnu/store — two leaks from one line, and exactly what AGENTS.md's
  ;; secret-handling rules forbid.  Use the repo's sops-secret convention (see
  ;; peteches/systems/caddy.scm and peteches/systems/ai-bots.scm) to decrypt it
  ;; to /run/secrets/… and point this field THERE — not at a store path, which is
  ;; world-readable.  When you do, add `sops-secrets' to the (requirement …) list
  ;; in ovms-shepherd-service, or the service can start before the file exists.
  (api-key-file ovms-server-configuration-api-key-file
                (default #f))

  ;; Only open the firewall when intentionally serving beyond localhost.
  (open-firewall? ovms-server-configuration-open-firewall?
                  (default #f))

  ;; CIDRs the accept rule is narrowed to, one nft rule each.  Empty (the
  ;; default) means "unrestricted accept" — which ovms-firewall-rules now
  ;; REFUSES to emit when api-key-file is also #f, raising at
  ;; configuration-evaluation time instead (see that function).  So whenever
  ;; open-firewall? is #t, either list the CIDRs that may reach the endpoint or
  ;; set api-key-file: with neither, an unrestricted accept hands an
  ;; unauthenticated GPU endpoint to every host on the 192.168.50.0/23 LAN, not
  ;; just the tailnet that caddy and the desktops arrive over.  Same shape as
  ;; strata-server-configuration's firewall-sources and the deleted
  ;; openvino-server-configuration's, including the Tailscale CGNAT + ULA
  ;; prefixes; see the instance in peteches/systems/comfyui.scm for why all
  ;; three CIDRs are listed.
  (firewall-sources ovms-server-configuration-firewall-sources
                    (default '()))

  ;; Unrestricted passthroughs, appended LAST so an instance can override any
  ;; field above.  That is also what makes them the only two routes to a flag
  ;; this record deliberately does not expose, so two rules apply:
  ;;   * no `--pull' or `--source_model' in extra-args.  A boot-time Hugging
  ;;     Face download is the crash-loop failure mode the deleted (peteches
  ;;     services openvino) warned about, and it needs an HF_TOKEN that
  ;;     extra-environment-variables would then have to carry inline.
  ;;   * no API_KEY / HF_TOKEN literal in extra-environment-variables, for the
  ;;     committed-to-git plus world-readable-gexp reason given on
  ;;     api-key-file above.
  (extra-environment-variables
   ovms-server-configuration-extra-environment-variables
   (default '()))
  (extra-args ovms-server-configuration-extra-args
              (default '())))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (ovms-profile-site-packages python-pkg)
  "The system profile's merged site-packages directory for PYTHON-PKG, e.g.
/run/current-system/profile/lib/python3.12/site-packages.

Both halves are known at build time — the profile path is fixed and the python
minor version is part of the package definition — so this returns a plain string,
which the gexp embeds with #$.  Mirrors the deleted
openvino-profile-site-packages.

Needed because OVMS's embedded interpreter must be able to `import jinja2', and
jinja2 is NOT bundled in $out/lib/python.  A raw shepherd process gets no profile
search-path environment (no GUIX_PYTHONPATH), so this service has to name the
merged directory itself.  Without it OVMS silently falls back to the minja C++
template engine, which does not implement every chat template — see the module
header."
  (let ((parts (string-split (package-version python-pkg) #\.)))
    (string-append "/run/current-system/profile/lib/python"
                   (car parts) "." (cadr parts) "/site-packages")))

(define (ovms-resolved-state-dir config)
  "The account's home directory and the service's own state dir.

OVMS writes nothing here — it is given no --pull and no --log_path — but a system
account still needs a home that is not /var/empty, and shepherd sets HOME from
the passwd entry rather than from this service's environment list.  Per
service-name so two instances do not share one directory.  When several instances
share a *user* name, the first one wins the account's home (see ovms-accounts);
the others still get their own directory created and chowned by activation.

That shared HOME is currently harmless, and was verified empirically rather than
assumed: comfyui runs three instances under one `ovms' account, so ovms-embed and
ovms-rerank both run with HOME=/var/lib/ovms/ovms-qwen36, and ovms-embed served
for hours that way with nothing written there.  It holds only because OVMS is
given neither --pull nor --log_path.  If a future OVMS ever caches under $HOME (a
Hugging Face hub cache, crash dumps, a telemetry spool) the three processes would
share one directory and could race -- at which point give the instances distinct
users rather than relying on this."
  (string-append "/var/lib/ovms/"
                 (ovms-server-configuration-service-name config)))

(define (ovms-resolved-log-file config)
  (or (ovms-server-configuration-log-file config)
      (string-append "/var/log/ovms/"
                     (ovms-server-configuration-service-name config) ".log")))

(define (ovms-resolved-opencl-icd-dir config)
  "The directory OCL_ICD_VENDORS points at: a synthesised, instance-private
vendors directory holding exactly one symlink, to the ICD named by `opencl-icd'.

`opencl-icd-dir' must be a plain, writable string (or #f): ovms-environment
string-appends this value into an environment entry at meta time, and a
file-like object here raises wrong-type-arg in string-append.  It could not be
a store path anyway — activation has to mkdir-p and populate the directory,
which a read-only store path could not do.  Activation creates the directory
and the symlink inside it before the shepherd constructor runs.

Why a directory of one rather than the profile merge or the single .icd file is
documented at length in ovms-environment — the short version is that the file
form yields ZERO GPUs and the whole-merge form yields the wrong GPU.

Note there is deliberately no `ovms-resolved-opencl-icd-link' counterpart: the
symlink's own name is derived from the ICD's basename, and `opencl-icd' may be a
file-like object rather than a string, so the basename is taken INSIDE the
activation gexp (where #$ has already resolved it to a plain string) rather than
here at meta time."
  (or (ovms-server-configuration-opencl-icd-dir config)
      (string-append (ovms-resolved-state-dir config) "/opencl-vendors")))

;;; ── Command line ─────────────────────────────────────────────────────────

;; Tasks whose OVMS option grammar accepts the generation-only flags
;; (--pipeline_type, --enable_tool_guided_generation,
;; --enable_prefix_caching, --dynamic_split_fuse).
;;
;; MEASURED on OVMS 2026.4.1 (comfyui, 2026-11-27): under `--task embeddings'
;; all FOUR are rejected at the PARSE phase, before any model is touched --
;;
;;   task: embeddings - error parsing options - unmatched arguments :
;;   --pipeline_type, AUTO, --enable_tool_guided_generation, false,
;;   --enable_prefix_caching, true, --dynamic_split_fuse, true
;;
;; and the identical command line minus those four parses cleanly, enumerates
;; CPU + GPU and binds the port, failing only later at model load.  cxxopts
;; rejects a flag that is not in the selected task's grammar rather than
;; ignoring it, so these must be omitted per task: while they were rendered
;; unconditionally an `embeddings' instance could never start at all.
;;
;; `text_generation' is the only task this fleet has measured as accepting
;; them.  Add another task here ONLY after measuring it the same way -- the
;; safe assumption for an unmeasured task is that it does not.
;;
;; NOTE this is defined here, below the record type, not up with the other
;; helpers: define-record-type* binds its accessors as SYNTAX TRANSFORMERS,
;; so a function body that calls one must be expanded after the record
;; definition.  Placing this beside bool->ovms compiles but fails at runtime
;; with "Wrong type to apply: #<syntax-transformer
;; ovms-server-configuration-task>".
(define %ovms-generative-tasks '("text_generation"))

(define (ovms-generative-task? config)
  "True when CONFIG's task accepts OVMS's generation-only flags.

#f (task omitted) is treated as generative, because OVMS then infers the task
from the model config and every model this fleet has served that way was a
generation model; being wrong in that direction only reproduces the
long-standing behaviour rather than silently dropping --enable_prefix_caching
from an LLM, which costs 8.2x on repeated prompts."
  (let ((task (ovms-server-configuration-task config)))
    (or (not task) (member task %ovms-generative-tasks))))

(define (ovms-serve-args config)
  "Render CONFIG's ovms command line, minus the executable itself.

Flag order follows the invocation measured working on the target host
(--model_name … --model_path … --task … --rest_port … --rest_bind_address …
--target_device … --pipeline_type … --tool_parser … --reasoning_parser …
--log_level) so that a rendered command line can be diffed against that
measurement by eye.  The two parser flags are OMITTED whenever their field is
#f, which is now the default, so a default-configured instance renders no
--tool_parser/--reasoning_parser at all and OVMS auto-detects both from the
chat template.  extra-args comes last, so an instance can override anything
above it without this function having to know about it.

Nothing here may return #f: every optional field goes through maybe-option,
which yields '() instead.  The rendered argv for the production instance was
verified to be 22 elements with no #f anywhere in it — see the module's
`CURRENT SHAPE' header section for what that command line must contain.

The three Continuous-Batching booleans are the exception to maybe-option: for a
generative task they are always rendered, as the literal strings `true' and
`false', so the command line stays a complete statement of the configuration.
For any other task they — and --pipeline_type — are omitted entirely rather than
passed, because OVMS's cxxopts grammar is per-task and rejects them; see
%ovms-generative-tasks for the measured error text."
  (let ((grpc-port (ovms-server-configuration-grpc-port config)))
    (append
     (list "--model_name" (ovms-server-configuration-model-name config)
           "--model_path" (ovms-server-configuration-model-path config))
     (maybe-option "--task" (ovms-server-configuration-task config))
     ;; REST always: the port is a required field with a real default, so these
     ;; are emitted unconditionally.
     (list "--rest_port"
           (number->string (ovms-server-configuration-port config))
           "--rest_bind_address"
           (ovms-server-configuration-host config))
     (maybe-option "--target_device"
                   (ovms-server-configuration-target-device config))
     (if (ovms-generative-task? config)
         (maybe-option "--pipeline_type"
                       (ovms-server-configuration-pipeline-type config))
         '())
     (maybe-option "--tool_parser"
                   (ovms-server-configuration-tool-parser config))
     (maybe-option "--reasoning_parser"
                   (ovms-server-configuration-reasoning-parser config))
     ;; Booleans always rendered as true/false -- see the field comments --
     ;; but ONLY for a generative task.  All four of these (including
     ;; --pipeline_type above) are rejected outright by OVMS's option parser
     ;; under `--task embeddings'; see %ovms-generative-tasks for the
     ;; measured error text.
     (if (ovms-generative-task? config)
         (list "--enable_tool_guided_generation"
               (bool->ovms
                (ovms-server-configuration-enable-tool-guided-generation? config))
               "--enable_prefix_caching"
               (bool->ovms
                (ovms-server-configuration-enable-prefix-caching? config))
               "--dynamic_split_fuse"
               (bool->ovms
                (ovms-server-configuration-dynamic-split-fuse? config)))
         '())
     (maybe-option "--max_num_seqs"
                   (ovms-server-configuration-max-num-seqs config))
     (maybe-option "--cache_size"
                   (ovms-server-configuration-cache-size config))
     (maybe-option "--plugin_config"
                   (ovms-server-configuration-plugin-config config))
     (maybe-option "--kv_cache_precision"
                   (ovms-server-configuration-kv-cache-precision config))
     (maybe-option "--rest_workers"
                   (ovms-server-configuration-rest-workers config))
     (maybe-option "--api_key_file"
                   (ovms-server-configuration-api-key-file config))
     ;; gRPC off by omission, not by an explicit zero — see the field comment.
     (if (and grpc-port (not (zero? grpc-port)))
         (list "--port" (number->string grpc-port))
         '())
     (maybe-option "--log_level"
                   (ovms-server-configuration-log-level config))
     (ovms-server-configuration-extra-args config))))

;;; ── Environment ──────────────────────────────────────────────────────────

(define (ovms-environment config)
  "The #:environment-variables list for CONFIG's shepherd constructor.

Returns a mix of plain strings and gexps on purpose: shepherd's
#:environment-variables is spliced into a gexp, so each element only has to be
valid gexp data, and anything mentioning a store path has to *be* a gexp — at
this point `(file-append pkg \"/lib/python\")' is a <file-append> object, and a
meta-time string-append over it dies with \"wrong type to apply\".  Making the
env entry itself a gexp whose #$(...) splices are evaluated here, where pkg is
bound, is the idiom the deleted openvino-environment used for the same reason.

LD_LIBRARY_PATH *is* set, and it is not a courtesy line: the
package patchelf's a RUNPATH onto bin/ovms and every bundled ELF object, which
fully covers ovms's own closure, but the host's OpenCL ICD chain is dlopened by
absolute store path and carries no RUNPATH of its own.  See
\"RUNPATH COVERS OVMS, NOT THE HOST'S ICD CHAIN\" in the module header for the
libigdrcl.so → libigdgmm.so.12 chain and the A/B/C measurement table."
  (let* ((pkg (ovms-server-configuration-package config))
         (icd-dir (ovms-resolved-opencl-icd-dir config))
         ;; TWO entries.  $out/lib/python for the bundled openvino/genai/
         ;; tokenizers/pyovms, then the profile's merged site-packages so the
         ;; embedded interpreter can import jinja2 and numpy.  NEITHER is
         ;; bundled; both are declared by ovms-packages so they actually reach
         ;; the profile rather than resolving by accident.
         ;; Omitting the second makes OVMS fall back to minja, which does not
         ;; implement every chat template, and every /chat/completions request
         ;; then 400s.  See ovms-profile-site-packages and the module header.
         (site-packages
          (ovms-profile-site-packages (ovms-server-configuration-python config)))
         (pythonpath
          #~(string-append "PYTHONPATH="
                           #$(file-append pkg "/lib/python") ":" #$site-packages)))
    (append
     (list pythonpath)

     ;; A DIRECTORY OF ONE, NOT A FILE, AND NOT THE WHOLE PROFILE MERGE.
     ;;
     ;; Where the OpenCL ICD loader finds vendor descriptors.  A shepherd process
     ;; gets ONLY the variables listed here — nothing from an interactive
     ;; session's environment reaches it; without this line the loader
     ;; falls back to /etc/OpenCL/vendors, which does not exist on this VM, finds
     ;; no platform, and the GPU plugin fails with "[GPU] ProgramBuilder build
     ;; failed! could not create an engine".  Required for the GPU path even
     ;; though OVMS talks Level Zero, because oneDNN's ZE backend enumerates
     ;; devices through OpenCL; harmless for CPU.
     ;;
     ;; It points at the directory *this service synthesises* in activation
     ;; (ovms-resolved-opencl-icd-dir, default /var/lib/ovms/<name>/opencl-
     ;; vendors), which holds exactly one symlink to `opencl-icd' — by default
     ;; the profile's intel.icd.  Both halves of that are measured requirements:
     ;;
     ;;   1. It MUST be a directory.  ocl-icd scans a directory for `*.icd'
     ;;      entries; it does not accept a path to one file.  Pointing
     ;;      OCL_ICD_VENDORS directly at `<vendors>/intel.icd' enumerates **ZERO
     ;;      GPUs** — measured twice, with and without LD_LIBRARY_PATH (rows A
     ;;      and B of the header's table: both `devices: ['CPU']`).  This is the
     ;;      trap: the file form looks like the more precise, more secure
     ;;      "simplification" of the directory form and silently removes the
     ;;      Intel card.  Do not make that change.
     ;;   2. It MUST contain only the Intel ICD.  Pointing at the whole profile
     ;;      vendors merge exposes intel.icd, pocl.icd **and** nvidia.x86_64.icd
     ;;      (row C).  With LD_LIBRARY_PATH set that enumerates GPU.0 = Arc Pro
     ;;      B70 *and* GPU.1 = RTX 4090; with target-device defaulting to
     ;;      "AUTO" OVMS is free to pick either, or a CPU-backed `pocl'
     ;;      pseudo-GPU.  Picking the 4090 means aiming a 24B model at a card
     ;;      already full of ComfyUI — CL_MEM_OBJECT_ALLOCATION_FAILURE in
     ;;      ~3.3 s; picking pocl means a ~20x throughput cliff.  Both look like
     ;;      a perfectly healthy server from the outside.  Restricting the merge
     ;;      to one descriptor makes `AUTO` deterministic instead.
     ;;
     ;; The Intel ICD descriptor itself still has to exist in the **system
     ;; profile**, along with libigdgmm and zstd:lib — comfyui.scm's
     ;; #:extra-packages.  Documented dependency, not added here.
     (list (string-append "OCL_ICD_VENDORS=" icd-dir))

     ;; MANDATORY FOR THE INTEL GPU PATH — see the module header.  The bundled
     ;; libopenvino_intel_gpu_plugin.so dlopens the ICD loader, which dlopens
     ;; the absolute store path inside intel.icd (…-intel-opencl-icd-…/lib/
     ;; libigdrcl.so).  That object has NO DT_RPATH and NO DT_RUNPATH and
     ;; DT_NEEDEDs libigdgmm.so.12, which is in neither ovms's tree nor its
     ;; RUNPATH — it is reachable only from /run/current-system/profile/lib.  So
     ;; glibc's search order (DT_RPATH → LD_LIBRARY_PATH → DT_RUNPATH →
     ;; ld.so.cache, which Guix does not generate → /lib, /usr/lib, absent) has
     ;; exactly one slot we can fill, and if we do not fill it the Intel ICD
     ;; fails to load **silently** and no Intel GPU enumerates.  Same story for
     ;; NEO's libigdfcl.so.2 → libigc.so.2 → libzstd.so.1.
     ;;
     ;; Note this is NOT redundant with the package's patchelf work: that fixes
     ;; ovms's own closure, which was never broken.  And note a Guix login
     ;; shell supplies no LD_LIBRARY_PATH of its own (the profile's
     ;; etc/profile exports PATH/MANPATH/INFOPATH/GUILE_LOAD_PATH/SSL_CERT_DIR
     ;; etc. and no *LIBRARY_PATH), so a hand-test over SSH DOES reproduce the
     ;; GPU failure unless the test command itself exports this variable —
     ;; verify under `env -i` or via the lowered shepherd gexp, never by
     ;; assuming what a shell's environment contains.
     (list "LD_LIBRARY_PATH=/run/current-system/profile/lib:/run/current-system/profile/lib64")

     ;; Not strictly needed — the binary is exec'd by absolute store path and
     ;; resolves its own libraries through RUNPATH — but set so that anything
     ;; ovms ever spawns (and any `herd`-adjacent debugging) sees the system
     ;; profile.  Same line vllm.scm sets (and the deleted openvino.scm set).
     (list "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin")
     (ovms-server-configuration-extra-environment-variables config))))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (ovms-accounts config-or-configs)
  "The user-group and user-account records for every instance asking for one.

Deduplicated on purpose — two models served under one account (an int4 trial now,
an int8 instance later) would otherwise ask account-service-type for the same
group and user twice, which lands as duplicated /etc/passwd lines rather than as
an error.

Dedup is by *name*, not delete-duplicates over the records: Guile's
delete-duplicates compares with eq?, and two separately constructed user-group
records with identical fields are two different objects, so it would keep both.
Keys here are strings, where eq?-based dedup is also unreliable across instances,
so an explicit comparator is passed and accounts are folded by user name."
  (let* ((configs (ovms-configs config-or-configs))
         (want-account? (lambda (config)
                          (ovms-server-configuration-create-account? config)))
         ;; First config asking for a given user name decides its group and home
         ;; directory; later ones are ignored rather than merged, because "which
         ;; instance owns this account's /var/lib dir" should have one answer.
         ;; Entries are *pairs* (user . config), not 2-element lists: `(cons
         ;; (list user config) acc)' would make `cdr' return the list (config),
         ;; and every accessor on it then dies with "expecting struct".
         (by-user (fold (lambda (config acc)
                          (let ((user (ovms-server-configuration-user config)))
                            (if (assoc user acc) acc (cons (cons user config) acc))))
                        '() (filter want-account? configs)))
         ;; Groups are derived from by-user — the configs that actually own an
         ;; account — NOT from all configs: two instances sharing one user but
         ;; declaring different groups would otherwise ask account-service-type
         ;; for a second system group that no account ever references (an
         ;; orphan line in /etc/group), since the shared account only joins
         ;; the first config's group.
         (groups (delete-duplicates
                  ;; The accessor has to be wrapped: define-record-type* makes
                  ;; accessors syntax transformers, so `(map accessor lst)'
                  ;; rewrites at the call site and map ends up with too many
                  ;; arguments.  Every accessor use in this repo's services goes
                  ;; through a lambda for exactly this reason.
                  (map (lambda (entry)
                         (ovms-server-configuration-group (cdr entry)))
                       by-user)
                  string=)))
    (append (map (lambda (group)
                   (user-group (name group) (system? #t)))
                 groups)
            (map (lambda (entry)
                   (let ((config (cdr entry)))
                     (user-account
                      (name (ovms-server-configuration-user config))
                      (group (ovms-server-configuration-group config))
                      (system? #t)
                      (comment "OpenVINO Model Server daemon")
                      (home-directory (ovms-resolved-state-dir config))
                      (shell (file-append shadow "/sbin/nologin")))))
                 (reverse by-user)))))

(define (ovms-packages config-or-configs)
  "The packages to merge into the system profile.

Always includes `python-jinja2' and `python-numpy' (both pure Python, so
neither can collide, and OVMS's embedded interpreter needs them — see the
comment on the append below).  Includes the `ovms' package itself ONLY for
instances that opt in via `add-to-profile?', which defaults to #f, so by default
this returns a two-element list rather than `'()`.

WHY `ovms' ITSELF DEFAULTS OUT: THE oneTBB COLLISION IS SILENT.  ovms bundles an
Ubuntu-built oneTBB **12.13** at lib/libtbb.so.12.  Guix's `onetbb` **12.19** WAS
in comfyui's system profile while `(peteches services openvino)` was wired there,
because peteches/packages/openvino-genai has `(propagated-inputs (list onetbb))`
and that service's profile extension merged genai.  **That service is deleted
(2026-11-26), so this specific collision is now latent rather than live** —
whether onetbb still reaches comfyui's profile through some other package has
NOT been re-verified.  The default stays #f regardless, for the reason two
paragraphs down.  When the collision was live, putting ovms in the same profile
put two lib/libtbb.so.12 in one union, and Guix said nothing:

  * `&profile-collision-error` matches manifest entries on **name + output**.
    `ovms' and `onetbb' (or `opencl-icd-loader') differ, so the collision check
    that normally makes this class of mistake a build failure *cannot fire*.
  * File-level collisions inside a profile are settled by union-build's
    `resolve-collision/default`, which picks `(first files)` — and
    `build-profile` calls it with `#:log-port (%make-void-port \"w\")`, so the
    warning that would name the loser is written to /dev/null.

The winner is therefore decided by manifest input order, with no build output at
all.  That mattered — and would matter again if onetbb returns to the profile —
because strata, koboldcpp and vllm all point LD_LIBRARY_PATH at
/run/current-system/profile/lib, so anything they dlopen can resolve libtbb from
the profile.  If the vendor's 12.13 wins the coin-flip, those services can break
on any oneTBB symbol added after 12.13, and nothing in the resulting failure
points back at ovms.

The service does not need the profile entry: ovms-shepherd-service execs
#$out/bin/ovms by absolute store path, and the package's RUNPATH puts $out/lib
first so ovms always resolves its own bundled 12.13 correctly no matter what the
profile holds.  Do NOT \"fix\" this by deleting libtbb.so* from the package —
keeping the vendor tree intact is deliberate.  Turn `add-to-profile?' on only
for a debugging session on a host where nothing else resolves libtbb from the
profile, and turn it back off.

What the entry was ever for: putting `ovms' and `convert_tokenizer' on the
operator's PATH, and merging the package's etc/OpenCL/vendors (empty here) and
lib/ into the profile.  Note the OCL_ICD_VENDORS this service emits does NOT
come from that merge — it is the synthesised directory activation builds, whose
single symlink points at the *system profile's* intel.icd.

A second, benign collision for the record: ovms bundles its own OpenCL ICD
*loader*, so lib/libOpenCL.so{,.1,.1.0.0} also come from `opencl-icd-loader',
which comfyui.scm installs via #:extra-packages.  Building a profile with both
(guix package -i ovms opencl-icd-loader) succeeds silently with all three
symlinks pointing at opencl-icd-loader's.  It does not matter which wins, because
bin/ovms resolves libOpenCL.so.1 through its own RUNPATH and never looks at the
profile.

Deduped with eq?, which is reliable for packages: two instances both defaulting
to the module-level `ovms' variable really are the same object, and the two
unconditional python packages are likewise single module-level bindings however
many instances there are.  Dedup happens after the filter, so instances that opt
out contribute no `ovms' entry even when another instance opts in with the same
package."
  (delete-duplicates
   (append
    ;; python-jinja2 and python-numpy UNCONDITIONALLY, and this is not the
    ;; oneTBB case above: both are pure Python with no shared libraries, so
    ;; merging them cannot collide with anything.  OVMS's embedded interpreter
    ;; needs jinja2 to apply the served model's chat template and numpy for the
    ;; bundled openvino python package; NEITHER is in $out/lib/python (see
    ;; ovms-profile-site-packages), and the vendor's own requirements.txt lists
    ;; numpy as the caller's responsibility.  Declaring them here means neither
    ;; dependency rests on some unrelated package happening to pull it into this
    ;; host's profile — which is exactly how jinja2 worked by accident during
    ;; interactive testing and then failed silently under shepherd, taking every
    ;; /chat/completions request with it.
    (list python-jinja2 python-numpy)
    (map (lambda (config)
           (ovms-server-configuration-package config))
         (filter (lambda (config)
                   (ovms-server-configuration-add-to-profile? config))
                 (ovms-configs config-or-configs))))
   eq?))

(define (ovms-instance-activation config)
  "One instance's activation, as its own gexp so ovms-activation can splice it.

Every value is computed as a *plain string* BEFORE building the gexp.  The gexp
is spliced via #$@ into ovms-activation's gexp; if it referenced let-bindings
from *this* function's scope, Guix would try to resolve those bindings in the
ovms-activation gexp's meta environment, where they are out of scope, and fall
through to a top-level module variable — which fails with \"unbound variable\" at
OS-build time.  The log *directory* is likewise dirname'd here rather than
inside the gexp — not because dirname would be unavailable there (activation
scripts run with `(gnu build activation)' and `(guix build utils)' in scope:
gnu/services.scm's activation-service->script wraps each action in
with-imported-modules over (source-module-closure '((gnu build activation)
(guix build utils))) plus a matching top-level use-modules, and the gexp below
adds its own use-modules as well) — but because every input is already a plain
string at meta time, so computing it here keeps the builder minimal.

The model directory is deliberately NOT created.  It is a large out-of-band
artifact and conjuring an empty one would only convert a loud \"path does not
exist\" into a quieter \"no versions found\" later; the failure should stay
obvious.

The one exception is the OpenCL vendors directory, which IS created here: it is a
synthesised directory of one symlink that OCL_ICD_VENDORS points at, so it has to
exist and be correct before the shepherd constructor runs.  See
ovms-resolved-opencl-icd-dir and the \"A DIRECTORY OF ONE\" comment in
ovms-environment.  Activation is the right place rather than the constructor
because it is idempotent, runs once per reconfigure, and a failure here is a
loud build/activation error instead of a silent \"no GPU\" at model load.  What
keeps that last promise is the explicit file-exists? guard below:
switch-symlinks given a non-existent target succeeds silently and leaves a
DANGLING symlink (measured), which is exactly the documented prerequisite
failure — `intel-opencl-icd' absent from the system profile → zero Intel
GPUs, no error message."
  (let* ((user (ovms-server-configuration-user config))
         (state-dir (ovms-resolved-state-dir config))
         (log-file (ovms-resolved-log-file config))
         (log-dir (dirname log-file))
         (icd-dir (ovms-resolved-opencl-icd-dir config))
         ;; Left as the field's own value — a plain string by default, but a
         ;; file-like object is also legal and #$ resolves it here.
         (icd-file (ovms-server-configuration-opencl-icd config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw (getpwnam #$user))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          ;; NOTE: /var/log/ovms is SHARED by every default-configured
          ;; instance, so this loop creates and chowns it once per instance —
          ;; harmless with a single service user, last-writer-wins on the
          ;; directory's ownership if two instances ever declare different
          ;; users.  Shepherd still creates each instance's log *file*
          ;; itself, so the race only affects who may add further files
          ;; directly under the shared directory.
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    (list #$state-dir
                          #$log-dir
                          #$icd-dir))
          ;; The vendors directory must contain exactly one `.icd' entry, named
          ;; after the source file so the loader's `*.icd' scan picks it up.  The
          ;; basename is taken HERE rather than in ovms-resolved-opencl-icd-link
          ;; because #$icd-file may be a file-like object at meta time and only
          ;; becomes a plain string inside the builder.
          ;;
          ;; GUARD FIRST — loud, not silent: switch-symlinks with a
          ;; non-existent target returns successfully and leaves a DANGLING
          ;; symlink (measured).  Without this guard, a missing
          ;; `intel-opencl-icd' in the system profile would convert into
          ;; exactly the silent failure this activation is supposed to
          ;; prevent: zero Intel GPUs, no error message.  file-exists?
          ;; follows symlinks, so a valid
          ;; <profile>/etc/OpenCL/vendors/intel.icd (itself a symlink into
          ;; the store) passes and a missing one fails loudly here.
          (unless (file-exists? #$icd-file)
            (error
             (string-append
              "ovms activation: the OpenCL ICD " #$icd-file " does not "
              "exist.  `intel-opencl-icd' must be in the system profile via "
              "#:extra-packages (see peteches/systems/comfyui.scm); without "
              "it the GPU path enumerates zero Intel devices.")))
          ;;
          ;; switch-symlinks (a plain procedure from (guix build utils),
          ;; which this gexp imports at its top) is used instead of a
          ;; hand-rolled delete-file/symlink pair on purpose: it is
          ;; documented to work "both when LINK already exists and when it
          ;; does not", it copes with a *dangling* stale link (which
          ;; `file-exists?' reports as absent, so a guarded delete would
          ;; leave it and the following symlink would die with EEXIST), and
          ;; it swaps via a pivot + rename so a reconfigure never leaves the
          ;; directory empty.  The obvious alternative,
          ;; `(false-if-exception (delete-file …))', would also work —
          ;; false-if-exception is a macro from (ice-9 boot-9), and boot-9
          ;; is unconditionally in scope in every Guile module, gexp
          ;; builders included (peteches/packages/ovms.scm uses it inside a
          ;; phase body and builds fine) — but switch-symlinks's documented
          ;; behaviour already covers the dangling-link and atomicity
          ;; cases.
          (switch-symlinks (string-append #$icd-dir "/"
                                          (basename #$icd-file))
                           #$icd-file)))))

(define (ovms-activation config-or-configs)
  #~(begin
      (use-modules (guix build utils))
      #$@(map ovms-instance-activation (ovms-configs config-or-configs))))

(define (ovms-shepherd-service config)
  "CONFIG's shepherd service, as a one-element list.

Every record access happens in the let* below, outside the gexp:
define-record-type* makes its accessors macros, and a macro is not in scope
inside a gexp body (which is expanded in the builder's module environment).
Capturing the values here and splicing them with #$ is exactly what the deleted
openvino-shepherd-service did, and what every gexp-building service in this repo
does."
  (let* ((service-name (ovms-server-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (pkg (ovms-server-configuration-package config))
         (ovms-path (file-append pkg "/bin/ovms"))
         (args (ovms-serve-args config))
         (user (ovms-server-configuration-user config))
         (group (ovms-server-configuration-group config))
         (log-file (ovms-resolved-log-file config))
         (env (ovms-environment config)))
    (list (shepherd-service
           (provision (list provision-name))
           (documentation (ovms-server-configuration-documentation config))
           (requirement '(networking file-systems))
           (auto-start? (ovms-server-configuration-auto-start? config))
           (start #~(make-forkexec-constructor
                     (list #$ovms-path #$@args)
                     #:user #$user
                     #:group #$group
                     #:log-file #$log-file
                     #:environment-variables (list #$@env)))
           (stop #~(make-kill-destructor))))))

(define (ovms-shepherd-services config-or-configs)
  (append-map ovms-shepherd-service (ovms-configs config-or-configs)))

(define (ovms-firewall-rules config-or-configs)
  "nftables input rules for every instance that asked to be reachable.

Nothing opens unless asked: the default host is loopback, and this VM's base
policy is drop-input, so a silent \"tcp dport … accept\" here would be a security
change disguised as a convenience.

RAISES at configuration-evaluation time on the one combination that used to be
expressible by accident: open-firewall? #t + api-key-file #f + empty
firewall-sources.  That compiled into a bare `tcp dport 8094 accept' reachable
from every host on the 192.168.50.0/23 LAN, while OVMS itself logged `API key not
provided via --api_key_file or API_KEY environment variable.  Authentication will
be disabled.' — i.e. an unauthenticated GPU inference endpoint on the LAN, which
is a real cost (anyone can occupy the card and OOM the ComfyUI tenant next door)
and not merely a policy smell.  Narrowing to a set of CIDRs is always acceptable,
so instances that authenticate through the API_KEY environment variable instead of
api-key-file satisfy the guard by listing their sources — which they should be
doing anyway.

When it is asked for and passes the guard, `firewall-sources' narrows the accept
to those CIDRs, emitting one rule per entry — `ip saddr' for v4 and `ip6 saddr'
for v6, because the chain lives in an inet table.  Same shape as
strata-firewall-rules (and the deleted openvino-firewall-rules), for the same
reason: all of them are unauthenticated GPU endpoints that only the tailnet
should reach.

Note only `port' (REST) is ever opened, never grpc-port — see that field's NIT."
  (let ((configs (ovms-configs config-or-configs)))
    ;; See %ovms-service-name-chars for why this is a whitelist and why the
    ;; empty string is the case that actually matters.  Runs as its own pass
    ;; over every instance, before any rule is emitted, so a bad name on the
    ;; second instance is still caught.
    (for-each
     (lambda (config)
       (let ((name (ovms-server-configuration-service-name config)))
         (when (or (string-null? name)
                   (string-any (lambda (c)
                                 (not (string-index %ovms-service-name-chars c)))
                               name))
           (error
            "ovms: service-name must be non-empty and contain only ASCII \
letters, digits, dot, underscore or hyphen (it is spliced verbatim into an nft \
comment and into the provision symbol, log file name and state directory)"
            name))))
     configs)
    ;; Checked in its own pass first, and offenders are COLLECTED so a host
    ;; with several bad instances reports every offending one in a single
    ;; error; a for-each that errors per instance would throw on the first
    ;; and hide the rest.
    (let ((offenders
           (filter (lambda (config)
                     (and (ovms-server-configuration-open-firewall? config)
                          (not (ovms-server-configuration-api-key-file config))
                          (null? (ovms-server-configuration-firewall-sources
                                  config))))
                   configs)))
      (unless (null? offenders)
        (error
         (string-append
          "ovms: refusing to expose unauthenticated endpoint(s) to the whole "
          "network: "
          (string-join
           (map (lambda (config)
                  (string-append
                   "'" (ovms-server-configuration-service-name config)
                   "' (port "
                   (number->string
                    (ovms-server-configuration-port config))
                   ")"))
                offenders)
           ", ")
          ".  Each of these has open-firewall? #t, api-key-file #f and "
          "empty firewall-sources, which compiles to a bare `tcp dport "
          "<port> accept' while OVMS logs `Authentication will be "
          "disabled.'  Change ONE of the three per instance: set "
          "firewall-sources to the CIDRs that may reach it (for a caddy or "
          "tailnet client, the same three prefixes the comfyui ovms instance "
          "uses -- 100.64.0.0/10, fd7a:115c:a1e0::/48, "
          "fd7a:115c:a6e0::/48); or set api-key-file to a file of accepted "
          "keys; or leave open-firewall? #f and keep the endpoint on "
          "loopback."))))
    (nftables-rules
     (input (append-map (lambda (config)
                          (if (not (ovms-server-configuration-open-firewall? config))
                              '()
                              (let ((rule-suffix
                                     (string-append "tcp dport "
                                                    (number->string
                                                     (ovms-server-configuration-port
                                                      config))
                                                    " accept comment \""
                                                    (ovms-server-configuration-service-name
                                                     config) "\""))
                                    (sources
                                     (ovms-server-configuration-firewall-sources
                                      config)))
                                (if (null? sources)
                                    (list rule-suffix)
                                    (map (lambda (cidr)
                                           (string-append
                                            (if (string-index cidr #\:)
                                                "ip6 saddr " "ip saddr ")
                                            cidr " " rule-suffix))
                                         sources)))))
                        configs)))))

;;; ── Service type ─────────────────────────────────────────────────────────

;; Exported through the module's #:export clause alone — one export mechanism
;; per module.  peteches/packages/ovms.scm's header argues against exactly the
;; double declaration (both #:export and define-public) that the deleted
;; peteches/services/openvino.scm carried; this file uses #:export throughout,
;; including for the record bindings above.
(define ovms-service-type
  (service-type (name 'ovms)
                (description
                 ;; Splices %ovms-version so the package module stays the
                 ;; single source of truth for the pin.
                 (string-append
                  "Serve an OpenVINO IR model with Intel OpenVINO Model "
                  "Server " %ovms-version " (OpenAI-compatible REST)."))
                (extensions (list (service-extension account-service-type
                                                     ovms-accounts)
                                  (service-extension activation-service-type
                                                     ovms-activation)
                                  (service-extension shepherd-root-service-type
                                                     ovms-shepherd-services)
                                  (service-extension profile-service-type
                                                     ovms-packages)
                                  (service-extension firewall-service-type
                                                     ovms-firewall-rules)))))
