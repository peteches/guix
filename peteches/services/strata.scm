;; peteches/services/strata.scm — Strata MoE inference engine (system).
;;
;; System service for the Strata inference engine
;; (github.com/Niko1221/Strata), targeting the Intel Arc B70 (Battlemage)
;; via its SYCL/oneAPI backend — NOT the CUDA path (that is the RTX 4090's
;; koboldcpp.scm).  The `strata` package carries the self-contained
;; Terminal bundle (relocatable launcher + private CPython); the Intel
;; engine package (libsycl, Level Zero runtime) is fetched by Strata itself
;; on first launch into XDG_DATA_HOME/strata/components, so it is NOT a
;; guix input — only the engine's *system* library dependencies are:
;;
;;   libze_loader.so.1  upstream guix `level-zero`
;;   libOpenCL.so.1     upstream guix `pocl`
;;   libstdc++/libgcc   `gcc`
;;   libc/libm/ld-linux `glibc`
;;   libz.so.1          `zlib`
;;
;; Two pure-Guix-system blockers are handled here (see the package file for
;; the build side):
;;
;;   1. PT_INTERP — the bundle's CPython and the downloaded engine binary
;;      both carry PT_INTERP = /lib64/ld-linux-x86-64.so.2, an FHS path
;;      that does not exist on a Guix system.  The /lib64 -> glibc/lib
;;      symlink is created by an activation service declared in the system
;;      file (comfyui.scm), not here.
;;
;;   2. Runtime libs — a non-store binary (the downloaded engine) cannot
;;      use Guix's per-package ld.so.cache (it is looked up relative to the
;;      executable), so the wrapper exports LD_LIBRARY_PATH over the store
;;      lib dirs of the packages in `runtime-lib-packages`.
;;
;; Shepherd integration: EVERY Strata launch path daemonizes — `launch
;; --no-tui` "serves, prints the address and returns", and there is no
;; foreground mode.  So the tracked process is a long-running wrapper
;; (written at activation) that (a) starts the daemon, (b) polls the
;; daemon's runtime state file (XDG_STATE_HOME/strata/runtime/launch-*.json
;; — present from daemon start until stop, independent of whether the port
;; is bound yet), and (c) traps TERM to run `launch --stop` in the
;; background (the kill destructor's TERM->KILL window is shorter than a
;; full stop; the stop is idempotent and the daemon's host lease recovers
;; stale state).
;;
;; auto-start? defaults to #f: the first launch downloads the engine
;; (~186 MB) and the model (tens of GB) and maps the experts into RAM —
;; booting into that contends with koboldcpp (which auto-starts on the
;; 4090) and ComfyUI.  Start it explicitly with `herd start strata`.
(define-module (peteches services strata)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages oneapi)
  #:use-module (gnu packages compression)
  #:use-module (peteches packages strata)
  #:use-module (peteches services firewall)
  ;; The record constructor is a macro, so naming it in #:export does not make
  ;; the field accessors visible to a consumer that merely
  ;; (resolve-interface '(peteches services strata)) — they are only exported by
  ;; the define-record-type* form itself, which puts them on the module's export
  ;; list.  Listing them explicitly here is what lets `guix build -e` and any
  ;; other consumer read the reasoning defaults; without it they come back
  ;; "Unbound variable".
  ;;  The accessors below are named as well as constructed for the same reason
  ;;  the reasoning ones are: a consumer that only (resolve-interface)s this
  ;;  module cannot see accessors that came from define-record-type* unless
  ;;  they are on this module's export list.
  #:export (strata-configuration strata-configuration?
            strata-configuration-reasoning-budget-tokens
            strata-configuration-reasoning-effort
            strata-configuration-answer-room?
            strata-configuration-repetition-penalty
            strata-configuration-temperature
            strata-configuration-top-p
            strata-configuration-top-k
            strata-configuration-min-p
            strata-configuration-seed
            strata-configuration-presence-penalty
            strata-configuration-frequency-penalty
            strata-configuration-penalty-last-n
            strata-configuration-repeat-stop-tokens
            strata-configuration-conversation-cache-mib
            strata-configuration-conversation-cache-slots
            strata-configuration-conversation-cache-min-free-mib
            strata-configuration-batch
            strata-configuration-batch-mtp?
            strata-configuration-serve-args
            strata-service-type))

;;; ── Utilities ────────────────────────────────────────────────────────────

(define (maybe-option flag value)
  (if value
      (list flag (if (number? value) (number->string value) value))
      '()))

;; One element of the native run config's "args" array, ready to be joined
;; with "\n": the flag and its value quoted, with the comma a NON-last element
;; needs.  #f emits nothing, i.e. omit the flag and let the engine keep its own
;; default for it.
(define (engine-arg-line flag value)
  (if value
      (list (string-append "    \"" flag "\", \""
                           (if (number? value) (number->string value) value)
                           "\","))
      '()))

;; The same, for a flag that takes NO value ("--batch-mtp", "--batch-telemetry"
;; on the engine side). It still carries the comma, because every element of
;; "args" that is not the last one needs it.
(define (engine-flag-line flag)
  (list (string-append "    \"" flag "\",")))

;; One "key": value entry for the run config's `sampling` block (the object
;; the engine merges over the model-card sampling, server.py:6200), ready to
;; be joined with ", ".  #f emits nothing, i.e. omit the key and let the model
;; card (server.py MODEL_CARD_SAMPLING) keep its default for it.  Called from
;; the sampling-json local at gexp elaboration, so the record accessors that
;; feed it are in scope - the same constraint as every other *-json local in
;; strata-native-wrapper-script.  A number is rendered with number->string, so
;; an integer field (top_k, penalty_last_n, seed) must be given an integer,
;; not a float, or the engine's sampling_defaults_from_config rejects it.
(define (sampling-entry key value)
  (if value
      (list (string-append "\"" key "\": "
                           (if (number? value) (number->string value) value)))
      '()))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <strata-configuration> strata-configuration
                     make-strata-configuration
  strata-configuration?
  (package strata-configuration-package
           (default strata))
  ;; Serve this package's engine binary INSTEAD of the upstream engine the
  ;; bundle would download: the giridhart-fork SYCL engine
  ;; (strata-sycl-engine in (peteches packages strata)) with the
  ;; non-finite-logits guard that fixes the single-token `!' loop (#606,
  ;; #5160).  When set (native-model path only), the run config's "exe"
  ;; points directly at the fork's entrypoint in the store — an absolute
  ;; path, not a component: reference — and the bundle-version engine
  ;; fetch guard is skipped.  #f — the default — keeps the stock
  ;; download-the-matching-engine behaviour.
  (sycl-engine strata-configuration-sycl-engine
               (default #f))
  ;; The engine's system-library dependencies; each contributes
  ;; <store-path>/lib to the wrapper's LD_LIBRARY_PATH.
  (runtime-lib-packages strata-configuration-runtime-lib-packages
                        (default (list level-zero pocl glibc gcc zlib)))
  (service-name strata-configuration-service-name
                (default "strata"))
  (documentation strata-configuration-documentation
                 (default "Strata MoE inference engine (Intel Arc SYCL)."))
  (auto-start? strata-configuration-auto-start?
               (default #f))

  (create-account? strata-configuration-create-account?
                   (default #t))
  (user strata-configuration-user
        (default "strata"))
  (group strata-configuration-group
         (default "strata"))
  ;; /dev/dri access: the kernel nodes are root:video, and Strata's own
  ;; doctor checks for `render` membership — the service account needs
  ;; both.  The `render` group does not exist on the VM by default; it is
  ;; added by the account extension below.
  (supplementary-groups strata-configuration-supplementary-groups
                        (default '("render" "video")))

  ;; `strata launch` model selection: --family qwen --model q2_0
  ;; (Qwen3.8-Flash-Next, Q2_0 quant — the first quant that fits in the
  ;; expanded 48 GB RAM; see docs/MODELS.md's "fits when RAM is at least
  ;; experts + ~10 GB").
  (family strata-configuration-family
          (default "qwen"))
  (model strata-configuration-model
         (default "q2_0"))
  ;; Where Strata stores downloaded model files (the big NVMe, not /).
  (data-dir strata-configuration-data-dir
            (default "/media/models/strata"))
  ;; --access local binds 127.0.0.1 (lan/cloudflare/tailscale for more).
  (access strata-configuration-access
          (default "local"))
  (port strata-configuration-port
        (default 8000))
  ;; Open this port in the host nftables firewall.  Needed when serving
  ;; beyond localhost (e.g. --access tailscale): the comfyui VM's base
  ;; firewall is drop-by-default, so Caddy's strata.ts.peteches.co.uk
  ;; reverse proxy (peteches/systems/caddy.scm) cannot reach the engine
  ;; over Tailscale unless the port is accepted here.  Same pattern as
  ;; koboldcpp's open-firewall? (peteches/services/koboldcpp.scm).
  (open-firewall? strata-configuration-open-firewall?
                  (default #f))
  ;; Source CIDRs the open-firewall? accept rules are restricted to — one
  ;; nft rule per CIDR (`ip saddr' / `ip6 saddr' chosen per family; the
  ;; chain is inet, so both work).  '() — the default — keeps the
  ;; historical unrestricted accept.  The engine has no API key by
  ;; default, so an unrestricted accept hands every host that can route
  ;; to the port a keyless GPU and a way to starve the parked
  ;; conversations; a fleet whose consumers all arrive over the tailnet
  ;; (caddy's reverse proxy) can say so here.  Loopback is unaffected:
  ;; the base firewall accepts `iifname "lo"' before any of this.
  (firewall-sources strata-configuration-firewall-sources
                    (default '()))
  (context strata-configuration-context
           (default #f))
  ;; KV-cache quant.  Catalog path: the --kv flag (omit on #f, engine
  ;; default).  Native path: the run config's "--kv" arg, defaulting to
  ;; q4_0 when unset — the 262k-context VRAM math in comfyui.scm's
  ;; comment assumes q4_0 (~6 KB/token for the hybrid GDN layout).
  (kv strata-configuration-kv
      (default #f))
  ;; Native path only.  MTP draft length: the engine proposes `spec'
  ;; tokens per verify window and the target keeps the prefix it agrees
  ;; with, so decode throughput scales with the acceptance rate.  The
  ;; default 2 is what this service has always run; the fork's own B70
  ;; setup default is 4 with a 0.5 confidence floor (docs/INTEL.md:
  ;; "--spec 6 against --spec 4 is +6.5% on code and -1.5% on prose, so
  ;; the setup default (4, 0.5) stays").
  (spec strata-configuration-spec
        (default 2))
  ;; --spec-min-p: drafts below this model probability are not proposed
  ;; (higher acceptance, fewer wasted verify slots).  #f omits the flag.
  (spec-min-p strata-configuration-spec-min-p
              (default #f))
  ;; --prefill chunk tokens.  #f renders "auto".  INTEL.md: setup writes
  ;; 4096 for cards >= 24 GB and recommends 8192 for a Battlemage card
  ;; above 32K context (the chunk borrows expert-cache slots and refills
  ;; them after the prompt, ~1 s).
  (prefill strata-configuration-prefill
           (default #f))
  ;; --vram-reserve-mib: headroom the engine leaves free instead of
  ;; sizing the expert cache to the last mebibyte.  #f omits the flag
  ;; (engine default); the fork's B70 recipe uses 1024-2048 — without a
  ;; reserve the auto tier first tries a cache the card cannot actually
  ;; back ("malloc_device(31.03 GiB) failed") and retries smaller.
  (vram-reserve-mib strata-configuration-vram-reserve-mib
                    (default #f))
  ;; Native path only: KEY . VALUE pairs the wrapper exports before
  ;; exec'ing the server; the engine child inherits them (the same path
  ;; the NEO_* and STRATA_SSD_GBPS exports take).  Values are emitted
  ;; unquoted so they may reference earlier exports ($XDG_CACHE_HOME)
  ;; but must not contain spaces.  The fork engine's documented runtime
  ;; contract lives here — its own launcher (sycl/serve/strata-sycl.sh)
  ;; sets STRATA_VERIFY_DEVICE_PLAN=1, STRATA_VERIFY_NO_HOST=1 and
  ;; STRATA_STAGER_THREADS=12, and INTEL.md flags VERIFY_NO_HOST as
  ;; REQUIRED on xe whenever part of the experts is not in VRAM
  ;; ("the logits turn NaN a few tokens in" without it).
  (engine-env strata-configuration-engine-env
              (default '()))
  (vision strata-configuration-vision
          (default #f))
  (parallel strata-configuration-parallel
            (default #f))
  (extra-args strata-configuration-extra-args
              (default '()))
  ;; Batch decode windows for the engine ("--batch N", 2..8 normally). #f - what
  ;; every host runs today - is the right setting for a single stream: on engine
  ;; 0.1.57 aggregate decode on this B70 is flat in concurrency (measured 65.4
  ;; tok/s at 2 concurrent requests, 65.2 at 4, first token 0.11-0.77 s), so
  ;; lanes now buy FAIRNESS, not throughput. Set it when several conversations
  ;; genuinely decode at once, as pi's parallel sub-agents on agents do.
  ;; docs/strata-subagent-throughput.org §6 has the measured arm table.
  (batch strata-configuration-batch
         (default #f))
  ;; "--batch-mtp": each slot also verifies one MTP proposal per window.
  ;; Defaults ON because --batch WITHOUT it measured -59% on this card: the
  ;; slots then carry no proposal, every window decodes a single token, and
  ;; parking degrades ("conversation cache: slot 0 not parked before its
  ;; overwrite"). Its marginal cost is small - ~198 MiB of VRAM per slot
  ;; measured, against 2.25 GiB of session K/V per slot - and it is what turned
  ;; 2 lanes into +33%. It needs --mtp and --spec, which the native path always
  ;; passes, so it is silently omitted for a model with no MTP runtime.
  (batch-mtp? strata-configuration-batch-mtp?
              (default #t))
  ;; Extra flags for `strata serve' ITSELF (see serve --help), not for the
  ;; engine's "args" array: e.g. '("--queue-timeout" "300" "--max-queue" "12").
  ;; `--parallel' is emitted from the `parallel' field and `--batch-telemetry'
  ;; from `batch', so keep those out of this list. Two constraints learned the
  ;; hard way (verified live 2026-11-06): `--queue-timeout' on its own makes
  ;; serve refuse to start with "queue_timeout requires max_queue to bound
  ;; pending calls", so it must always come with `--max-queue'; and `--parallel'
  ;; really is honoured — /v1/status reports it as
  ;; "concurrency": {"serving": N, "requested": N} — so asking for more lanes
  ;; than the card can hold is visible there rather than silent.
  (serve-args strata-configuration-serve-args
              (default '()))

  ;; What the engine should assume the MODEL volume reads at, in GB/s, when it
  ;; plans which files a prompt chunk has to re-read.  Unset, strata derives it
  ;; from the device flag: `drive_read_gbps()` (src/program/generate.cpp:1477)
  ;; prices a disk whose /sys/block/<dev>/queue/rotational is 1 at 0.15 GB/s,
  ;; an unrotational device named nvme* at 2.5, and any other unrotational name
  ;; at 0.45 ("is a SATA-class SSD (assumed)").  That flag is a problem for VMs:
  ;; virtio-blk has nothing to say — QEMU's `virtio-blk-pci` has no
  ;; rotation_rate property (scsi-hd and ide-hd do) and the virtio-blk config
  ;; space has no rotation field — so every virtio disk in a guest comes up
  ;; rotational, whatever the host pool it sits on really is.  Setting this
  ;; exports STRATA_SSD_GBPS, which drive_read_gbps() reads before it ever
  ;; looks at sysfs, so the planner gets the measured number instead of a
  ;; platter guess.  #f exports 0 = "not set" = detection as before.
  (ssd-gbps strata-configuration-ssd-gbps
            (default #f))

  ;; ── Conversation parking ───────────────────────────────────────────────
  ;; `--conversation-cache-mib` is the switch.  Like `api-key`/`host`/
  ;; `allowed-hosts` these are NATIVE-MODE ONLY, and unlike the three
  ;; reasoning keys they go INSIDE the run config's "args": they are engine
  ;; options, and this is the SYCL engine binary's own help text (0.1.52,
  ;; read off the component this VM downloaded):
  ;;
  ;;   --conversation-cache-mib N           --serve: RAM budget for parked
  ;;                                        conversations (default 0 = off)
  ;;   --conversation-cache-slots N         --serve: at most N parked
  ;;                                        conversations (default 4)
  ;;   --conversation-cache-min-free-mib N  --serve: physical RAM floor when
  ;;                                        parking or restoring a session
  ;;                                        file (default 2560)
  ;;
  ;; #f for `mib` omits all three lines, and that is what "disabled" has
  ;; always meant here: the Monitor's "Conversation cache" card is derived,
  ;; not configured — serve/server.py:834 `conversation_cache_view()` reports
  ;; `enabled` only when the engine's INFO line carries
  ;; `conversation_cache_mib` > 0 — and serve/web/app.js:404's "add
  ;; --conversation-cache-mib 8192" is setup.py's `AGENT_CACHE_MIB`, sized for
  ;; its `AGENT_CACHE_FREE_GB = 24` GB of RAM free beside the model.
  ;;
  ;; What parking buys, and what it does not: `--prompt-cache` is never passed
  ;; here, so its default (6 checkpoints, "~118 MB of RAM each") already keeps
  ;; the LAST conversation's state between requests — single-chat reuse is
  ;; already working, and this deployment's own /metrics said so: 11,863,657
  ;; of 12,591,483 prompt tokens (94.2%) were prefix-cache hits over 179
  ;; requests.  Parking is what lets SEVERAL conversations coexist, and that
  ;; is the case here: rules-lawyer (peteches/systems/agents.scm), pi
  ;; (configs/pi/defaults/models.json) and SillyTavern all hit one batch=1
  ;; engine, so whoever spoke last owns the resident state and every switch
  ;; costs a full re-read + re-prefill.  `slots` also feeds
  ;; serve/server.py:4057, which sizes the prompt re-encoder's `keep` from
  ;; `--conversation-cache-slots`.
  ;;
  ;; Sizing: only 12 of this model's 48 layers are full attention (see the
  ;; context-size note in peteches/systems/comfyui.scm), so at --kv q4_0 the
  ;; per-token cost is ~6 KB and the recurrent layers hold fixed-size state —
  ;; 70k tokens ≈ 420 MB, 131k ≈ 800 MB, the full 262k window ≈ 1.5 GB, so 5
  ;; conversations ≈ 3-4 GiB plus one transient copy per park/restore (that
  ;; transient copy is exactly what `min-free-mib` guards).  The floor is the
  ;; knob to get right, because --mmap-experts is always passed and the expert
  ;; tiers then depend on the guest's page cache: a park that eats the last of
  ;; that cache re-reads experts off the NVMe.  "conversation cache: skipped"
  ;; in the service log means the floor is above the RAM actually free
  ;; (serve/server.py:768 parses exactly that line).
  (conversation-cache-mib strata-configuration-conversation-cache-mib
                          (default #f))
  (conversation-cache-slots strata-configuration-conversation-cache-slots
                            ;; #f: omit, engine default 4.  Set it to at least
                            ;; the number of conversations that share the
                            ;; engine — one conversation more than the slots
                            ;; available evicts the oldest on every turn.
                            (default #f))
  (conversation-cache-min-free-mib
   strata-configuration-conversation-cache-min-free-mib
   ;; #f: omit, engine default 2560 MiB.  Raise it once parking is on, so a
   ;; park never leaves less page cache than the mmap'd experts need.
   (default #f))

  ;; Strata's per-user state (runtime/, tuning/, profiles/, components/
  ;; — the engine download).  Kept off /var to survive a /var rebuild and
  ;; to keep the big engine package out of the root fs.
  (state-dir strata-configuration-state-dir
             (default "/var/lib/strata"))
  (log-file strata-configuration-log-file
            (default #f))
  ;; Native-pack mode: a manually-provisioned model that the catalog's
  ;; fingerprint and SHA-256 gates refuse (eddoursul/Strata issues #4087 and
  ;; #4160). A symbol naming a key into %native-models, or #f for the
  ;; catalog path. When set, `family`/`model` are ignored and the service
  ;; downloads the model's files, builds a native pack with the bundle's
  ;; tools/iq_pack.py, writes a run config, and starts the server in the
  ;; foreground with `strata serve` (see make-native-wrapper).
  (native-model strata-configuration-native-model
                (default #f))
  ;; The server's API key (native mode only). #f runs with no key ("none");
  ;; a string requires it on /v1/*. The caddy reverse proxy does not add
  ;; auth, so the client supplies it.
  (api-key strata-configuration-api-key
           (default #f))
  ;; The address the server listens on (native mode only). 0.0.0.0 makes it
  ;; reachable via the caddy tailnet proxy; 127.0.0.1 keeps it local.
  (host strata-configuration-host
        (default "0.0.0.0"))
  ;; The Host names the server answers to (native mode only), for the web UI's
  ;; DNS-rebinding protection: without an API key every request first passes a
  ;; Host check, and a name not in this list (or the bind host) is refused with
  ;; "Host ... is not allowed (DNS rebinding protection)". Caddy's tailnet proxy
  ;; reaches the engine under strata.ts.peteches.co.uk, so that name must be
  ;; listed here (or an API key set, which turns the check off). A list of host
  ;; names; the server also accepts suffixes (".example.org") and "*".
  (allowed-hosts strata-configuration-allowed-hosts
                 (default '()))

  ;; ── Reasoning / thinking budgets ───────────────────────────────────────
  ;; These three are *server-wide defaults*: every caller (SillyTavern, pi,
  ;; any OpenAI-compatible client) inherits them, and each can still override
  ;; per request.  Verified against strata 0.1.52's serve/server.py:
  ;;
  ;;   reasoning_budget_tokens — read as request > POST /settings > config
  ;;   (server.py:4279 `reasoning_budget`), so a request that names
  ;;   `reasoning_budget_tokens`/`thinking_budget_tokens` wins over this; 0 in
  ;;   a request means "no budget", which is how a client turns it off.
  ;;   reasoning_effort — the level for requests that name none (server.py:9031,
  ;;   `effort_default()` at 4620): POST /settings beats the config, and any
  ;;   request naming an effort/`enable_thinking` beats both.  The Qwen family
  ;;   table (serve/frontend.py EFFORT) accepts only none/low/medium/high —
  ;;   `high` renders the template's xhigh sentence — and anything else is a
  ;;   400, so pi's level map must land on one of those four words.
  ;;   answer_room — true (the upstream default) makes strata close thinking
  ;;   early enough to leave the answer a quarter of max_tokens, but ONLY when
  ;;   the request named no budget and max_tokens >= ANSWER_ROOM_MIN_CAP (1024,
  ;;   server.py:205/5253).  That is why requests arriving with max_tokens 1024
  ;;   got a 512-token thinking budget; false makes the configured budget apply
  ;;   as-is, so behaviour stops depending on what cap a client happened to send.
  (reasoning-budget-tokens strata-configuration-reasoning-budget-tokens
                           ;; #123: measured against this deployment's own log —
                           ;; 15402 thinking runs at budget 12288 never once
                           ;; saturated it (max observed usage 11566) and 75% of
                           ;; requests finished thinking in under 1k tokens, while
                           ;; 934 requests arrived with max_tokens >= 8192 against
                           ;; only 58 below it.  8192 is also the tier boundary in
                           ;; frontend.py's `budget_effort` (under 2K low, under 8K
                           ;; medium, else high) and sits inside strata's own
                           ;; reserve rule for pi's 16384-token cap.  Set it to #f
                           ;; to omit the key entirely and let strata guess.
                           (default 8192))
  (reasoning-effort strata-configuration-reasoning-effort
                    ;; #1641: "" is accepted and maps to no level, i.e. "inject
                    ;; no effort sentence" — the sentinel for a caller that wants
                    ;; budgets without prompt text.
                    (default "medium"))
  (answer-room? strata-configuration-answer-room?
                (default #f))
  ;; ── Sampling ───────────────────────────────────────────────────────────────
  ;; These are the run config's `sampling` block (self.sampling_defaults,
  ;; server.py:4775), which the engine merges OVER the model-card sampling
  ;; (server.py:6200 `defaults = {**model_card_sampling(...), **self.sampling_defaults,
  ;; **self.shared}`): a value set here overrides whatever the model card
  ;; (server.py MODEL_CARD_SAMPLING) chose for this arch, and a value left #f
  ;; keeps the card's default for that key.  The accepted keys and their
  ;; validation are server.py:9377 `sampling_defaults_from_config`:
  ;; temperature/top_p/top_k/min_p/seed numbers, presence_penalty and
  ;; frequency_penalty >= 0, penalty_last_n a non-negative integer, and
  ;; top_k/penalty_last_n/seed integers not floats; aliases
  ;; repeat_penalty->repetition_penalty and repeat_last_n->penalty_last_n
  ;; (server.py:9344).  The IQ2_XS model's thinking-mode card carries no
  ;; repetition penalty, which is what lets it fall into a single-token loop
  ;; (#606); repetition-penalty is the guard for that.  seed is a server-wide
  ;; default, so setting it makes every caller's generation deterministic.
  (repetition-penalty strata-configuration-repetition-penalty
                      (default #f))
  (temperature strata-configuration-temperature
               (default #f))
  (top-p strata-configuration-top-p
         (default #f))
  (top-k strata-configuration-top-k
         (default #f))
  (min-p strata-configuration-min-p
         (default #f))
  (seed strata-configuration-seed
        (default #f))
  (presence-penalty strata-configuration-presence-penalty
                    (default #f))
  (frequency-penalty strata-configuration-frequency-penalty
                     (default #f))
  (penalty-last-n strata-configuration-penalty-last-n
                  (default #f))
  ;; #606: a degenerate output (one token repeated to the limit, e.g. 256 '!'
  ;; mid-reply) is cut off by the engine's repeat_stop_tokens (default 256),
  ;; which caps the damage but does not prevent the loop.  This is a TOP-LEVEL
  ;; run-config key (a sibling of `sampling`), not part of the sampling block;
  ;; it shortens the safety net so a caller sees less of a loop if one happens.
  (repeat-stop-tokens strata-configuration-repeat-stop-tokens
                      (default #f)))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (strata-resolved-log-file config)
  (or (strata-configuration-log-file config)
      (string-append "/var/log/strata/"
                     (strata-configuration-service-name config) ".log")))

;;; ── Native-pack models ───────────────────────────────────────────────────
;; A manually-provisioned model the catalog refuses: the SC117 abliterated
;; GGUFs are a modified Qwen3.8-Flash-Next, so the catalog's fingerprint
;; gate (SUPPORTED_SELECTIONS, strata_control/models.py) and the SHA-256
;; gate (setup.py's verify_sha256, which runs even for own=True files) both
;; refuse them — the normal `strata launch --family/--model` path can never
;; install one (eddoursul/Strata issues #4087 and #4160; the opt-in
;; `--gguf-dir --unverified` flag requested in #4160 is not implemented as
;; of v0.2.6). The working path, demonstrated in #4160 at ~89-94 tok/s, is
;; to bypass setup entirely: download the files, build a native pack with
;; tools/iq_pack.py, write a run config that passes the files straight to
;; the engine (--native/--ple-gguf/--pack/--mtp/--expert-profile, with
;; --expert-cache auto), and start the server with `strata serve`. Each
;; entry below records exactly what to download (HF path + published size,
;; verified after fetch) and how to lay it out.
(define-record-type* <strata-native-model> strata-native-model
                     make-strata-native-model
  strata-native-model?
  (hf-repo strata-native-model-hf-repo (default #f))
  (model-files strata-native-model-model-files (default '()))
  (aux-files strata-native-model-aux-files (default '()))
  (pack-source strata-native-model-pack-source (default #f))
  (shard2 strata-native-model-shard2 (default #f))
  (mmproj strata-native-model-mmproj (default #f))
  (mtp-dir strata-native-model-mtp-dir (default #f))
  (slug strata-native-model-slug (default #f)))

;; model-files / aux-files are alists of (HF-relative-path . published-size):
;; every file is fetched from https://huggingface.co/<hf-repo>/resolve/main/
;; <path> and size-verified after the download (a partial or corrupt fetch
;; fails the start instead of building a bad pack). pack-source is shard 1
;; (the file tools/iq_pack.py indexes); shard2 is the --ple-gguf file; the
;; strata/rt/* files are the pre-built MTP runtime (--mtp points at their
;; directory); mmproj is the stock vision projector (the abliteration did
;; not touch the vision or MTP weights).
(define-public %native-models
  (list (cons 'sc117-iq2xs
          (strata-native-model
           (hf-repo "SC117/Qwen3.8-Flash-Next-GSQ-RCO-abliterated-GGUF")
           (model-files
            (list (cons "IQ2_XS/Qwen3.8-Flash-Next-GSQ-RCO-abliterated-IQ2_XS-00001-of-00002.gguf" 39665015072)
                  (cons "IQ2_XS/Qwen3.8-Flash-Next-GSQ-RCO-abliterated-IQ2_XS-00002-of-00002.gguf" 28800138432)))
           (aux-files
            (list (cons "strata/rt/dense.bin" 116099072)
                  (cons "strata/rt/dense.txt" 1880)
                  (cons "strata/rt/draft_vocab.bin" 425196)
                  (cons "strata/rt/experts.bin" 707788800)
                  (cons "mmproj-Qwen3.8-Flash-Next-BF16.gguf" 907542944)))
           (pack-source "IQ2_XS/Qwen3.8-Flash-Next-GSQ-RCO-abliterated-IQ2_XS-00001-of-00002.gguf")
           (shard2 "IQ2_XS/Qwen3.8-Flash-Next-GSQ-RCO-abliterated-IQ2_XS-00002-of-00002.gguf")
           (mmproj "mmproj-Qwen3.8-Flash-Next-BF16.gguf")
           (mtp-dir "strata/rt")
           (slug "sc117-iq2xs")))
         ;; The 3-bit IQ3_XXS of the same SC117 abliterated weights — the
         ;; durable fix for the 2-bit IQ2_XS single-token `!' loop (#606).
         ;; At 3 bpw the `!' token no longer dominates the sampling
         ;; distribution, so the loop is gone at the source and
         ;; repetition-penalty 1.2 (a bare nudge) is sufficient again.
         ;; Same hf-repo as sc117-iq2xs: the two IQ3_XXS shards are the only
         ;; new downloads (~71 GiB) and the native pack rebuilds from them.
         ;; strata/rt and mmproj are byte-identical to the IQ2_XS entry
         ;; (published at this repo's root, shared across quants — the MTP
         ;; runtime is tied to the MTP draft and the projector to the base
         ;; model, neither to the main quant), so they are reused, not
         ;; re-fetched. Experts are mmap'd, so the larger on-disk size costs
         ;; no extra memory: the profile-filled expert cache is memory-bound
         ;; and holds the hot experts regardless of quant. The higher quant's
         ;; only cost is a larger per-expert footprint (fewer cached experts,
         ;; slower decode) — the IQ3_XXS-vs-IQ3_S trade-off.
         (cons 'sc117-iq3xxs
          (strata-native-model
           (hf-repo "SC117/Qwen3.8-Flash-Next-GSQ-RCO-abliterated-GGUF")
           (model-files
            (list (cons "IQ3_XXS/Qwen3.8-Flash-Next-GSQ-RCO-abliterated-IQ3_XXS-00001-of-00002.gguf" 47342144896)
                  (cons "IQ3_XXS/Qwen3.8-Flash-Next-GSQ-RCO-abliterated-IQ3_XXS-00002-of-00002.gguf" 28800138432)))
           (aux-files
            (list (cons "strata/rt/dense.bin" 116099072)
                  (cons "strata/rt/dense.txt" 1880)
                  (cons "strata/rt/draft_vocab.bin" 425196)
                  (cons "strata/rt/experts.bin" 707788800)
                  (cons "mmproj-Qwen3.8-Flash-Next-BF16.gguf" 907542944)))
           (pack-source "IQ3_XXS/Qwen3.8-Flash-Next-GSQ-RCO-abliterated-IQ3_XXS-00001-of-00002.gguf")
           (shard2 "IQ3_XXS/Qwen3.8-Flash-Next-GSQ-RCO-abliterated-IQ3_XXS-00002-of-00002.gguf")
           (mmproj "mmproj-Qwen3.8-Flash-Next-BF16.gguf")
           (mtp-dir "strata/rt")
           (slug "sc117-iq3xxs")))
         ;; The upstream quantiser's own IQ2_XS of the same weights, WITHOUT
         ;; SC117's abliteration: ISTA-DASLab make the GSQ-RCO quants that
         ;; strata's catalog `qwen' family points at (setup.py's MODELS table,
         ;; "by": "Qwen; GSQ-RCO quants by ISTA-DASLab"), so this is the model
         ;; upstream actually ships, and it is 1.1% smaller than SC117's file
         ;; (39225954592 against 39665015072 for shard 1). Chosen 2026-11-06 as
         ;; the plan-recommended model for the agents workflow
         ;; (docs/strata-subagent-throughput.org); SC117's abliterated variant
         ;; stays as the rollback, because abliteration is what keeps the bot
         ;; from refusing, and only live traffic can show whether the base model
         ;; refuses enough to matter.
         ;;    Shard 2 is the n-gram/PLE table and is byte-identical between the
         ;; two repos: both publish 28800138432 bytes and the sc117 file hashed
         ;; 316b46f3a2dbd68c900f43136ab9449f9dcc3725dfd8c794847c204bc161e113 on
         ;; 2026-11-06, which is setup.py's pin for this quant tier. That is why
         ;; this entry costs one 36.5 GiB download and not two: comfyui hardlinks
         ;; shard 2, strata/rt and the tokenizer pack out of the sc117 tree.
         ;;    strata/rt is NOT fetchable from this repo — the HF tree API lists
         ;; 21 files and none of them is under strata/rt (verified 2026-11-06).
         ;; setup.py builds the MTP runtime itself with tools/mtp_fetch.py from
         ;; the original Qwen checkpoint, so the files listed here exist only
         ;; because fetch skips a file that is already present at the published
         ;; size. On a fresh host these four lines download nothing but a 404;
         ;; it needs the MTP runtime built by that tool (which is exactly what
         ;; upstream would have done anyway).
         ;;    The mmproj does differ: 907543008 bytes here against SC117's
         ;; 907542944, so the sc117 hard link is NOT reused and the wrapper
         ;; re-downloads the projector next start — 907 MB, and the right thing
         ;; to do rather than record a size this repo does not publish.
         (cons 'ista-iq2xs
          (strata-native-model
           (hf-repo "ISTA-DASLab/Qwen3.8-Flash-Next-GSQ-RCO-GGUF")
           (model-files
            (list (cons "IQ2_XS/Qwen3.8-Flash-Next-GSQ-RCO-IQ2_XS-00001-of-00002.gguf" 39225954592)
                  (cons "IQ2_XS/Qwen3.8-Flash-Next-GSQ-RCO-IQ2_XS-00002-of-00002.gguf" 28800138432)))
           (aux-files
            (list (cons "strata/rt/dense.bin" 116099072)
                  (cons "strata/rt/dense.txt" 1880)
                  (cons "strata/rt/draft_vocab.bin" 425196)
                  (cons "strata/rt/experts.bin" 707788800)
                  (cons "mmproj-Qwen3.8-Flash-Next-BF16.gguf" 907543008)))
           (pack-source "IQ2_XS/Qwen3.8-Flash-Next-GSQ-RCO-IQ2_XS-00001-of-00002.gguf")
           (shard2 "IQ2_XS/Qwen3.8-Flash-Next-GSQ-RCO-IQ2_XS-00002-of-00002.gguf")
           (mmproj "mmproj-Qwen3.8-Flash-Next-BF16.gguf")
           (mtp-dir "strata/rt")
           (slug "ista-iq2xs")))))

(define-public (strata-native-model-lookup symbol)
  "The strata-native-model record for SYMBOL (a key into %native-models), or
  an error naming the unknown key. Used to resolve the configuration's
  native-model field."
  (let ((entry (assoc symbol %native-models)))
    (if entry
        (cdr entry)
        (raise (make-error 'unknown-native-model
                (list symbol
                      (string-append "unknown native-model '"
                                     (symbol->string symbol) "' (known: "
                                     (string-join (map (lambda (e) (symbol->string (car e)))
                                                       %native-models) ", ")
                                     ")")))))))

;;; ── launch arguments ─────────────────────────────────────────────────────

;; The full argv after the binary: the `launch` subcommand first, then its
;; options.  Omitting `launch` makes strata's top-level parser consume the
;; first positional (the family name) as an unknown `command` and fail with
;; "invalid choice: 'qwen'" — every other token is an option to `launch`.
(define (strata-launch-args config)
  (append (list "launch"
                "--family" (strata-configuration-family config)
                "--model" (strata-configuration-model config)
                "--data-dir" (strata-configuration-data-dir config)
                "--access" (strata-configuration-access config)
                "--port" (number->string (strata-configuration-port config))
                "--no-tui"
                "--yes")
          (maybe-option "--context" (strata-configuration-context config))
          (maybe-option "--kv" (strata-configuration-kv config))
          (maybe-option "--vision" (strata-configuration-vision config))
          (maybe-option "--parallel" (strata-configuration-parallel config))
          (strata-configuration-extra-args config)))

;; Must be defined AFTER the record type: define-record-type*'s field accessors
;; are syntax transformers, so an earlier procedure body that calls one compiles
;; the identifier as a free-variable reference and fails at call time with
;; "Wrong type to apply: #<syntax-transformer ...>" — which is exactly what a
;; helper placed up beside maybe-option did when gexp-elaborated.
(define (strata-ssd-gbps-export config)
  ;; The STRATA_SSD_GBPS line both wrappers need (see the ssd-gbps field).
  ;; `exact->inexact` because number->string renders an exact rational as
  ;; "21/10", which strata's atof() would read as 21 (`inexact` is not bound
  ;; in this module's environment).  #f exports 0, and strata's own
  ;; `atof(e) > 0.0` guard treats 0 as "not set" — so the default
  ;; configuration keeps the engine's sysfs detection untouched.
  (let ((v (strata-configuration-ssd-gbps config)))
    (string-append "export STRATA_SSD_GBPS="
                   (if (number? v) (number->string (exact->inexact v)) "0"))))

;;; ── Wrapper script ───────────────────────────────────────────────────────
;; Written to <state-dir>/run-strata.sh at activation.  The store paths are
;; substituted at system-build time.

(define (strata-wrapper-script config)
  (let* ((pkg (strata-configuration-package config))
         (state-dir (strata-configuration-state-dir config))
         ;; <file-append> objects — resolved to store-path strings at gexp
         ;; expansion (activation time), NOT at compile time: a regular
         ;; function cannot string-join them (they are records, not strings),
         ;; and the store paths are not available when the gexp is compiled.
         (fa-list (map (lambda (p) (file-append p "/lib"))
                       (strata-configuration-runtime-lib-packages config)))
         (strata-fa (file-append pkg "/bin/strata"))
         ;; The launch command is a plain string (no store paths), so build it
         ;; at compile time and embed it as a literal (interpolating the args
         ;; list via #$ would splice it into the map expression).
         (launch-cmd (string-join
                      (cons "$STRATA" (map (lambda (a) (string-quote a))
                                           (strata-launch-args config)))
                      " ")))
    #~(string-join
        (list
         "#!/run/current-system/profile/bin/bash"
         "# Strata daemon wrapper — tracked by shepherd; see the module header."
         "set -u"
         "export PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin"
         ;; Every var below is EXPORTED: the tracked wrapper starts the daemon
         ;; via `launch`, and the daemon (and the engine it spawns) inherit
         ;; this environment.  Plain (non-exported) assignments would leave
         ;; the daemon with the shepherd account's defaults — which is exactly
         ;; what happened first: with HOME/XDG_STATE_HOME not exported, Strata
         ;; fell back to ~/.local/state instead of the intended state dir, and
         ;; with LD_LIBRARY_PATH not exported the engine could not find its
         ;; system libraries.  STRATA and RUNTIME_DIR are used by this script
         ;; too, but exporting them is harmless (export only marks them for
         ;; child processes).
         (string-append "export HOME=" #$state-dir)
         (string-append "export XDG_DATA_HOME=" #$state-dir "/share")
         (string-append "export XDG_STATE_HOME=" #$state-dir "/state")
         (string-append "export XDG_CONFIG_HOME=" #$state-dir "/config")
         (string-append "export LD_LIBRARY_PATH=/run/current-system/profile/lib")
         (string-append "export ZE_ICD_FILE=" #$state-dir "/ze_intel_gpu.icd.json")
         "export SYCL_CACHE_PERSISTENT=0"
         "export ZES_ENABLE_SYSMAN=1"
         ;; Force the Intel/SYCL engine: Strata's gpu_fit.pick_vendor() picks
         ;; NVIDIA first when the box has several GPU makes, and this VM also
         ;; passes through the RTX 4090 (in active use by koboldcpp).  Without
         ;; this override Strata downloads the CUDA engine and targets the
         ;; wrong card.  `sycl' is the bundle's own backend-override value
         ;; ("used in tests and by the support desk").
         "export STRATA_ENGINE_BACKEND=sycl"
         ;; Workaround for the Battlemage GuC firmware not reporting the
         ;; command-stream-receiver (preemption surface) size in the hwconfig
         ;; blob: the Intel NEO runtime computes requiredPreemptionSurfaceSize
         ;; from that (missing) field, gets 0, and aborts at device init with
         ;; UNRECOVERABLE_IF(preemptionSurfaceSize == 0) before the SIP kernel
         ;; can supply the real size.  Force a safe size via the documented
         ;; debug override (NEOReadDebugKeys=1 lets the runtime read these from
         ;; the environment).  64 MiB is well above the state the SIP kernel
         ;; needs and negligible against the B70's VRAM.
         "export NEOReadDebugKeys=1"
         "export NEO_L0_OverridePreemptionSurfaceSizeInMb=64"
         "export NEO_OverridePreemptionSurfaceSizeInMb=64"
         ;; What strata prices the model volume at for its prompt/expert tier
         ;; planning; 0 (the #f default) means "detect from sysfs", which is
         ;; what happened before this line existed.
         #$(strata-ssd-gbps-export config)
         (string-append "export STRATA=" #$strata-fa)
         (string-append "export RUNTIME_DIR=" #$state-dir "/state/strata/runtime")
         ""
         "stop_daemon() {"
         "  \"$STRATA\" launch --stop >/dev/null 2>&1 || true"
         "}"
         ""
         "on_term() {"
         "  # Background the stop and exit at once: the kill destructor's"
         "  # TERM->KILL window is shorter than a full stop, and the stop is"
         "  # idempotent (a stale host lease is recovered on the next start)."
         "  (stop_daemon) &"
         "  exit 0"
         "}"
         "trap on_term TERM"
         "trap on_term INT"
         ""
         #$launch-cmd
         "rc=$?"
         "if [ \"$rc\" -ne 0 ]; then"
         "  exit \"$rc\""
         "fi"
         ""
         "# Stay alive while the daemon's runtime state file exists (it is"
         "# created at daemon start and removed on stop; its presence is"
         "# independent of the port being bound, which lags during the"
         "# engine/model download and load)."
         "while true; do"
         "  if ! ls \"$RUNTIME_DIR\"/launch-*.json >/dev/null 2>&1; then"
         "    exit 1"
         "  fi"
         "  sleep 10"
         "done")
        "\n")))

(define (string-quote s)
  (string-append "\"" s "\""))

;;; ── Native-pack wrapper script ───────────────────────────────────────────
;; The native-mode counterpart of strata-wrapper-script: provision (download
;; + pack + config) then start the server in the foreground. The only gexp
;; interpolation is the bundle root (the strata package's store path); every
;; other path is a plain string (data-dir and state-dir are plain strings in
;; the configuration). See the native-models section for the rationale.
(define-public (strata-native-wrapper-script config model vision-reader)
  (let* ((pkg (strata-configuration-package config))
         (state-dir (strata-configuration-state-dir config))
         (data-dir (strata-configuration-data-dir config))
         (port (strata-configuration-port config))
         (host (strata-configuration-host config))
         (api-key (strata-configuration-api-key config))
         (allowed-hosts (strata-configuration-allowed-hosts config))
         (allowed-hosts-json
          (string-join (map string-quote allowed-hosts) ", "))
         (context (or (strata-configuration-context config) 32768))
         ;; The three reasoning config lines, precomputed HERE (at gexp
         ;; elaboration) into a single string and spliced into the gexp below
         ;; with #$reasoning-json.  They MUST NOT appear as free variables
         ;; inside the gexp: a gexp does not capture let-bound locals — the
         ;; gexp macro (guix/gexp.scm) only substitutes #$ (ungexp) references
         ;; and leaves every other identifier to be resolved where the gexp is
         ;; EVALUATED, i.e. at activation time in a nearly empty module
         ;; environment.  Bare `reasoning-budget` there is "Unbound variable",
         ;; the activation's guard swallows the error ("failed to activate"),
         ;; and with-output-to-file has already truncated run-strata.sh — so
         ;; the wrapper is left 0 bytes and shepherd respawns an empty script
         ;; that "exits successfully".  #f means "omit the key" (strata
         ;; guesses); answer-room? renders true/false and is never omitted.
         (reasoning-json
          (string-join
           (append
            (if (strata-configuration-reasoning-budget-tokens config)
                (list (string-append
                       "  \"reasoning_budget_tokens\": "
                       (number->string
                        (strata-configuration-reasoning-budget-tokens
                         config))
                       ",\n"))
                '())
            (if (strata-configuration-reasoning-effort config)
                (list (string-append
                       "  \"reasoning_effort\": "
                       (string-quote
                        (strata-configuration-reasoning-effort config))
                       ",\n"))
                '())
            (list (string-append
                  "  \"answer_room\": "
                  (if (strata-configuration-answer-room? config)
                      "true" "false")
                  ",\n")))
           ""))
         ;; The run config's `sampling` block (see the Sampling record fields
         ;; above for what each key does and which the engine accepts) plus the
         ;; top-level repeat_stop_tokens safety net.  Precomputed HERE and
         ;; spliced with #$ (a gexp does not capture let-bound locals - see the
         ;; comment on reasoning-json for the 0-byte run-strata.sh that trap
         ;; produced).  Each key is omitted on its own via sampling-entry, so a
         ;; host that sets none keeps the run config byte-identical to before;
         ;; the `sampling` object itself is emitted only when at least one key
         ;; is set.
         (sampling-json
          (let ((sampling-entries
                 (append
                  (sampling-entry "repetition_penalty"
                                  (strata-configuration-repetition-penalty
                                   config))
                  (sampling-entry "presence_penalty"
                                  (strata-configuration-presence-penalty
                                   config))
                  (sampling-entry "frequency_penalty"
                                  (strata-configuration-frequency-penalty
                                   config))
                  (sampling-entry "penalty_last_n"
                                  (strata-configuration-penalty-last-n
                                   config))
                  (sampling-entry "temperature"
                                  (strata-configuration-temperature config))
                  (sampling-entry "top_p"
                                  (strata-configuration-top-p config))
                  (sampling-entry "top_k"
                                  (strata-configuration-top-k config))
                  (sampling-entry "min_p"
                                  (strata-configuration-min-p config))
                  (sampling-entry "seed"
                                  (strata-configuration-seed config)))))
           (string-join
            (append
             (if (null? sampling-entries)
                 '()
                 (list (string-append
                        "  \"sampling\": {"
                        (string-join sampling-entries ", ")
                        "},\n")))
             (if (strata-configuration-repeat-stop-tokens config)
                 (list (string-append
                        "  \"repeat_stop_tokens\": "
                        (number->string
                         (strata-configuration-repeat-stop-tokens config))
                        ",\n"))
                 '()))
            "")))
         ;; The parking flags as "args" lines, in the same style as
         ;; reasoning-json above: precomputed HERE and spliced with #$,
         ;; because a gexp does not capture let-bound locals (see that
         ;; comment for the 0-byte run-strata.sh that trap produced once).
         ;; Gated on the budget — slots and the floor mean nothing without
         ;; one — and each flag is omitted on its own so an unset one keeps
         ;; the engine's default.  "" when parking is off: the engine then
         ;; behaves exactly as it did before these knobs existed.
         (conv-cache-json
          (let ((mib (strata-configuration-conversation-cache-mib config)))
            (if mib
                (string-join
                 (append
                  (engine-arg-line "--conversation-cache-mib" mib)
                  (engine-arg-line "--conversation-cache-slots"
                                   (strata-configuration-conversation-cache-slots
                                    config))
                  (engine-arg-line "--conversation-cache-min-free-mib"
                                   (strata-configuration-conversation-cache-min-free-mib
                                    config)))
                 "\n")
                "")))
         (slug (strata-native-model-slug model))
         (model-root (string-append data-dir "/" slug))
         (pack-dir (string-append model-root "/pack"))
         (hf-repo (strata-native-model-hf-repo model))
         (shard1 (strata-native-model-pack-source model))
         (shard2 (strata-native-model-shard2 model))
         (mmproj (strata-native-model-mmproj model))
         ;; Vision is on only when BOTH a reader package and the model's
         ;; mmproj are present: a reader alone leaves the server looking for
         ;; an mmproj it cannot open, and mmproj alone (the state fcc2a88
         ;; removed this block from) is the same failure the other way round
         ;; — a vision sub-config with no "exe".
         (vision-on? (and vision-reader mmproj))
         (mtp-dir (strata-native-model-mtp-dir model))
         ;; Batch windows as "args" lines, in the same precomputed style as
         ;; conv-cache-json above - a gexp does NOT capture let-bound locals, so
         ;; everything that is not written #$ like this resolves at activation
         ;; time in a nearly empty environment and leaves run-strata.sh at 0
         ;; bytes (see the comment on reasoning-json for that trap).
         ;; --batch without --batch-mtp measured -59% sequential here, hence the
         ;; gate: batch-mtp? is emitted only with a model that has an MTP runtime.
         (batch-json
          (let ((batch (strata-configuration-batch config)))
            (if batch
                (string-join
                 (append
                  (engine-arg-line "--batch" batch)
                  (if (and (strata-configuration-batch-mtp? config) mtp-dir)
                      (engine-flag-line "--batch-mtp") '()))
                 "\n")
                "")))
         ;; `extra-args' was the catalog `strata launch' path's hook; on the
         ;; native path it reached NOTHING until now, which is why --batch could
         ;; not be expressed from a Guix config at all (the verified live wrapper
         ;; had zero occurrences of it). Engine options go here, one array
         ;; element per list element, so a flag and its value are two entries.
         (extra-args-json
          (if (null? (strata-configuration-extra-args config))
              ""
              (string-join
               (map (lambda (a) (string-append "    " (string-quote a) ","))
                    (strata-configuration-extra-args config))
               "\n")))
         ;; `serve' flags, spliced onto the exec line: `--parallel' is "requested
         ;; concurrent engine slots" (serve --help) and `--batch-telemetry'
         ;; samples the real decode windows so the monitor's live.slots is
         ;; populated - the way the batch arms were verified in
         ;; docs/strata-subagent-throughput.org §8. With none of parallel/batch/
         ;; serve-args configured this renders "", so a host that asks for no
         ;; lanes keeps exactly the command line it has always had.
         (serve-flags
          (string-append
           (if (strata-configuration-parallel config)
               (string-append " --parallel "
                              (number->string
                               (strata-configuration-parallel config)))
               "")
           (if (strata-configuration-batch config)
               " --batch-telemetry" "")
           (if (null? (strata-configuration-serve-args config))
               ""
               (string-append " "
                              (string-join
                               (map string-quote
                                    (strata-configuration-serve-args config))
                               " ")))))
         (cfg (string-append state-dir "/" slug ".json"))
         (log (string-append state-dir "/" slug ".log"))
         (api-key-json (if api-key (string-quote api-key) "\"none\""))
         ;; The native run config's --kv value: the configured quant, or
         ;; q4_0 — the tier the context/VRAM sizing comments assume.  It
         ;; used to be hardcoded here while the `kv' field only reached
         ;; the catalog path; now the field drives both.
         (kv-native (or (strata-configuration-kv config) "q4_0"))
         ;; --spec (and its confidence floor) as one precomputed JSON
         ;; fragment: the draft length the MTP verify window proposes.
         ;; Both live in the args array; --spec-min-p is omitted on #f.
         (spec-json
          (string-append
           "    \"--spec\", \""
           (number->string (strata-configuration-spec config)) "\","
           (if (strata-configuration-spec-min-p config)
               (string-append
                "\n    \"--spec-min-p\", \""
                (number->string (strata-configuration-spec-min-p config))
                "\",")
               "")))
         (prefill-arg
          (if (strata-configuration-prefill config)
              (number->string (strata-configuration-prefill config))
              "auto"))
         (vram-reserve-json
          (if (strata-configuration-vram-reserve-mib config)
              (string-append "    \"--vram-reserve-mib\", \""
                             (number->string
                              (strata-configuration-vram-reserve-mib config))
                             "\",\n")
              ""))
         ;; Extra `export KEY=VALUE' lines (the engine-env field): values
         ;; unquoted so they may reference earlier exports; an empty list
         ;; renders one blank line, which sh and the JSON both ignore.
         (engine-env-exports
          (string-join
           (map (lambda (kv)
                  (string-append "export " (car kv) "=" (cdr kv)))
                (strata-configuration-engine-env config))
           "\n"))
         ;; The run config's "exe" line.  With `sycl-engine' set it points
         ;; DIRECTLY at the forked engine's entrypoint in the store: an
         ;; absolute path, which strata_control's engine_component.expand()
         ;; passes through unchanged, bypassing the component resolver —
         ;; newest-wins resolution would otherwise prefer the `!'-looping
         ;; upstream 0.1.57/0.1.59 component folders over the fork's 0.1.56,
         ;; and a locally-built component manifest should not have to pass
         ;; the bundle's component checks at all.  The binary is
         ;; self-contained: build_sycl_linux.sh collects the SYCL/MKL-SYCL/
         ;; UR/Level-Zero libraries beside it and patchelfs RUNPATH $ORIGIN
         ;; into every ELF, so it runs from the read-only store path exactly
         ;; like the vision sub-config's "exe" below.  Precomputed HERE and
         ;; spliced with #$ — a gexp does not capture let-bound locals (see
         ;; the reasoning-json comment).
         (sycl-engine (strata-configuration-sycl-engine config))
         (exe-json
          (if sycl-engine
              #~(string-append "  \"exe\": \"" #$sycl-engine "/strata\",")
              "  \"exe\": \"component:strata-engine\","))
         (engine-fetch-lines
          (if sycl-engine
              '()
              (list
               "# The engine component version MUST match the Terminal/Control"
               "# bundle: $BUNDLE/VERSION holds exactly the engine version this"
               "# bundle expects (\"0.1.57\"), so the guard below fires on every"
               "# package bump instead of leaving the old component in place"
               "# (\"strata launch\" resolves component:strata-engine by newest"
               "# glob, so an unmatched old engine would otherwise keep running"
               "# after an upgrade). \"strata update\" fetches ONLY the engine"
               "# package (docs/INTEL_ARC.md); \"strata install\" also pulls a"
               "# whole catalog model, so it is the fallback for a fresh VM."
               "WANT=$(head -1 \"$BUNDLE/VERSION\" 2>/dev/null | tr -d ' \\t\\r\\n')"
               "if [ -n \"$WANT\" ] && ! ls \"$ENGINE_DIR\"/\"$WANT\"-*-sycl >/dev/null 2>&1; then"
               "  echo \"[strata] engine $WANT-sycl not present; fetching it (one-time)\""
               "  if ls \"$ENGINE_DIR\"/*-sycl >/dev/null 2>&1; then"
               "    printf 'y\\n' | \"$BUNDLE/bin/strata\" update --yes --no-control || echo \"[strata] WARNING: engine fetch failed; continuing with the installed component\""
               "  else"
               "    \"$BUNDLE/bin/strata\" install --family qwen --model q2_0 --yes"
               "  fi"
               "fi")))
         (fetch-calls
          (string-join
           (append
            (map (lambda (ps)
                   (string-append "fetch " (string-quote (car ps)) " "
                                  (number->string (cdr ps))))
                 (strata-native-model-model-files model))
            (map (lambda (ps)
                   (string-append "fetch " (string-quote (car ps)) " "
                                  (number->string (cdr ps))))
                 (strata-native-model-aux-files model)))
           "\n")))
    #~(string-join
        (list
         "#!/bin/sh"
         "set -eu"
         ""
         "# Native-pack model wrapper — provision then serve in the foreground."
         "# Tracked by shepherd; the final `exec` makes the tracked PID the"
         "# server itself (TERM to it stops the engine cleanly)."
         "export PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin"
         (string-append "export HOME=" #$state-dir)
         (string-append "export XDG_DATA_HOME=" #$state-dir "/share")
         (string-append "export XDG_STATE_HOME=" #$state-dir "/state")
         (string-append "export XDG_CONFIG_HOME=" #$state-dir "/config")
         (string-append "export XDG_CACHE_HOME=" #$state-dir "/cache")
         "export LD_LIBRARY_PATH=/run/current-system/profile/lib"
         (string-append "export ZE_ICD_FILE=" #$state-dir "/ze_intel_gpu.icd.json")
         "export SYCL_CACHE_PERSISTENT=0"
         "export ZES_ENABLE_SYSMAN=1"
         ;; Force the Intel/SYCL engine (this VM also passes through the RTX
         ;; 4090, in active use by koboldcpp) and the Battlemage GuC
         ;; preemption-surface workaround — same as the catalog wrapper.
         "export STRATA_ENGINE_BACKEND=sycl"
         "export NEOReadDebugKeys=1"
         "export NEO_L0_OverridePreemptionSurfaceSizeInMb=64"
         "export NEO_OverridePreemptionSurfaceSizeInMb=64"
         ;; See the ssd-gbps field: virtio-blk cannot tell the guest that its
         ;; backing volume is NVMe, so without this the engine plans the expert
         ;; and file tiers for a 0.15 GB/s platter.  The export MUST be here —
         ;; this is the script that execs the engine — and it survives because
         ;; run-strata.sh is rewritten at every activation.
         #$(strata-ssd-gbps-export config)
         ;; The fork engine's persistent measurement cache (int8 expert
         ;; crossovers etc., re-measured at start when absent).  Under
         ;; XDG_CACHE_HOME so it survives restarts but not reimages.
         "mkdir -p \"$XDG_CACHE_HOME/strata-engine\""
         "export STRATA_CACHE_DIR=\"$XDG_CACHE_HOME/strata-engine\""
         ;; Host-configured engine environment (the engine-env field).
         #$engine-env-exports
         (string-append "BUNDLE=" #$pkg)
         (string-append "MODEL_ROOT=" #$model-root)
         (string-append "HF_REPO=" #$hf-repo)
         (string-append "CFG=" #$cfg)
         (string-append "LOG=" #$log)
         (string-append "PORT=" (number->string #$port))
         (string-append "HOST=" #$host)
         (string-append "PACK_DIR=" #$pack-dir)
         ""
         "# Idempotent, resume-capable fetch: skip when the file is present at"
         "# the published size; otherwise download to a .part (wget -c"
         "# resumes a partial one) and move it into place only after the"
         "# size verifies. A partial or corrupt fetch fails the start"
         "# instead of building a bad pack."
         "fetch() {"
         "  path=$1"
         "  size=$2"
         "  dest=$MODEL_ROOT/$path"
         "  url=\"https://huggingface.co/$HF_REPO/resolve/main/$path\""
         "  if [ -f \"$dest\" ] && [ \"$(stat -c%s \"$dest\" 2>/dev/null)\" = \"$size\" ]; then"
         "    return 0"
         "  fi"
         "  # A complete .part (an interrupted mv) moves into place directly,"
         "  # avoiding a wget -c resume that the server would answer 416."
         "  if [ -f \"$dest.part\" ] && [ \"$(stat -c%s \"$dest.part\" 2>/dev/null)\" = \"$size\" ]; then"
         "    mv \"$dest.part\" \"$dest\""
         "    return 0"
         "  fi"
         "  mkdir -p \"$(dirname \"$dest\")\""
         "  [ -f \"$dest\" ] && rm -f \"$dest\""
         "  echo \"[strata] downloading $path ($size bytes)\""
         "  wget -q --tries=5 --waitretry=5 --timeout=60 -c -O \"$dest.part\" \"$url\""
         "  actual=$(stat -c%s \"$dest.part\")"
         "  if [ \"$actual\" != \"$size\" ]; then"
         "    echo \"[strata] ERROR: $path is $actual bytes, expected $size\" >&2"
         "    exit 1"
         "  fi"
         "  mv \"$dest.part\" \"$dest\""
         "  echo \"[strata] $path verified\""
         "}"
         ""
         "# Ensure the engine component is installed: the native path reuses it"
         "# via component:strata-engine, and it is a runtime artifact Strata"
         "# downloads on first use. If it is missing (a fresh VM), install it"
         "# now — the install also fetches the catalog model it is invoked"
         "# with, a one-time cost paid only when the engine is absent."
         "ENGINE_DIR=\"$XDG_DATA_HOME/strata/components/engine\""
         #$@engine-fetch-lines
         ""
         #$fetch-calls
         ""
         "# Build the native pack (idempotent — native_experts.txt is written"
         "# last by iq_pack.py, so its absence means the pack is unfinished)."
         "if [ ! -f \"$PACK_DIR/native_experts.txt\" ]; then"
         "  echo \"[strata] building the native pack (a few minutes)\""
         "  (cd \"$BUNDLE\" && \"$BUNDLE/python/bin/python3\" tools/iq_pack.py \\"
         (string-append "     --gguf \"$MODEL_ROOT/" #$shard1 "\" \\")
         "     --out \"$PACK_DIR\")"
         "fi"
         ""
         "# Which expert-VRAM ranking to pre-load. INTEL.md: on the `xe` driver"
         "# setup uses data/expert-profile-sycl.bin — a ranking built from the"
         "# routing of 45 varied chat/code/non-English prompts — while an"
         "# i915 card keeps the shipped generic profile. Both files ship in"
         "# the bundle, and which driver the guest has is NOT visible to"
         "# Guile when this script is generated, so choose it here at run"
         "# time. (comfyui's B70 runs on xe, so this normally picks the"
         "# sycl ranking; deleting the /sys test back to \"i915 behaviour\""
         "# is the A/B arm.)"
         "PROFILE=\"$BUNDLE/data/expert-profile.bin\""
         "if [ -e /sys/bus/pci/drivers/xe ] && [ -f \"$BUNDLE/data/expert-profile-sycl.bin\" ]; then"
         "  PROFILE=\"$BUNDLE/data/expert-profile-sycl.bin\""
         "fi"
         ""
         "# Write the run config the server will load. The engine reads the"
         "# raw GGUFs in place (--native/--ple-gguf) AND the native pack"
         "# (--pack); the MTP runtime is the pre-built strata/rt folder."
         "mkdir -p \"$XDG_STATE_HOME\""
         "cat > \"$CFG\" <<EOF"
         "{"
         #$exe-json
         "  \"cwd\": \"$BUNDLE\","
         "  \"args\": ["
         "    \"--pack\", \"$PACK_DIR\","
         (string-append "    \"--native\", \"$MODEL_ROOT/" #$shard1 "\",")
         (string-append "    \"--ple-gguf\", \"$MODEL_ROOT/" #$shard2 "\",")
         ;; --expert-profile + --expert-cache auto are REQUIRED for the engine
         ;; to build the profile-filled VRAM expert tier. Without the profile
         ;; the prompt path falls back to "allocates its own buffers (too few
         ;; cache slots to borrow)" and the server dies at startup with a SYCL
         ;; UR_RESULT_ERROR_UNINITIALIZED instead of coming up. A fixed
         ;; --expert-cache 64 is far too small for the prompt path to borrow
         ;; from, hence "auto" (the engine then sizes the resident expert tier
         ;; to the free VRAM and streams the rest from the mmap'd GGUFs). The
         ;; profile ships with the bundle (data/expert-profile.bin plus the
         ;; xe-specific expert-profile-sycl.bin, which the PROFILE line above
         ;; selects) and is the same kind of file the catalog `strata launch`
         ;; path passes - it is a bundle-level architecture default, not a
         ;; per-model artifact, so it is valid for the abliterated weights.
         ;; Confirmed live 2026-10-29: with these two
         ;; flags the abliterated IQ2_XS serves (context 32768) on the B70.
         "    \"--expert-profile\", \"$PROFILE\","
         "    \"--expert-cache\", \"auto\","
         #$vram-reserve-json
         (string-append "    \"--prefill\", \"" #$prefill-arg "\",")
         #$spec-json
         (string-append "    \"--mtp\", \"$MODEL_ROOT/" #$mtp-dir "\",")
         ;; After --spec and --mtp on purpose: --batch-mtp says it "needs --mtp
         ;; and --spec", and the engine parses flags in order.
         #$batch-json
         (string-append "    \"--max-context\", \"" (number->string #$context) "\",")
         (string-append "    \"--kv\", \"" #$kv-native "\",")
         ;; Conversation parking, if a budget was configured (what each flag
         ;; does and how to size it is documented on the
         ;; conversation-cache-mib field above).  These belong INSIDE "args"
         ;; because they are engine options, while the three reasoning keys
         ;; below are top-level config keys.  With parking off this renders a
         ;; blank line, which JSON reads as whitespace — `"log"` already sits
         ;; behind the same blank line coming from reasoning-json.
         #$conv-cache-json
         #$extra-args-json
         (if #$vision-on?
             "    \"--mmap-experts\",\n    \"--vision\""
             "    \"--mmap-experts\"")
         "  ],"
         "  \"tokenizer\": \"$PACK_DIR/tokenizer\",\n"
         ;; The reader sub-config.  serve/server.py's Vision.__init__ reads
         ;; cfg["exe"] from it (that KeyError: 'exe' is what took the service down);
         ;; Vision.reader_args appends --gpu only when cfg["gpu"] is truthy, so
         ;; "gpu": false is the CPU reader - what upstream prescribes for an Intel
         ;; Arc of 24 GB or more (vision_reader.py, INTEL_GPU_READER_BY_DEFAULT =
         ;; False) and the one that takes nothing from the B70's expert cache.
         ;; max_tokens overrides DEFAULT_MAX_TOKENS' cpu default of 300, which
         ;; coarsens a 1024x1024 picture past the point body paint can be told
         ;; from skin.
         (if #$vision-on?
             (string-append
              "  \"vision\": {\n"
              (string-append "    \"exe\": \"" #$vision-reader
                             "/bin/strata-vision\",\n")
              "    \"gpu\": false,\n"
              "    \"max_tokens\": 1024,\n"
              (string-append "    \"model\": \"$MODEL_ROOT/" #$shard1 "\",\n")
              (string-append "    \"mmproj\": \"$MODEL_ROOT/" #$mmproj "\"\n")
              "  },")
             "")
         "  \"host\": \"$HOST\","
         (string-append "  \"port\": " (number->string #$port) ",")
         (string-append "  \"api_key\": " #$api-key-json ",")
         (string-append "  \"allowed_hosts\": [" #$allowed-hosts-json "],")
         ;; The three reasoning keys, spliced in as a precomputed string (see
         ;; reasoning-json above for why it cannot be free variables in this
         ;; gexp).  They are OUTSIDE "args": --reasoning-effort inside args is
         ;; not an engine option (server.py:8316 strips it out and promotes it
         ;; to this config key), and reasoning_budget_tokens / reasoning_effort /
         ;; answer_room are only ever read from the config's top level.
         #$reasoning-json
         #$sampling-json
         "  \"log\": \"$LOG\""
         "}"
         "EOF"
         ""
         "# Start the server in the foreground. `exec` chains wrapper ->"
         "# bin/strata (sh) -> the private CPython server, all one PID, so"
         "# shepherd tracks the server and TERM to it stops the engine."
         ;; --parallel / --batch-telemetry / serve-args, all precomputed into
         ;; serve-flags above: empty when nothing is configured, so the line is
         ;; byte-identical to what it was before those fields existed.
         (string-append
          "exec \"$BUNDLE/bin/strata\" serve --profile \"$CFG\" --engine strata"
          " --host \"$HOST\" --port \"$PORT\""
          #$serve-flags)
         "\n")
        "\n")))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (strata-accounts config)
  (if (strata-configuration-create-account? config)
      (list
       ;; The `render` group (supplementary membership is required by
       ;; Strata's doctor; it is not in %default-groups on the VM).
       (user-group (name "render") (system? #t))
       (user-group (name (strata-configuration-group config))
                   (system? #t))
       (user-account (name (strata-configuration-user config))
                     (group (strata-configuration-group config))
                     (supplementary-groups
                      (strata-configuration-supplementary-groups config))
                     (system? #t)
                     (comment "Strata MoE inference engine daemon")
                     (home-directory (strata-configuration-state-dir
                                      config))
                     (shell (file-append shadow "/sbin/nologin"))))
      '()))

(define (strata-activation config)
  (let* ((user (strata-configuration-user config))
         (state-dir (strata-configuration-state-dir config))
         (data-dir (strata-configuration-data-dir config))
         (log-file (strata-resolved-log-file config))
         (log-dir (dirname log-file))
         (icd-lib (file-append intel-level-zero-gpu "/lib/libze_intel_gpu.so.1"))
         ;; The wrapper script: the native-pack provisioner when a native
         ;; model is configured, else the catalog daemon wrapper. Both are
         ;; gexps (store paths substituted at activation time).
         (script
          (let ((nm (strata-configuration-native-model config)))
            (if nm
                (strata-native-wrapper-script
                 config (strata-native-model-lookup nm)
                 ;; The CPU picture reader, built by (peteches packages
                 ;; strata) because upstream packages one only inside its
                 ;; CUDA engine builds.  (vision "cpu") turns it on; the #f
                 ;; default passes #f through and the wrapper stays exactly
                 ;; as it was — text only, no reader reference, so a missing
                 ;; reader cannot take the service down again.
                 (if (string=? "cpu" (or (strata-configuration-vision config)
                                         ""))
                     strata-vision
                     #f))
                (strata-wrapper-script config))))
         ;; Extra data dirs to create: the native model's root and pack dir
         ;; when in native mode (the downloads create their own subdirs via
         ;; mkdir -p; these two are the top-level ones the pack build and the
         ;; size-verify marker check expect to exist).
         (extra-dirs
          (let ((nm (strata-configuration-native-model config)))
            (if nm
                (let* ((model (strata-native-model-lookup nm))
                       (slug (strata-native-model-slug model))
                       (model-root (string-append data-dir "/" slug)))
                  (list model-root (string-append model-root "/pack")))
                '())))
         ;; The full directory list, computed at build time. It is spliced
         ;; into the activation as a QUOTED data list: a bare #$ of a list
         ;; value is written as an unquoted list in the generated script,
         ;; which Scheme reads as a procedure call ("Wrong type to apply"),
         ;; not as the list of directories to create.
         (all-dirs
          (append (list state-dir
                        (string-append state-dir "/share")
                        (string-append state-dir "/state")
                        (string-append state-dir "/config")
                        (string-append state-dir "/state/strata/runtime")
                        log-dir
                        data-dir)
                  extra-dirs)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw (getpwnam #$user))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          ;; State dirs: runtime/ + tuning/ under XDG_STATE_HOME, the
          ;; engine components/ under XDG_DATA_HOME, profiles/ under
          ;; XDG_CONFIG_HOME, plus the model data dir and the log dir (and,
          ;; in native mode, the model's root and pack dir).
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    (quote #$all-dirs))
          ;; Level Zero ICD manifest: the Intel NEO runtime .deb ships no
          ;; ICD json, so generate the one-line manifest the loader needs
          ;; (same as the oneAPI installer does) and point ZE_ICD_FILE at
          ;; it from the wrapper.
          (with-output-to-file (string-append #$state-dir
                                              "/ze_intel_gpu.icd.json")
            (lambda _
              (display
               (string-join
                (list "{"
                      (string-append
                       "\"library_path\": \"" #$icd-lib "\"")
                      "}")))
              (newline)))
          (chmod (string-append #$state-dir "/ze_intel_gpu.icd.json") #o644)
          (chown (string-append #$state-dir "/ze_intel_gpu.icd.json")
                 uid gid)
          ;; The wrapper (store paths substituted at build time).
          (with-output-to-file (string-append #$state-dir
                                              "/run-strata.sh")
            (lambda _ (display #$script)))
          (chmod (string-append #$state-dir "/run-strata.sh") #o755)
          (chown (string-append #$state-dir "/run-strata.sh") uid gid)))))

(define (strata-shepherd-service config)
  (let* ((service-name (strata-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (state-dir (strata-configuration-state-dir config))
         (user (strata-configuration-user config))
         (group (strata-configuration-group config))
         (log-file (strata-resolved-log-file config)))
    (list (shepherd-service
           (provision (list provision-name))
           (documentation (strata-configuration-documentation config))
           (requirement '(networking file-systems))
           (auto-start? (strata-configuration-auto-start? config))
           (start #~(make-forkexec-constructor
                     (list (string-append #$state-dir "/run-strata.sh"))
                     #:user #$user
                     #:group #$group
                     #:log-file #$log-file))
           (stop #~(make-kill-destructor))))))

;;; ── Service type ─────────────────────────────────────────────────────────

;;; ── Firewall ─────────────────────────────────────────────────────────────

;; Accept the engine's port in the input chain only when open-firewall? is
;; set.  Mirrors koboldcpp-firewall-rules (peteches/services/koboldcpp.scm):
;; the comfyui VM's base firewall is drop-by-default, so without this the
;; tailnet-bound engine is unreachable from Caddy's reverse proxy.
;;; The picture reader must be in the *system closure*, not just named in the
;;; wrapper.  `display #$script` writes a store path that was substituted at
;;; elaboration of a NESTED gexp, and a nested gexp's localized inputs are not
;;; propagated into the derivation that embeds it (the activate.scm drv of a
;;; deployed vision build listed only 4 derivations, none of them
;;; strata-vision, while the same gexp passed straight to `gexp->file` did
;;; carry the reader's drv).  A path that nothing references is not a GC root,
;;; so `guix gc` on the VM could free the reader out from under the wrapper.
;;; This extension puts it in the system profile, which makes it a root and
;;; guarantees the deploy sends it.
(define (strata-vision-packages config)
  ;; Mirror the wrapper's gate exactly: "cpu" is the only mode with a reader
  ;; package today (a CUDA reader for "gpu" would be a separate build), and
  ;; anything else — #f, "off", a typo — keeps the system package-free so a
  ;; text-only service cannot be taken down by a missing reader.
  (if (string=? "cpu" (or (strata-configuration-vision config) ""))
      (list strata-vision)
      '()))

(define (strata-firewall-rules config)
  (if (strata-configuration-open-firewall? config)
      (let ((rule-suffix
             (string-append "tcp dport "
                            (number->string
                             (strata-configuration-port config))
                            " accept comment \""
                            (strata-configuration-service-name config)
                            "\""))
            (sources (strata-configuration-firewall-sources config)))
        (nftables-rules
         (input
          (if (null? sources)
              (list rule-suffix)
              (map (lambda (cidr)
                     (string-append
                      ;; The chain lives in an inet table: v4 sources match
                      ;; with `ip saddr', v6 with `ip6 saddr'.
                      (if (string-index cidr #\:) "ip6 saddr " "ip saddr ")
                      cidr " " rule-suffix))
                   sources)))))
      (nftables-rules (input '()))))

(define-public strata-service-type
  (service-type (name 'strata)
                (description
                 "Run the Strata MoE inference engine (Intel Arc SYCL) as a
system service.")
                (extensions (list (service-extension account-service-type
                                                     strata-accounts)
                                  (service-extension activation-service-type
                                                     strata-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   strata-shepherd-service)
                                  (service-extension firewall-service-type
                                                     strata-firewall-rules)
                                  (service-extension profile-service-type
                                                     strata-vision-packages)))))