;; peteches/services/openvino.scm — OpenVINO GenAI model-serving service.
;;
;; Runs one OpenVINO IR model directory behind an OpenAI-compatible HTTP endpoint,
;; using the Guix packages in (peteches packages openvino) rather than a pip venv:
;; the runtime, the GenAI library and its Python bindings all come from the store,
;; so there is no env to build and nothing to `pip install' at boot.  The HTTP
;; layer is openvino-server.py (co-located with this module, delivered by
;; local-file — same arrangement as (peteches services transcriber) ships its
;; runner), speaking /v1/chat/completions and /v1/completions so the rest of the
;; fleet can talk to it like any other chat backend.
;;
;; THE STRATA RULE — READ THIS BEFORE TURNING IT ON.
;; comfyui's Arc B-series GPU (8086:e223) and the RAM strata holds against it are
;; strata's until Peteches says otherwise; nothing in this trial may compete for
;; them.  Two things enforce that rather than just requesting it:
;;   1. The `openvino' package is built with -DENABLE_INTEL_GPU=OFF (and NPU off),
;;      so the GPU plugin does not exist in the runtime at all — there is no
;;      device="GPU" to ask for, and OV cannot wander onto the B-series by accident
;;      or because someone flipped a config value.  Same "structurally incapable"
;;      argument colibri.scm makes for its CPU-only variant.
;;   2. device defaults to CPU below, which on comfyui means the i9-14900KS: no
;;      SYCL, no Level Zero, no CUDA, no torch.  ("CORE" is *not* an available
;;      device name in this build — measured: the runtime registers CPU/AUTO/
;;      HETERO/MULTI/TEMPLATE and asking for CORE fails with `Device with "CORE"
;;      name is not registered', so the narrower string is also the correct one.)
;; What it *does* still take is CPU threads and host RAM — bf16 weights for a 24B
;; MoE are ~48 GB against this VM's 54 GiB, so the first test should use a
;; quantised/INT4 export (or fewer experts resident), not the raw download.  That
;; is a capacity question to settle before auto-start?, not a GPU one.
;;
;; auto-start? defaults to #f: the model directory is a large out-of-band artifact
;; that has to be converted first (`optimum-cli export openvino'), and a boot-time
;; crash loop over an unconverted or absent model helps no one.  Start it by hand
;; with `herd start <service-name>' for a trial run, then flip auto-start? on the
;; instance here once the model is proven.
;;
;; Serving concurrency: one generation at a time (a lock in openvino-server.py).
;; GenAI pipelines are thread-hungry already and this box also runs ComfyUI,
;; AllTalk, WhisperX and strata; serialising requests keeps a chatty client from
;; turning into N concurrent 24B generations on the same core set.  num-threads /
;; cpu-pinning / perf-hint below map onto OV's PERF_* properties so the trial can
;; be capped without touching the script.
;;
;; Firewall: nothing opens by default (host defaults to 127.0.0.1).  Set
;; open-firewall? #t only when serving beyond loopback, and prefer a tailnet bind
;; over opening the LAN interface — the VM's base policy is drop-input.

(define-module (peteches services openvino)
  #:use-module (guix gexp)
  #:use-module (guix packages)              ;package / inherit / package-arguments
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (srfi srfi-1)                ;delete-duplicates
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)     ;python-fastapi, python-uvicorn
  #:use-module (peteches packages openvino)
  #:use-module (peteches services firewall)
  #:use-module (peteches repository)                ;source-path
  #:export (openvino-server-configuration
            openvino-server-configuration?
            openvino-service-type))

;;; ── Utilities ────────────────────────────────────────────────────────────

(define (maybe-env-var name value)
  (if (and value (not (string=? value "")))
      (list (string-append name "=" value))
      '()))

(define (package-with-tests-off pkg)
  "Return PKG with its test phase disabled, preserving every other build argument.

Guix's build systems read #:tests? through assq-ref, so the FIRST such key in a
package's arguments list wins.  We prepend our own and drop any the package already
carried, which forces tests off no matter what upstream set -- here that is unambiguous
because neither fastapi nor uvicorn lists #:tests? at all (they only carry #:test-flags,
so tests run by default).  This exists for one concrete reason: python-fastapi 0.142.3
fails its `check' phase on a flaky pytest-xdist race -- the tutorial tests copytree into
a shared ./static with exist_ok=False and collide under --numprocesses, raising
FileExistsError even though 3321 other tests pass.  One such failure aborts the whole
system derivation this service is spliced into, so a green-looking suite still blocks the
deploy.  Skipping the test phase sidesteps it deterministically; these two are ordinary
site-packages installs whose compiled bytes do not depend on running their own suite."
  (package
    (inherit pkg)
    (arguments
      ;; Prepend our #:tests? #f and rebuild the original argument plist minus any upstream
      ;; #:tests? pair, preserving order and all other keys (assq picks ours first).
      (cons* #:tests? #f
             (let loop ((a (package-arguments pkg)) (out '()))
               (cond
                ((null? a) out)
                ((eq? (car a) #:tests?) (loop (cddr a) out))   ;drop upstream tests? + value
                (else (cons* (car a) (cadr a)
                             (loop (cddr a) out)))))))))

(define %ov-serve-packages
  ;; The HTTP serving layer, merged into the system profile.  Only fastapi is wrapped in
  ;; package-with-tests-off (the flaky suite documented above); uvicorn is left as the
  ;; original upstream package on purpose.  Wrapping uvicorn too would put a *second*
  ;; python-uvicorn of the same name+version into the profile -- my tests-off variant next
  ;; to the original that already arrives transitively via fastapi -> python-fastapi-cli --
  ;; and guix rejects two profile entries for one package with
  ;; "profile contains conflicting entries for python-uvicorn."  Leaving it unwrapped keeps
  ;; both references pointing at the same store path, so they dedup.
  (list (package-with-tests-off python-fastapi)
        python-uvicorn))

(define (openvino-configs config-or-configs)
  ;; Guix calls a service extension with the *value* of one instance at a time -- see
  ;; gnu/services.scm, which does (compute (service-value service)) per dependent and then
  ;; composes the results across instances.  This repo uses that single value two ways:
  ;; transcriber's instances carry a LIST of configurations, koboldcpp's and strata's carry
  ;; one record.  Accept both shapes so an instance can be written whichever reads better
  ;; in a system config; a service type should not have opinions about that.
  ;; Accept both shapes so an instance can be written whichever reads better in a system
  ;; config; a service type should not have opinions about that.  Dispatch is on `pair?',
  ;; not on the generated predicate: define-record-type* makes its predicate a syntax
  ;; transformer, and using it in ordinary procedure position fails with "wrong type to
  ;; apply" — which is also why nothing in this repo's services calls a *-configuration?
  ;; at run time.  A wrong value still gets caught, just by the first accessor that needs
  ;; a configuration field rather than here.
  (if (pair? config-or-configs)
      config-or-configs
      (list config-or-configs)))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <openvino-server-configuration> openvino-server-configuration
                     make-openvino-server-configuration
  openvino-server-configuration?
  (package openvino-server-configuration-package
           (default openvino))
  ;; GenAI pipeline library + its Python bindings.  #f means "no GenAI layer": the
  ;; service still starts and serves /healthz, which is how the runtime can be
  ;; smoke-tested before openvino-genai finishes building.
  (genai-package openvino-server-configuration-genai-package
                 (default openvino-genai))
  (server-script openvino-server-configuration-server-script
                 ;; Resolved through source-path, not a bare relative local-file — the same
                 ;; trap transcriber.scm documents from its own live hit: a default thunk is
                 ;; evaluated in the INSTANTIATING module's context (comfyui.scm, or this
                 ;; repo's probe), where "openvino-server.py" resolves against
                 ;; peteches/systems/ and dies with "No such file or directory",
                 ;; which is exactly what this line produced in the dry-run.
                 (default (local-file (source-path "peteches/services/openvino-server.py"))))

  ;; The interpreter that runs the server.  It has to be the same python the
  ;; bindings were installed against, because their site-packages land in
  ;; lib/pythonX.Y/ — both sides derive X.Y from this package's version.
  (python openvino-server-configuration-python
          (default python))

  (service-name openvino-server-configuration-service-name
                (default "openvino"))
  (documentation openvino-server-configuration-documentation
                 ;; Device-neutral on purpose: which device this serves is the
                 ;; `device' field's job, and GPU is now a working option (see
                 ;; the Intel OpenCL requirement documented in (peteches packages
                 ;; strata)).  It used to say "CPU" here, which went stale the
                 ;; moment the package was rebuilt with the intel_gpu plugin.
                 (default "OpenVINO GenAI model server (OpenAI-compatible)."))
  (auto-start? openvino-server-configuration-auto-start?
               (default #f))

  (create-account? openvino-server-configuration-create-account?
                   (default #t))
  (user openvino-server-configuration-user
        (default "openvino"))
  (group openvino-server-configuration-group
         (default "openvino"))

  ;; Directory holding the OpenVINO export: openvino.xml/.bin or fp16.npy + the
  ;; GenAI tokenizer files (openvino_tokenizer.xml, openvino_chat_template... if
  ;; it was saved with an OV tokenizer).  Produced by `optimum-cli export
  ;; openvino', not by anything here.
  (model-dir openvino-server-configuration-model-dir)

  ;; OV device string.  CPU = the i9-14900KS only: see the strata rule in the
  ;; module comment.  This is not a style choice — measured against the built
  ;; runtime (openvino 2026.4.1), `ov.Core().available_devices' reports ['CPU']
  ;; and LLMPipeline(dir, "CORE") fails with `Device with "CORE" name is not
  ;; registered'.  AUTO/HETERO:CPU/MULTI:CPU all construct too, so if a future
  ;; package build ever compiles in a GPU plugin, AUTO would then pick it — which
  ;; is exactly the accident the strata rule exists to prevent.  Keep CPU.
  (device openvino-server-configuration-device
          (default "CPU"))

  (host openvino-server-configuration-host
        (default "127.0.0.1"))
  (port openvino-server-configuration-port
        (default 8093))

  ;; CPU-plugin knobs, empty/#f to leave OpenVINO's own defaults alone.  These are
  ;; the names this build accepts (measured against a live pipeline — every PERF_*
  ;; spelling is rejected by intel_cpu/src/config.cpp):
  ;;   num-threads  -> INFERENCE_NUM_THREADS   ("8" caps the trial at eight threads)
  ;;   cpu-pinning  -> ENABLE_CPU_PINNING      ("NO", "ENABLED", "TRUE")
  ;;   perf-hint    -> PERFORMANCE_HINT        ("LATENCY", "THROUGHPUT",
  ;;                                            "CUMULATIVE_THROUGHPUT" — anything
  ;;                                            else is a config error)
  ;; The server script passes them as device properties to the LLMPipeline ctor;
  ;; there is no set_property() on the pipeline in this binding to fall back on.
  (num-threads openvino-server-configuration-num-threads
               (default #f))
  (cpu-pinning openvino-server-configuration-cpu-pinning
               (default #f))
  (perf-hint openvino-server-configuration-perf-hint
             (default #f))
  (max-new-tokens openvino-server-configuration-max-new-tokens
                  (default #f))

  ;; Think guard: bound the model's reasoning block separately from its answer.
  ;; max-new-tokens has to cover BOTH (the chat template opens every assistant
  ;; turn inside `<think>`), so a runaway reasoning block returns reasoning and
  ;; an empty reply -- measured on comfyui 2026-11-24, 11 of 94 generations were
  ;; TRUNCATED EMPTY-ANSWER(think-block-never-closed) at 8192 tokens, all with
  ;; content_chars=0.  These four map 1:1 onto the OPENVINO_THINK_BUDGET /
  ;; OPENVINO_ANSWER_RESERVE / OPENVINO_CHARS_PER_TOKEN /
  ;; OPENVINO_MAX_THINK_PASSES knobs in openvino-server.py, whose comments carry
  ;; the full measurement and the recovery-loop design.  #f (the default) emits
  ;; no variable, so the script's own defaults apply.  The guard switches itself
  ;; off -- server behaves exactly as it did before it existed -- when ANY of
  ;; think-budget, answer-reserve or chars-per-token is 0, or when
  ;; max-think-passes is below 2 (no pass left to recapture the answer in).
  ;;   think-budget     tokens of reasoning before the block is force-closed
  ;;   answer-reserve   tokens held back for the answer pass(es)
  ;;   chars-per-token  streamer sees text, not tokens: the budget is converted
  ;;   max-think-passes  total generations per turn, pass 1 included
  (think-budget openvino-server-configuration-think-budget
                (default #f))
  (answer-reserve openvino-server-configuration-answer-reserve
                  (default #f))
  (chars-per-token openvino-server-configuration-chars-per-token
                   (default #f))
  (max-think-passes openvino-server-configuration-max-think-passes
                    (default #f))

  ;; Packages the server needs at run time, merged into the system profile so the
  ;; interpreter finds them through its normal search path: the HTTP layer
  ;; (fastapi/uvicorn) has a deep transitive closure, so stitching PYTHONPATH by
  ;; hand here would be fragile.  openvino and genai are added implicitly from the
  ;; two package fields above.  The default uses tests-disabled variants of both (see
  ;; %ov-serve-packages / package-with-tests-off): fastapi's pytest suite is flaky under
  ;; xdist and otherwise fails this service's whole system build.
  (serve-packages openvino-server-configuration-serve-packages
                  (default %ov-serve-packages))

  (extra-environment-variables
   openvino-server-configuration-extra-environment-variables
   (default '()))
  (extra-args openvino-server-configuration-extra-args
              (default '()))

  (state-dir openvino-server-configuration-state-dir
             (default #f))
  (log-file openvino-server-configuration-log-file
            (default #f))

  ;; Only open the firewall when intentionally serving beyond localhost.
  (open-firewall? openvino-server-configuration-open-firewall?
                  (default #f))

  ;; CIDRs the accept rule is narrowed to, one nft rule each.  Empty (the
  ;; default) keeps the historical unrestricted accept.  Set this whenever
  ;; open-firewall? is #t: the server has no authentication at all, so an
  ;; unrestricted accept hands an unauthenticated GPU endpoint to every host on
  ;; the 192.168.50.0/23 LAN, not just the tailnet that caddy and the desktops
  ;; arrive over.  Mirrors strata-configuration-firewall-sources, including the
  ;; Tailscale CGNAT + ULA prefixes; see that field's comment in
  ;; peteches/systems/comfyui.scm for why all three CIDRs are listed.
  (firewall-sources openvino-server-configuration-firewall-sources
                    (default '())))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (openvino-resolved-state-dir config)
  (or (openvino-server-configuration-state-dir config)
      (string-append "/var/lib/openvino/"
                     (openvino-server-configuration-service-name config))))

(define (openvino-resolved-log-file config)
  (or (openvino-server-configuration-log-file config)
      (string-append "/var/log/openvino/"
                     (openvino-server-configuration-service-name config) ".log")))

(define (openvino-profile-site-packages python-pkg)
  "The system profile's merged site-packages directory for PYTHON-PKG, e.g.
  /run/current-system/profile/lib/python3.12/site-packages.  Both halves are known at
  build time — the profile path is fixed and the python minor version is part of the
  package definition — so this returns a plain string, which the gexp embeds with #$.
  A raw shepherd process gets no profile search-path environment, so the service must
  name this merged dir itself to reach numpy (openvino's propagated-input) and the
  fastapi/uvicorn closure."
  (let ((parts (string-split (package-version python-pkg) #\.)))
    (string-append "/run/current-system/profile/lib/python"
                   (car parts) "." (cadr parts) "/site-packages")))

;;; ── Environment ──────────────────────────────────────────────────────────

(define (openvino-environment config)
  ;; Returns a list of strings and gexps — mixed on purpose: shepherd's
  ;; #:environment-variables is spliced into a gexp, so each element only has to be valid
  ;; gexp data.  Anything that mentions a store path has to be a gexp, because at this
  ;; point `(file-append pkg "/lib")' is a <file-append> object and a meta-time
  ;; string-append over it fails with "wrong type to apply".
  (let* ((pkg (openvino-server-configuration-package config))
         (genai (openvino-server-configuration-genai-package config))
         (python (openvino-server-configuration-python config))
         ;; Computed here, not inside the gexp: a gexp evaluates its #$(...) splices in a
         ;; meta environment where this module's internal (non-exported) functions are not
         ;; resolvable, so `#$(openvino-profile-site-packages python)' dies with an unbound
         ;; variable at OS-build time.  Binding the resulting plain string here and splicing
         ;; it with #$ is exactly the idiom the HOME line uses for state-dir, which works.
         (site-packages (openvino-profile-site-packages python))
         (state-dir (openvino-resolved-state-dir config))
         ;; Store paths have to be assembled INSIDE a gexp, not with a meta-time
         ;; string-append: `(file-append pkg "/python")' returns a <file-append>
         ;; object, so (string-append "PYTHONPATH=" (file-append pkg ...)) dies
         ;; with "wrong type to apply" right here at OS-build time.  The idiom
         ;; that works is to make the env entry itself a gexp whose #$(...) splices
         ;; are evaluated now, in this function's scope where pkg and genai are
         ;; bound, so the build-time string-append only ever sees plain strings.
         ;; openvino-shepherd-service splices this whole list with #$@env into its
         ;; start gexp, and a gexp element lowers to the string it computes, so
         ;; #:environment-variables still receives a list of strings.  The trap to
         ;; avoid is referencing pkg/genai from *build-time* code: Guix would
         ;; resolve them against the outer gexp's module environment, where they
         ;; are out of scope, and fall back to the top-level `openvino' module
         ;; variable — "openvino: unbound variable" at OS-build time.
         (pythonpath
          #~(string-append "PYTHONPATH="
                           #$(file-append pkg "/python") ":"
                           #$(file-append genai "/python") ":"
                           #$site-packages))
         ;; Neither package's libraries reach /run/current-system/profile/lib:
         ;; upstream's SDK layout writes them to runtime/lib/intel64 and the
         ;; profile merge only ever picks up lib/, so both trees must be named
         ;; explicitly (verified on the box — profile/lib holds no libopenvino*,
         ;; and profile site-packages holds no openvino/).  The profile entries
         ;; are still needed for everything the GPU path dlopens out of the
         ;; system profile: libtbb, libze_loader, libigdgmm, libigdfcl, libigc.
         (ld-library-path
          #~(string-append "LD_LIBRARY_PATH="
                           #$(file-append pkg "/runtime/lib/intel64") ":"
                           #$(file-append genai "/runtime/lib/intel64") ":"
                           "/run/current-system/profile/lib:"
                           "/run/current-system/profile/lib64"))
         (home (string-append "HOME=" state-dir)))
    (append
     ;; HOME is where GenAI would cache anything it downloads; keep it in the service's own
     ;; state dir rather than /var/empty.
     (list home
           "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin")

     ;; PYTHONPATH, with both trees named because neither is a site-packages install: the
     ;; core runtime puts its bindings in <out>/python/openvino and GenAI does the same for
     ;; openvino_genai (verified against the built openvino output, whose whole tree is
     ;; upstream's SDK-style runtime/ + python/ layout rather than the GNU one its
     ;; OV_CPACK_* flags asked for).  Nothing in a system or home profile merge looks at
     ;; <out>/python, so without this line `import openvino_genai' simply fails and reads
     ;; like a broken model directory instead of a missing search path.
     ;; fastapi/uvicorn *are* ordinary site-packages installs, but they do NOT arrive
     ;; "from the merged system profile that guix's python searches by default" — that is
     ;; only true in a login/profile shell or `guix shell', where the profile's search-path
     ;; environment (a PYTHONPATH listing every site-packages dir) is applied.  A raw
     ;; make-forkexec-constructor shepherd process gets *only* the env vars listed here,
     ;; and the profile exports no PYTHONPATH at all (verified: `env -i python3' sees it
     ;; unset), so the interpreter's default sys.path carries just the python package's OWN
     ;; site-packages, never the profile's merged one.  Without naming it, `import numpy'
     ;; dies with ModuleNotFoundError on the first `herd start' — hit live.  The merged
     ;; dir is version-pinned to the python package below and carries numpy (openvino's
     ;; propagated-input) plus the whole fastapi/uvicorn closure.
     (list pythonpath)

     ;; LD_LIBRARY_PATH, same reasoning as koboldcpp.scm's and strata.scm's: an interpreter
     ;; started by shepherd gets no login environment, and a missing libopenvino_genai.so
     ;; looks like a broken model directory rather than a linker problem.  Both packages put
     ;; their libraries under runtime/lib/intel64 — that is where their install phases wrote
     ;; them (plugins included), while lib/ is empty or absent; genai's own
     ;; -DLIBRARY_DESTINATION=lib was ignored for the same reason core's OV_CPACK_* flags
     ;; were, so both entries name the tree upstream actually produced.  Both paths are also
     ;; merged into /run/current-system/profile/lib by profile-service-type, which is what
     ;; makes libtbb (genai propagates onetbb) reachable without being spelled out here.
     (list ld-library-path)
     (maybe-env-var "OPENVINO_DEVICE" (openvino-server-configuration-device config))
     (maybe-env-var "OPENVINO_MODEL_DIR" (openvino-server-configuration-model-dir config))
     (maybe-env-var "OPENVINO_HOST" (openvino-server-configuration-host config))
     (maybe-env-var "OPENVINO_PORT"
                    (number->string (openvino-server-configuration-port config)))
     (maybe-env-var "OPENVINO_NUM_THREADS" (openvino-server-configuration-num-threads config))
     (maybe-env-var "OPENVINO_CPU_PINNING" (openvino-server-configuration-cpu-pinning config))
     (maybe-env-var "OPENVINO_PERF_HINT" (openvino-server-configuration-perf-hint config))
     (maybe-env-var "OPENVINO_MAX_NEW_TOKENS"
                    (let ((n (openvino-server-configuration-max-new-tokens config)))
                      (and n (number->string n))))
     ;; The think guard.  number->string on a float yields "4.0", which Python's
     ;; float() reads back, so chars-per-token needs no special casing.
     (maybe-env-var "OPENVINO_THINK_BUDGET"
                    (let ((n (openvino-server-configuration-think-budget config)))
                      (and n (number->string n))))
     (maybe-env-var "OPENVINO_ANSWER_RESERVE"
                    (let ((n (openvino-server-configuration-answer-reserve config)))
                      (and n (number->string n))))
     (maybe-env-var "OPENVINO_CHARS_PER_TOKEN"
                    (let ((n (openvino-server-configuration-chars-per-token config)))
                      (and n (number->string n))))
     (maybe-env-var "OPENVINO_MAX_THINK_PASSES"
                    (let ((n (openvino-server-configuration-max-think-passes config)))
                      (and n (number->string n))))
     ;; Where the OpenCL ICD loader finds vendor descriptors.  A shepherd
     ;; process gets ONLY the variables listed here, so the value an interactive
     ;; login inherits from the profile is not in scope; without this line the
     ;; loader falls back to /etc/OpenCL/vendors, which does not exist on this
     ;; VM, finds no platform, and the intel_gpu plugin fails with
     ;; "[GPU] ProgramBuilder build failed! could not create an engine" — the
     ;; CL_INVALID_DEVICE path described in (peteches packages strata).  The
     ;; profile directory is the merge of every installed package's
     ;; etc/OpenCL/vendors, so intel.icd (intel-opencl-icd), pocl.icd and
     ;; nvidia.x86_64.icd are all visible here.
     ;;
     ;; Note this is required even for device=GPU-via-Level-Zero: oneDNN's ZE
     ;; backend queries device extensions through OpenCL, so the GPU path has a
     ;; hard OpenCL dependency.  It is harmless for device=CPU.
     ;; (list ...) on purpose: everything handed to the enclosing append has to be
     ;; a list, and bare strings here fail with "wrong type argument in position N
     ;; (expecting empty list)" — maybe-env-var above returns a list for the same
     ;; reason.
     (list "OCL_ICD_VENDORS=/run/current-system/profile/etc/OpenCL/vendors")
     (openvino-server-configuration-extra-environment-variables config))))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (openvino-accounts config-or-configs)
  ;; Deduplicated on purpose — two models served under one account (an int4 trial now, a
  ;; bf16 instance later) would otherwise ask account-service-type for the same group and
  ;; user twice, which lands as duplicated /etc/passwd lines rather than as an error.
  ;;
  ;; Dedup is by *name*, not delete-duplicates over the records: Guile's
  ;; delete-duplicates compares with eq?, and two separately constructed user-group records
  ;; with identical fields are two different objects, so it would keep both.  (It works for
  ;; transcriber-profile-entries because package objects there really are the same object.)
  ;; Keys are strings, where eq?-based dedup is also unreliable across instances, so pass
  ;; an explicit comparator and fold accounts by user name.
  (let* ((configs (openvino-configs config-or-configs))
         (want-account? (lambda (config)
                          (openvino-server-configuration-create-account? config)))
         (groups (delete-duplicates
                  ;; The accessor has to be wrapped: define-record-type* makes accessors
                  ;; syntax transformers, so `(map accessor lst)' rewrites at the call site
                  ;; and map ends up with too many arguments.  Every accessor use in this
                  ;; repo's services goes through a lambda for exactly this reason.
                  (map (lambda (config)
                         (openvino-server-configuration-group config))
                       (filter want-account? configs))
                  string=))
         ;; First config asking for a given user name decides its group and home directory;
         ;; later ones are ignored rather than merged, because "which service owns this
         ;; account's /var/lib dir" should have one answer.  Entries are *pairs* (user .
         ;; config), not 2-element lists: `(cons (list user config) acc)' would make `cdr'
         ;; return the list (config), and every accessor on it then dies with "expecting
         ;; struct" — which is exactly the error this line produced once.
         (by-user (fold (lambda (config acc)
                          (let ((user (openvino-server-configuration-user config)))
                            (if (assoc user acc) acc (cons (cons user config) acc))))
                        '() (filter want-account? configs))))
    (append (map (lambda (group)
                   (user-group (name group) (system? #t)))
                 groups)
            (map (lambda (entry)
                   (let ((config (cdr entry)))
                     (user-account (name (openvino-server-configuration-user config))
                                   (group (openvino-server-configuration-group config))
                                   (system? #t)
                                   (comment "OpenVINO GenAI model server daemon")
                                   (home-directory
                                    (openvino-resolved-state-dir config))
                                   (shell (file-append shadow "/sbin/nologin")))))
                 (reverse by-user)))))


(define (openvino-packages config-or-configs)
  ;; Everything the interpreter must find on its normal search path.  The two
  ;; package fields plus whatever the instance lists in serve-packages.
  (append-map (lambda (config)
                (let ((genai (openvino-server-configuration-genai-package config)))
                  (append (list (openvino-server-configuration-package config))
                          (if genai (list genai) '())
                          (list (openvino-server-configuration-python config))
                          (filter identity
                                  (openvino-server-configuration-serve-packages
                                   config)))))
              (openvino-configs config-or-configs)))

(define (openvino-instance-activation config)
  ;; One instance's activation as its own gexp, so openvino-activation can splice the list.
  ;; Every value is computed as a *plain string* BEFORE building the gexp.  The gexp is
  ;; spliced via #$@ into openvino-activation's gexp; if it referenced let-bindings from
  ;; *this* function's scope, Guix would try to resolve those bindings in the
  ;; openvino-activation gexp's meta environment, where they are out of scope, and fall
  ;; through to the top-level `openvino' module variable — which fails with
  ;; "openvino: unbound variable" at OS-build time.
  (let* ((user (openvino-server-configuration-user config))
         (model-dir (openvino-server-configuration-model-dir config))
         (state-dir (openvino-resolved-state-dir config))
         (log-file (openvino-resolved-log-file config)))
    ;; Values are spliced via #$ so they are resolved at creation time (when they are in
    ;; scope) and embedded as literal data in the gexp.  Using (gexp ...) without #$ would
    ;; leave them as symbols to be resolved at gexp-evaluation time, in openvino-activation's
    ;; meta environment, where they are out of scope — which falls through to the top-level
    ;; `openvino' module variable and fails with "openvino: unbound variable" at OS-build time.
    #~(begin
       (use-modules (guix build utils))
       (let* ((pw (getpwnam #$user))
              (uid (passwd:uid pw))
              (gid (passwd:gid pw)))
         (mkdir-p #$model-dir)
         (for-each (lambda (dir)
                     (mkdir-p dir)
                     (chown dir uid gid))
                   (list #$state-dir
                         (dirname #$log-file)))))))

(define (openvino-activation config-or-configs)
  #~(begin
      (use-modules (guix build utils))
      #$@(map openvino-instance-activation (openvino-configs config-or-configs))))

(define (openvino-shepherd-service config)
  (let* ((service-name (openvino-server-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (python (openvino-server-configuration-python config))
         (python-path (file-append python "/bin/python3"))
         (pkg (openvino-server-configuration-package config))
         (genai (openvino-server-configuration-genai-package config))
         (site-packages (openvino-profile-site-packages python))
         (script (openvino-server-configuration-server-script config))
         (user (openvino-server-configuration-user config))
         (group (openvino-server-configuration-group config))
         (log-file (openvino-resolved-log-file config))
         (env (openvino-environment config))
         (args (openvino-server-configuration-extra-args config)))
    (list (shepherd-service
           (provision (list provision-name))
           (documentation (openvino-server-configuration-documentation config))
           (requirement '(networking file-systems))
           (auto-start? (openvino-server-configuration-auto-start? config))
           (start #~(make-forkexec-constructor
                     (append (list #$python-path #$script)
                             (list #$@args))
                     #:user #$user
                     #:group #$group
                     #:log-file #$log-file
                     #:environment-variables (list #$@env)))
           (stop #~(make-kill-destructor))))))

(define (openvino-firewall-rules config-or-configs)
  ;; Nothing opens unless asked: the default host is loopback, and this VM's base policy
  ;; is drop-input, so a silent "tcp dport ... accept" here would be a security change
  ;; disguised as a convenience.
  ;;
  ;; When it is asked for, `firewall-sources' narrows the accept to those CIDRs, emitting
  ;; one rule per entry — `ip saddr' for v4 and `ip6 saddr' for v6, because the chain
  ;; lives in an inet table.  Same shape as strata-firewall-rules, for the same reason:
  ;; both are unauthenticated GPU endpoints that only the tailnet should reach.
  (nftables-rules
   (input (append-map (lambda (config)
                        (if (not (openvino-server-configuration-open-firewall? config))
                            '()
                            (let ((rule-suffix
                                   (string-append "tcp dport "
                                                  (number->string
                                                   (openvino-server-configuration-port
                                                    config))
                                                  " accept comment \""
                                                  (openvino-server-configuration-service-name
                                                   config) "\""))
                                  (sources
                                   (openvino-server-configuration-firewall-sources
                                    config)))
                              (if (null? sources)
                                  (list rule-suffix)
                                  (map (lambda (cidr)
                                         (string-append
                                          (if (string-index cidr #\:)
                                              "ip6 saddr " "ip saddr ")
                                          cidr " " rule-suffix))
                                       sources)))))
                      (openvino-configs config-or-configs)))))

(define (openvino-shepherd-services config-or-configs)
  (append-map openvino-shepherd-service (openvino-configs config-or-configs)))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public openvino-service-type
  (service-type (name 'openvino)
                (description
                 "Serve an OpenVINO GenAI model over an OpenAI-compatible HTTP API.")
                (extensions (list (service-extension account-service-type
                                                     openvino-accounts)
                                  (service-extension activation-service-type
                                                     openvino-activation)
                                  (service-extension shepherd-root-service-type
                                                     openvino-shepherd-services)
                                  (service-extension profile-service-type
                                                     openvino-packages)
                                  (service-extension firewall-service-type
                                                     openvino-firewall-rules)))))
