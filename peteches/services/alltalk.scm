;; peteches/services/alltalk.scm — Guix service type for AllTalk TTS v2 via uv.
;;
;; AllTalk (https://github.com/erew123/alltalk_tts, branch `alltalkbeta') is
;; the multi-engine TTS server SillyTavern integrates with natively: it serves
;; a JSON/HTTP API on port 7851 (what SillyTavern's "AllTalk" TTS provider
;; talks to) plus a Gradio management interface on 7852.  The engine this
;; fleet runs it for is Coqui XTTS v2 — a voice-cloning model that takes its
;; emotional delivery from a ~6 s reference WAV per voice, which is exactly
;; the "variety of voices with emotional resonance" the CPU Kokoro setup
;; (koboldcpp --ttsmodel) could not deliver.
;;
;; Same service boundary as peteches/services/comfyui.scm: Guix owns the
;; account, paths, environment, ports and supervision; uv owns the Python
;; environment.  Upstream's own installers are Miniconda-based (atsetup.sh)
;; and its Docker image is a conda environment — neither is reproducible
;; under Guix, so this service rebuilds the same dependency set with uv
;; inside the proven `guix shell --container --emulate-fhs' sandbox (see
;; comfyui.scm's module header for why prebuilt wheels force that).  The
;; version choices mirror AllTalk's own v2 Docker base image exactly, which
;; is the combination upstream actually tests: Python 3.11 (the bundled
;; system/config/fairseq-*.whl is cp311-only, so 3.11 is not negotiable),
;; torch/torchaudio/torchvision 2.7.1/0.22.1/2.7.1 from the cu128 wheel
;; index, then system/requirements/requirements_standalone.txt verbatim.
;;
;; DeepSpeed is deliberately NOT installed: it is an optional 2-3x XTTS
;; accelerator whose prebuilt wheels are pinned to conda's torch ABI, and
;; plain torch XTTS on a 4090 is already several times realtime.  It can be
;; revisited later (compile-from-source inside the sandbox, like
;; comfyui.scm's SageAttention install) if generation speed ever matters.
;;
;; Three shepherd services, same bootstrap contract as comfyui:
;;   <name>-sync    one-shot, manual (`herd start alltalk-sync'): clones the
;;                  repo if absent, creates the venv, installs torch +
;;                  requirements, then runs upstream's own
;;                  system/config/firstrun.py --tts_model <tts-model>, which
;;                  is idempotent (skips files already on disk) and downloads
;;                  the XTTS v2 weights (~1.9 GB from HuggingFace).
;;   <name>-update  one-shot, manual: git fetch + pull --ff-only, then the
;;                  same sync steps against the new checkout.
;;   <name>         the daemon (`python script.py`), auto-start.  script.py
;;                  spawns tts_server.py (API, :7851) and serves the Gradio
;;                  UI (:7852) itself, then supervises — the same entrypoint
;;                  the upstream Docker image runs headless.  It waits up to
;;                  240 s for /api/ready (model loaded) before giving up.
;;
;; Because the daemon does not depend on the sync one-shot, a fresh deploy
;; needs one manual `herd start alltalk-sync' before the daemon can run —
;; identical to the comfyui service's first-boot contract.  Everything
;; AllTalk writes at runtime (confignew.json, models/, voices/, outputs/)
;; lives inside project-dir, so it must be writable and persistent; put it
;; on the data volume, not the root disk.

(define-module (peteches services alltalk)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages version-control)
  #:use-module (srfi srfi-1)
  #:use-module (peteches services firewall)
  #:export (alltalk-configuration alltalk-configuration? alltalk-service-type))

;;; ── Utilities ────────────────────────────────────────────────────────────

(define (join-paths-gexp paths)
  (if (null? paths)
      #~""
      #~(string-append
         #$@(let loop ((rest (cdr paths)) (acc (list (car paths))))
              (if (null? rest)
                  (reverse acc)
                  (loop (cdr rest) (cons (car rest) (cons ":" acc))))))))

(define (unique-configs-by proc configs)
  (fold-right (lambda (cfg acc)
                (if (any (lambda (existing)
                           (equal? (proc cfg)
                                   (proc existing))) acc) acc
                    (cons cfg acc)))
              '() configs))

;;; ── ALLTALK_API_URL source patch ────────────────────────────────────

;; The single upstream edit this service makes (only when api-public-url is
;; set): teach script.py's build_dynamic_url to prefer an ALLTALK_API_URL
;; env override for its base URL, instead of the hardcoded
;; http://<Host-header>:<api-port> it assembles in "local" (non-Docker, non-
;; Colab) mode.  All three generation-facing URLs (the server-side POST to
;; /api/tts-generate, the browser-side /audio/<f>.wav, and the browser-side
;; streaming endpoint) flow through this one base_url line with
;; include_protocol=False, so overriding it here fixes the scheme AND host
;; for all of them at once -- which is what lets the Gradio UI be served over
;; https through a reverse proxy on a different domain than the API.  The
;; old/new strings live in separate store files (not inline in the gexp) to
;; keep quote/brace escaping out of the Scheme, and the substitution is
;; applied by the tiny Python script below, run against the git checkout by
;; the venv python at sync time.  It is idempotent (re-applies cleanly after
;; the update service's `git pull' restores the pristine file) and it exits
;; non-zero if the anchor line ever disappears (upstream refactor), so sync
;; fails loudly rather than silently serving a broken https UI.
(define alltalk-api-url-patch
  (plain-file "alltalk-api-url-patch.py"
"import sys, pathlib
target, oldfile, newfile = sys.argv[1], sys.argv[2], sys.argv[3]
path = pathlib.Path(target)
old = pathlib.Path(oldfile).read_text()
new = pathlib.Path(newfile).read_text()
source = path.read_text()
if new in source:
    print('alltalk: script.py already carries the ALLTALK_API_URL override')
elif old in source:
    path.write_text(source.replace(old, new))
    print('alltalk: patched script.py build_dynamic_url for ALLTALK_API_URL')
else:
    raise SystemExit('alltalk: ERROR build_dynamic_url anchor line not found in script.py; upstream layout changed, cannot apply ALLTALK_API_URL override')
"))

(define alltalk-api-url-old
  (plain-file "alltalk-base-url-old.txt"
    "base_url = f\"{protocol_prefix}{_state['my_current_url']}\""))

(define alltalk-api-url-new
  (plain-file "alltalk-base-url-new.txt"
    "base_url = os.environ.get(\"ALLTALK_API_URL\") or f\"{protocol_prefix}{_state['my_current_url']}\""))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <alltalk-configuration> alltalk-configuration
                     make-alltalk-configuration
  alltalk-configuration?
  (service-name alltalk-configuration-service-name
                (default "alltalk"))
  (documentation alltalk-configuration-documentation
                 (default "AllTalk TTS server (XTTS voice synthesis)."))
  (auto-start? alltalk-configuration-auto-start?
               (default #t))

  ;; System account.
  (create-account? alltalk-configuration-create-account?
                   (default #t))
  (user alltalk-configuration-user
        (default "alltalk"))
  (group alltalk-configuration-group
         (default "alltalk"))
  (supplementary-groups alltalk-configuration-supplementary-groups
                        (default '("video")))

  ;; uv runtime — same contract as comfyui-service-type: uv and the Python
  ;; interpreter live in the store/system profile; the dependency graph
  ;; lives in a venv under project-dir that the sync service manages.
  (uv-command alltalk-configuration-uv-command
              (default "/run/current-system/profile/bin/uv"))
  (uv-package alltalk-configuration-uv-package
              (default (@ (gnu packages build-tools) uv)))
  ;; Python 3.11, NOT the distro default: AllTalk bundles its patched
  ;; fairseq as a cp311-only wheel (system/config/fairseq-0.12.3.1-
  ;; cp311-cp311-linux_x86_64.whl, referenced straight from
  ;; requirements_standalone.txt), and upstream's Docker base is 3.11.11.
  (python-package alltalk-configuration-python-package
                  (default (@ (gnu packages python) python-3.11)))
  (project-dir alltalk-configuration-project-dir
               (default "/srv/alltalk"))
  (git-repo-url alltalk-configuration-git-repo-url
                (default "https://github.com/erew123/alltalk_tts"))
  ;; AllTalk v2 lives on the `alltalkbeta' branch — `master'/'main' is the
  ;; legacy v1 codebase (a completely different server).
  (git-ref alltalk-configuration-git-ref
           (default "alltalkbeta"))
  (git-command alltalk-configuration-git-command
               (default (file-append git "/bin/git")))

  ;; Torch stack — mirrors AllTalk's own docker/base/Dockerfile (CUDA 12.8
  ;; wheels).  The cu128 wheels carry their whole CUDA runtime as nvidia-*
  ;; wheel deps; only the kernel driver's libcuda.so.1 comes from the host
  ;; (via ld-library-paths below), same split as comfyui.
  (torch-version alltalk-configuration-torch-version
                 (default "2.7.1"))
  (torchvision-version alltalk-configuration-torchvision-version
                       (default "0.22.1"))
  (torchaudio-version alltalk-configuration-torchaudio-version
                      (default "2.7.1"))
  (torch-index-url alltalk-configuration-torch-index-url
                   (default "https://download.pytorch.org/whl/cu128"))

  ;; Engine firstrun.py downloads and marks as loaded.  "xtts" = Coqui XTTS
  ;; v2 (voice cloning, emotional reference-driven delivery).  Other
  ;; shipped engines: f5tts, parler, piper, vits — switching means a
  ;; different tts-model plus whatever extra requirements that engine
  ;; needs (see extra-requirements-files).
  (tts-model alltalk-configuration-tts-model
             (default "xtts"))
  ;; Additional requirements files (relative to project-dir) installed
  ;; after requirements_standalone.txt, e.g.
  ;; "system/requirements/requirements_f5tts.txt" to add the F5-TTS engine.
  (extra-requirements-files alltalk-configuration-extra-requirements-files
                            (default '()))
  ;; Bare package specs `uv pip install'ed together (single uv call, so the
  ;; resolver satisfies them jointly and re-resolves their own deps) after
  ;; the requirements files, for pins upstream's requirements omit.  Three
  ;; coupled caps resolve a dependency diamond that surfaces only at daemon
  ;; import time (hit live across the 2026-11-12 first boot):
  ;;   - coqui-tts 0.24.3's TTS/tts/layers/tortoise/arch_utils.py does
  ;;     `from transformers import LogitsWarper'.  LogitsWarper is a
  ;;     top-level transformers export only through 4.46.0 — it is GONE by
  ;;     4.46.3 (verified against the transformers git tags), and
  ;;     requirements_standalone.txt leaves transformers unpinned, so uv
  ;;     resolves transformers 5.x and coqui-tts dies on that import.
  ;;     => pin transformers==4.46.0 (the newest with LogitsWarper; also
  ;;        satisfies coqui-tts's own >=4.43.0).  Its deps force
  ;;        tokenizers>=0.20,<0.21 and huggingface-hub>=0.23.2,<1.0.
  ;;   - gradio 4.44.1's oauth.py does `from huggingface_hub import
  ;;     HfFolder', removed in huggingface_hub 1.0.  transformers 4.46.0
  ;;     already caps hub <1.0, but pin hub==0.26.2 explicitly to the
  ;;     transformers-4.46 era (Oct 2024) so uv can't pick a late 0.3x with
  ;;     drifted internals — deterministic, known-coherent snapshot.
  ;; Upstream's Docker image only escapes all of this because it was built
  ;; while transformers 4.4x / huggingface_hub 0.2x were current.  Keep all
  ;; three pins together; loosening any one re-opens the diamond.
  (extra-pip-packages alltalk-configuration-extra-pip-packages
                      (default (list "transformers==4.46.0"
                                     "huggingface_hub==0.26.2")))

  ;; Network.  Both ports are read from AllTalk's own confignew.json
  ;; (api_def.api_port_number / gradio_port_number), which ships with these
  ;; defaults; the fields here drive the firewall rules and must be kept in
  ;; step if the in-app admin UI ever changes them.
  (port alltalk-configuration-port
        (default 7851))
  (gradio-port alltalk-configuration-gradio-port
               (default 7852))

  ;; Public https base URL of the API reverse-proxy vhost (e.g. caddy's
  ;; alltalk.ts.peteches.co.uk), WITHOUT a trailing slash.  When set, the
  ;; sync service patches script.py's build_dynamic_url so that BOTH the
  ;; server-side generation POST and the browser-side audio/streaming URLs
  ;; use this absolute https base instead of AllTalk's hardcoded
  ;; http://<Host-header>:<api-port>.  That is what lets the Gradio UI be
  ;; served over https through a reverse proxy on a DIFFERENT domain/port
  ;; than the API: without it the UI derives http://<proxy-host>:7851 from
  ;; the browser's Host header (routes nowhere behind caddy -> the 180x1s
  ;; retry hang) and would in any case be http:// under an https:// page
  ;; (mixed content).  Mechanism: ALLTALK_API_URL env var (below) read by
  ;; the patched base_url line; see alltalk-api-url-patch.  Leave "" to keep
  ;; upstream behavior (direct http://<vm>:7852 access, no reverse proxy).
  (api-public-url alltalk-configuration-api-public-url
                  (default ""))

  ;; Paths.  Defaults are derived from service-name when #f.
  (state-dir alltalk-configuration-state-dir
             (default #f))
  (cache-dir alltalk-configuration-cache-dir
             (default #f))
  (log-dir alltalk-configuration-log-dir
           (default #f))
  (log-file alltalk-configuration-log-file
            (default #f))

  ;; Environment.  CUDA device 0 = the RTX 4090 on the comfyui VM (the only
  ;; CUDA device there; the Intel Arc B70 is not enumerated by CUDA).
  (cuda-visible-devices alltalk-configuration-cuda-visible-devices
                        (default "0"))
  (ld-library-paths alltalk-configuration-ld-library-paths
                    (default (list "/run/current-system/profile/lib"
                                   "/run/current-system/profile/lib64"
                                   ;; sounddevice (optional live-dictation
                                   ;; support in tts_server.py) dlopens
                                   ;; libportaudio.so.2 through the venv
                                   ;; Python's own Guix loader, which never
                                   ;; consults the FHS container's ldconfig
                                   ;; — LD_LIBRARY_PATH is the only mechanism
                                   ;; that reaches it (same class of problem
                                   ;; as opencv/glib in comfyui.scm).
                                   #~(string-append
                                      #$(@ (gnu packages audio) portaudio)
                                      "/lib"))))
  (extra-environment-variables
   alltalk-configuration-extra-environment-variables
   (default (list)))

  ;; Extra Guix packages declared inside the FHS container alongside the
  ;; baseline (coreutils, bash, git, gcc-toolchain, linux-libre-headers,
  ;; nss-certs, uv).  Same restriction as comfyui-service-type's field of
  ;; the same name: must resolve by NAME in the base distribution.  The
  ;; defaults cover what AllTalk shells out to: `ffmpeg' (ffmpeg-python
  ;; transcoding + openai-whisper) and `espeak-ng' (script.py's startup
  ;; check; required by some engines' phonemisation).
  (container-extra-packages alltalk-configuration-container-extra-packages
                            (default (list (@ (gnu packages video) ffmpeg)
                                           (@ (gnu packages speech) espeak-ng))))
  ;; Extra host paths writably bind-mounted into the container (e.g. an
  ;; external model volume holding project-dir).
  (container-extra-shares alltalk-configuration-container-extra-shares
                          (default (list)))
  ;; Same, read-only.
  (container-extra-exposes alltalk-configuration-container-extra-exposes
                           (default (list)))
  ;; Additional packages installed into the system profile (uv lives here —
  ;; uv-command's default resolves against the profile).
  (runtime-packages alltalk-configuration-runtime-packages
                    (default (list)))

  ;; Only open the firewall when intentionally serving beyond localhost.
  (open-firewall? alltalk-configuration-open-firewall?
                  (default #f)))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (alltalk-resolved-state-dir config)
  (or (alltalk-configuration-state-dir config)
      (string-append "/var/lib/alltalk/"
                     (alltalk-configuration-service-name config))))

(define (alltalk-resolved-cache-dir config)
  (or (alltalk-configuration-cache-dir config)
      (string-append "/var/cache/alltalk/"
                     (alltalk-configuration-service-name config))))

(define (alltalk-resolved-log-dir config)
  (or (alltalk-configuration-log-dir config)
      (string-append "/var/log/alltalk")))

(define (alltalk-resolved-log-file config)
  (or (alltalk-configuration-log-file config)
      (string-append (alltalk-resolved-log-dir config) "/"
                     (alltalk-configuration-service-name config) ".log")))

(define (alltalk-resolved-sync-log-file config)
  (string-append (alltalk-resolved-log-dir config) "/sync.log"))

(define (alltalk-resolved-update-log-file config)
  (string-append (alltalk-resolved-log-dir config) "/update.log"))

(define (alltalk-venv-dir config)
  (string-append (alltalk-configuration-project-dir config) "/.venv"))

;;; ── Environment rendering ────────────────────────────────────────────────

(define (alltalk-environment config)
  (let* ((state-dir (alltalk-resolved-state-dir config))
         (cache-dir (alltalk-resolved-cache-dir config))
         (hf-home (string-append cache-dir "/huggingface"))
         (uv-cache-dir (string-append cache-dir "/uv"))
         (xdg-cache-home (string-append cache-dir "/xdg"))
         (cuda-devices (alltalk-configuration-cuda-visible-devices config))
         (ld-paths (alltalk-configuration-ld-library-paths config))
         (api-public-url (alltalk-configuration-api-public-url config))
         (git-command (alltalk-configuration-git-command config)))
    (append (list (string-append "HOME=" state-dir)
             "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
             "UV_PYTHON_DOWNLOADS=never"
             (string-append "HF_HOME=" hf-home)
             "HF_HUB_DISABLE_TELEMETRY=1"
             "GRADIO_ANALYTICS_ENABLED=False"
             (string-append "UV_CACHE_DIR=" uv-cache-dir)
             (string-append "XDG_CACHE_HOME=" xdg-cache-home))
            (list #~(string-append "GIT_PYTHON_GIT_EXECUTABLE="
                                   #$git-command))
            (if cuda-devices
                (list (string-append "CUDA_VISIBLE_DEVICES=" cuda-devices)
                      "CUDA_DEVICE_ORDER=PCI_BUS_ID"
                      ;; Same Triton workaround as comfyui.scm: skip its
                      ;; ldconfig probe for libcuda by naming it directly.
                      "TRITON_LIBCUDA_PATH=/run/current-system/profile/lib")
                '())
            (if (null? ld-paths)
                '()
                (list #~(string-append "LD_LIBRARY_PATH="
                                       #$(join-paths-gexp ld-paths))))
            ;; Read by the patched build_dynamic_url in script.py (see
            ;; alltalk-api-url-patch).  Set for both the sync/update runners
            ;; (harmless) and, crucially, the daemon, so the running Gradio
            ;; process builds https API/audio URLs.  Empty -> not exported ->
            ;; the patch's `os.environ.get(...)' falls back to upstream
            ;; behavior.
            (if (string-null? api-public-url)
                '()
                (list (string-append "ALLTALK_API_URL=" api-public-url)))
            (alltalk-configuration-extra-environment-variables config))))

;;; ── FHS container wrapper ────────────────────────────────────────────────

;; Adapted from comfyui-container-runner-file — same rationale and same
;; sharp edges (see that module's comments): `--container' only bind-mounts
;; the requested closure, so --expose=/gnu/store makes every raw store path
;; reachable (the venv interpreter, uv-command, LD_LIBRARY_PATH entries);
;; nvidia device nodes are discovered at the wrapper's runtime; package
;; names are resolved by the invoked guix against the BASE distro only.
;; Unlike comfyui, no Python package is installed into the shell by name:
;; python-3.11's package name is plain "python", which `guix shell' would
;; resolve to the distro's default Python (3.12) — the venv interpreter is
;; referenced by its exact store path instead (UV_PYTHON / --python).
(define (alltalk-container-runner-file config suffix env inner-command)
  (let* ((service-name (alltalk-configuration-service-name config))
         (project-dir (alltalk-configuration-project-dir config))
         (state-dir (alltalk-resolved-state-dir config))
         (cache-dir (alltalk-resolved-cache-dir config))
         (uv-pkg (alltalk-configuration-uv-package config))
         (c-compiler-pkg (@ (gnu packages commencement) gcc-toolchain))
         (extra-pkgs (alltalk-configuration-container-extra-packages config))
         (extra-shares (alltalk-configuration-container-extra-shares config))
         (extra-exposes (alltalk-configuration-container-extra-exposes config))
         (package-names (map package-name
                             (append (list (@ (gnu packages base) coreutils)
                                           (@ (gnu packages bash) bash)
                                           (@ (gnu packages version-control) git)
                                           c-compiler-pkg
                                           (@ (gnu packages linux) linux-libre-headers)
                                           ;; Without this, SSL_CERT_FILE
                                           ;; points at a CA bundle
                                           ;; --emulate-fhs never populates,
                                           ;; and git/pip TLS fails.
                                           (@ (gnu packages nss) nss-certs)
                                           uv-pkg)
                                     extra-pkgs)))
         (static-args (append
                       (list "--container" "--emulate-fhs" "--network"
                             "--expose=/gnu/store"
                             (string-append "--share=" project-dir)
                             (string-append "--share=" state-dir)
                             (string-append "--share=" cache-dir)
                             "--expose=/run/current-system/profile")
                       (map (lambda (s) (string-append "--share=" s)) extra-shares)
                       (map (lambda (s) (string-append "--expose=" s)) extra-exposes)
                       package-names))
         (guix-pkg (@ (gnu packages package-management) guix)))
    (program-file
     (string-append service-name suffix "-container-runner")
     #~(begin
         (use-modules (ice-9 ftw) (srfi srfi-1) (srfi srfi-13))
         ;; Preserve env by NAME only, never "VAR=VALUE" — see the long
         ;; comment in comfyui-container-runner-file; `guix shell -E VAR=V'
         ;; does not inject V under shepherd's sparse environment.
         (define (preserve-flag kv)
           (string-append "--preserve=^" (substring kv 0 (string-index kv #\=)) "$"))
         (let* ((nvidia-exposes
                 (filter-map (lambda (name)
                               (and (string-prefix? "nvidia" name)
                                    (string-append "--expose=/dev/" name)))
                             (or (scandir "/dev") '())))
                (args (append (list "shell")
                              (list #$@static-args)
                              nvidia-exposes
                              (map preserve-flag (list #$@env))
                              (list "--")
                              (list #$@inner-command))))
           (apply execlp #$(file-append guix-pkg "/bin/guix") "guix" args))))))

;;; ── Sync / update steps ──────────────────────────────────────────────────

;; The shared install sequence, run by both the sync and update services
;; with cwd = project-dir.  Idempotent: uv pip install is a no-op when the
;; venv already satisfies the pins, and firstrun.py skips model files that
;; already exist.
(define (alltalk-sync-steps-gexp config)
  (let* ((uv-command (alltalk-configuration-uv-command config))
         (project-dir (alltalk-configuration-project-dir config))
         (venv-dir (alltalk-venv-dir config))
         (python-pkg (alltalk-configuration-python-package config))
         (python-bin (file-append python-pkg "/bin/python3"))
         (venv-python (string-append venv-dir "/bin/python"))
         (torch-version (alltalk-configuration-torch-version config))
         (torchvision-version (alltalk-configuration-torchvision-version config))
         (torchaudio-version (alltalk-configuration-torchaudio-version config))
         (torch-index-url (alltalk-configuration-torch-index-url config))
         (extra-requirements-files (alltalk-configuration-extra-requirements-files config))
         (extra-pip-packages (alltalk-configuration-extra-pip-packages config))
         (api-public-url (alltalk-configuration-api-public-url config))
         (tts-model (alltalk-configuration-tts-model config)))
    #~(begin
        (unless (file-exists? (string-append #$venv-dir "/pyvenv.cfg"))
          (let ((ret (system* #$uv-command "venv"
                              "--python" #$python-bin #$venv-dir)))
            (unless (zero? ret)
              (error "uv venv failed with exit code" ret))))
        ;; Torch first, from the CUDA wheel index, exactly like upstream's
        ;; docker/base/Dockerfile — so requirements_standalone.txt's
        ;; coqui-tts/onnxruntime pins resolve against an already-satisfied
        ;; torch instead of dragging PyPI's default build in.
        (let ((ret (system* #$uv-command "pip" "install"
                            "--python" #$venv-python
                            #$(string-append "torch==" torch-version)
                            #$(string-append "torchvision==" torchvision-version)
                            #$(string-append "torchaudio==" torchaudio-version)
                            "--index-url" #$torch-index-url)))
          (unless (zero? ret)
            (error "torch install failed with exit code" ret)))
        (let ((ret (system* #$uv-command "pip" "install"
                            "--python" #$venv-python
                            "-r" "system/requirements/requirements_standalone.txt")))
          (unless (zero? ret)
            (error "requirements_standalone install failed with exit code" ret)))
        #$@(map (lambda (req)
                  #~(let ((ret (system* #$uv-command "pip" "install"
                                        "--python" #$venv-python
                                        "-r" #$req)))
                      (unless (zero? ret)
                        (error "extra requirements install failed with exit code"
                               ret #$req))))
                extra-requirements-files)
        #$@(if (null? extra-pip-packages)
               '()
               (list #~(let ((ret (apply system* #$uv-command "pip" "install"
                                         "--python" #$venv-python
                                         (list #$@extra-pip-packages))))
                         (unless (zero? ret)
                           (error "extra pip packages install failed with exit code"
                                  ret)))))
        ;; Apply the ALLTALK_API_URL override to script.py's build_dynamic_url
        ;; (only when api-public-url is set).  Runs after the venv exists and
        ;; the checkout is present; idempotent, and fails loudly if upstream
        ;; moved the anchor line.  See alltalk-api-url-patch above.
        #$@(if (string-null? api-public-url)
               '()
               (list #~(let ((ret (system* #$venv-python #$alltalk-api-url-patch
                                           (string-append #$project-dir "/script.py")
                                           #$alltalk-api-url-old
                                           #$alltalk-api-url-new)))
                         (unless (zero? ret)
                           (error "alltalk script.py ALLTALK_API_URL patch failed with exit code"
                                  ret)))))
        ;; Upstream's own first-run model downloader (idempotent — skips
        ;; files already on disk).  Downloads the engine's weights into
        ;; <project-dir>/models/ and marks the engine loaded in
        ;; system/tts_engines/tts_engines.json + confignew.json.
        (let ((ret (system* #$venv-python "system/config/firstrun.py"
                            "--tts_model" #$tts-model)))
          (unless (zero? ret)
            (error "firstrun model download failed with exit code" ret))))))

(define (alltalk-sync-shepherd-service config)
  (let* ((service-name (alltalk-configuration-service-name config))
         (sync-name (string->symbol (string-append service-name "-sync")))
         (project-dir (alltalk-configuration-project-dir config))
         (git-command (alltalk-configuration-git-command config))
         (git-repo-url (alltalk-configuration-git-repo-url config))
         (git-ref (alltalk-configuration-git-ref config))
         (user (alltalk-configuration-user config))
         (group (alltalk-configuration-group config))
         (log-file (alltalk-resolved-sync-log-file config))
         (python-pkg (alltalk-configuration-python-package config))
         (env (append (alltalk-environment config)
                      (list #~(string-append "UV_PYTHON="
                                             #$(file-append python-pkg
                                                            "/bin/python3")))))
         (runner (program-file (string-append service-name "-sync-runner")
                               #~(begin
                                   (unless (file-exists? (string-append #$project-dir
                                                          "/.git"))
                                     (let ((ret (system* #$git-command "clone"
                                                         "--branch" #$git-ref
                                                         #$git-repo-url ".")))
                                       (unless (zero? ret)
                                         (error "git clone failed with exit code"
                                                ret))))
                                   #$(alltalk-sync-steps-gexp config))))
         (container-runner (alltalk-container-runner-file config "-sync" env
                                                           (list runner))))
    (shepherd-service (provision (list sync-name))
                      (documentation (string-append
                                      "Bootstrap/update the AllTalk checkout, "
                                      "uv environment and TTS model for "
                                      service-name "."))
                      (requirement '(networking file-systems))
                      (one-shot? #t)
                      (auto-start? #f)
                      (start #~(make-forkexec-constructor (list #$container-runner)
                                                          #:directory #$project-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env)))
                      (stop #~(lambda _
                                #t)))))

;; Manual version bump: fetch + fast-forward the checkout, then re-run the
;; sync steps against it (installs any new/changed pins).  Never restarts
;; the daemon — follow up with `herd restart <service-name>'.
(define (alltalk-update-shepherd-service config)
  (let* ((service-name (alltalk-configuration-service-name config))
         (update-name (string->symbol (string-append service-name "-update")))
         (project-dir (alltalk-configuration-project-dir config))
         (git-command (alltalk-configuration-git-command config))
         (user (alltalk-configuration-user config))
         (group (alltalk-configuration-group config))
         (log-file (alltalk-resolved-update-log-file config))
         (api-public-url (alltalk-configuration-api-public-url config))
         (python-pkg (alltalk-configuration-python-package config))
         (env (append (alltalk-environment config)
                      (list #~(string-append "UV_PYTHON="
                                             #$(file-append python-pkg
                                                            "/bin/python3")))))
         (runner (program-file (string-append service-name "-update-runner")
                               #~(begin
                                   (unless (file-exists? (string-append #$project-dir
                                                          "/.git"))
                                     (error "alltalk-update: no git checkout found at"
                                            #$project-dir))
                                   (let ((ret (system* #$git-command "-C" #$project-dir
                                                       "fetch" "--tags")))
                                     (unless (zero? ret)
                                       (error "git fetch failed with exit code" ret)))
                                   ;; If we patched script.py for
                                   ;; ALLTALK_API_URL, restore it to pristine
                                   ;; first so the --ff-only pull below isn't
                                   ;; blocked by our own local edit; the sync
                                   ;; steps re-apply the patch afterward.
                                   #$@(if (string-null? api-public-url)
                                          '()
                                          (list #~(system* #$git-command "-C" #$project-dir
                                                           "checkout" "--" "script.py")))
                                   ;; --ff-only refuses to move a checkout with
                                   ;; local commits/edits or a detached HEAD —
                                   ;; surface that as a failed run.  NOTE: AllTalk
                                   ;; rewrites files inside its own checkout at
                                   ;; runtime (confignew.json,
                                   ;; system/tts_engines/tts_engines.json,
                                   ;; model_settings.json); if a pull ever
                                   ;; conflicts with those, `git stash' them or
                                   ;; restore with `git checkout -- <file>' first.
                                   (let ((ret (system* #$git-command "-C" #$project-dir
                                                       "pull" "--ff-only")))
                                     (unless (zero? ret)
                                       (error "git pull failed with exit code" ret)))
                                   #$(alltalk-sync-steps-gexp config))))
         (container-runner (alltalk-container-runner-file config "-update" env
                                                           (list runner))))
    (shepherd-service (provision (list update-name))
                      (documentation (string-append
                                      "Manually pull the latest AllTalk commit and "
                                      "re-sync the uv environment for "
                                      service-name ".  Never starts on its own."))
                      (requirement '(networking file-systems))
                      (one-shot? #t)
                      (auto-start? #f)
                      (start #~(make-forkexec-constructor (list #$container-runner)
                                                          #:directory #$project-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env)))
                      (stop #~(lambda _
                                #t)))))

(define (alltalk-daemon-shepherd-service config)
  (let* ((service-name (alltalk-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (project-dir (alltalk-configuration-project-dir config))
         (user (alltalk-configuration-user config))
         (group (alltalk-configuration-group config))
         (log-file (alltalk-resolved-log-file config))
         (venv-dir (alltalk-venv-dir config))
         (env (alltalk-environment config))
         ;; script.py: spawns tts_server.py (the :7851 API SillyTavern talks
         ;; to) as a supervised subprocess, serves the Gradio management UI
         ;; on :7852 itself, and then sits in a keep-alive loop — the same
         ;; headless entrypoint the upstream Docker image runs.  It exits
         ;; non-zero if /api/ready is not up within its 240 s startup
         ;; window, which is what makes a failed model load visible to
         ;; shepherd instead of hanging.
         (inner-command (list (string-append venv-dir "/bin/python")
                              "script.py"))
         (container-runner (alltalk-container-runner-file config "" env
                                                           inner-command)))
    (shepherd-service (provision (list provision-name))
                      (documentation (alltalk-configuration-documentation config))
                      (requirement '(networking file-systems))
                      (auto-start? (alltalk-configuration-auto-start? config))
                      (start #~(make-forkexec-constructor (list #$container-runner)
                                                          #:directory #$project-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env)))
                      (stop #~(make-kill-destructor)))))

(define (alltalk-shepherd-services configs)
  (append (map alltalk-sync-shepherd-service configs)
          (map alltalk-update-shepherd-service configs)
          (map alltalk-daemon-shepherd-service configs)))

;;; ── Accounts / activation / firewall / profile ──────────────────────────

(define (alltalk-accounts configs)
  (let* ((account-configs (filter alltalk-configuration-create-account? configs))
         (group-configs (unique-configs-by alltalk-configuration-group
                                           account-configs))
         (user-configs (unique-configs-by alltalk-configuration-user
                                          account-configs)))
    (append (map (lambda (config)
                   (user-group
                     (name (alltalk-configuration-group config))
                     (system? #t)))
                 group-configs)
            (map (lambda (config)
                   (user-account
                     (name (alltalk-configuration-user config))
                     (group (alltalk-configuration-group config))
                     (system? #t)
                     (comment "AllTalk TTS server daemon")
                     (home-directory (alltalk-resolved-state-dir config))
                     (supplementary-groups
                      (alltalk-configuration-supplementary-groups config))
                     (shell (file-append shadow "/sbin/nologin"))))
                 user-configs))))

(define (alltalk-instance-activation config)
  (let* ((user (alltalk-configuration-user config))
         (state-dir (alltalk-resolved-state-dir config))
         (cache-dir (alltalk-resolved-cache-dir config))
         (log-dir (alltalk-resolved-log-dir config))
         (project-dir (alltalk-configuration-project-dir config))
         (dirs (list state-dir
                     cache-dir
                     (string-append cache-dir "/huggingface")
                     (string-append cache-dir "/uv")
                     (string-append cache-dir "/xdg")
                     log-dir
                     project-dir)))
    #~(let* ((pw (getpwnam #$user))
             (uid (passwd:uid pw))
             (gid (passwd:gid pw)))
        (for-each (lambda (dir)
                    (mkdir-p dir)
                    (chown dir uid gid))
                  (list #$@dirs)))))

(define (alltalk-activation configs)
  #~(begin
      (use-modules (guix build utils))
      #$@(map alltalk-instance-activation configs)))

(define (alltalk-firewall-rules configs)
  (nftables-rules
   (input (append-map (lambda (config)
                        (if (alltalk-configuration-open-firewall? config)
                            (list (string-append "tcp dport "
                                   (number->string
                                    (alltalk-configuration-port config))
                                   " accept comment \""
                                   (alltalk-configuration-service-name config)
                                   "\"")
                                  (string-append "tcp dport "
                                   (number->string
                                    (alltalk-configuration-gradio-port config))
                                   " accept comment \""
                                   (alltalk-configuration-service-name config)
                                   "-ui\""))
                            '()))
                      configs))))

(define (alltalk-profile-entries configs)
  (delete-duplicates
   (append-map (lambda (config)
                 (filter (lambda (x) x)
                         (cons (alltalk-configuration-uv-package config)
                               (alltalk-configuration-runtime-packages config))))
               configs)))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public alltalk-service-type
  (service-type (name 'alltalk)
                (compose append)
                (extend append)
                (extensions (list (service-extension account-service-type
                                                     alltalk-accounts)
                                  (service-extension activation-service-type
                                                     alltalk-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   alltalk-shepherd-services)
                                  (service-extension firewall-service-type
                                                     alltalk-firewall-rules)
                                  (service-extension profile-service-type
                                                     alltalk-profile-entries)))
                (default-value '())
                (description
                 "Run AllTalk TTS servers (XTTS and friends) via uv-managed Python environments.")))
