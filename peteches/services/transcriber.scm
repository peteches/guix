;; peteches/services/transcriber.scm — Guix service type for the WhisperX
;; transcription + speaker-diarization API.
;;
;; Answers the question AllTalk's Transcribe tab cannot: WHO said what.
;; AllTalk uses plain openai-whisper (single-stream transcript; its
;; "Enable Speaker Diarization" toggle is wired to nothing and its
;; identify_speaker() is dead code), so multi-speaker audio comes back with
;; no speaker labels.  This service runs WhisperX instead: faster-whisper
;; ASR + wav2vec2 phoneme alignment + pyannote diarization, producing
;; per-segment SPEAKER_xx labels.
;;
;; Shape: same service boundary as (peteches services alltalk) / comfyui —
;; Guix owns account/paths/ports/supervision, uv owns the Python env, all
;; inside `guix shell --container --emulate-fhs` (torch/ctranslate2/
;; onnxruntime/torchcodec are prebuilt ELF wheels with FHS assumptions).
;; Unlike AllTalk there is NO upstream git checkout: the entire app is
;; transcriber-server.py, co-located with this module and delivered from the
;; store via local-file — editing that file + redeploying is the whole
;; release process, and there is nothing to `git pull', so there is no
;; update service (version bumps = edit the pip pins here, redeploy, re-run
;; the sync one-shot).  Editing transcriber-server.py DOES need a manual
;; restart: deploying gen 118 switched the generation and shipped the new
;; runner but left the daemon gen 117 had started still running, so the change
;; only took effect after `sudo herd restart transcriber'.  Confirm with
;; `pgrep -f transcriber-server.py' plus /proc/<pid>/cmdline — the store hash
;; there must match the current generation's.
;;
;; Shepherd services:
;;   <name>-sync   one-shot, manual (`herd start transcriber-sync'): creates
;;                 the venv (Python 3.11 — whisperx requires >=3.10,<3.14 and
;;                 the VM's other GPU stacks are already 3.11), installs the
;;                 torch 2.8.0 cu128 stack (whisperx pins torch~=2.8.0), then
;;                 pip-packages (whisperx + fastapi/uvicorn/python-multipart).
;;                 Idempotent; re-run after any pin change.
;;   <name>        the daemon, auto-start: transcriber-server.py (lazy model
;;                 loading + idle unload — the 4090 is shared with ComfyUI
;;                 and AllTalk, so it must not squat VRAM between requests).
;;
;; HF token: diarization uses pyannote/speaker-diarization-community-1, a
;; GATED HuggingFace model.  The operator must (once, in a browser) accept
;; its terms and create an HF read token, which lands on the VM as a
;; sops-secret at hf-token-file (owner = the service user, #o400) — see
;; docs/secrets-management.org.  The server re-reads the file per request, so
;; no restart is needed after the secret appears.  Plain transcription
;; (diarize=false) needs no token.
;;
;; First request after boot downloads the model zoo into HF_HOME (~4 GB:
;; faster-whisper large-v3, wav2vec2 align model, pyannote pipeline) — put
;; cache-dir on the data volume, not the root disk.

(define-module (peteches services transcriber)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (srfi srfi-1)
  #:use-module (peteches repository)
  #:use-module (peteches services firewall)
  #:export (transcriber-configuration transcriber-configuration?
            transcriber-service-type))

;;; ── Utilities ────────────────────────────────────────────────────────────

(define (join-paths-gexp paths)
  (if (null? paths)
      #~""
      #~(string-append
         #$@(let loop ((rest (cdr paths)) (acc (list (car paths))))
              (if (null? rest)
                  (reverse acc)
                  (loop (cdr rest) (cons (car rest) (cons ":" acc))))))))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <transcriber-configuration> transcriber-configuration
                     make-transcriber-configuration
  transcriber-configuration?
  (service-name transcriber-configuration-service-name
                (default "transcriber"))
  (documentation transcriber-configuration-documentation
                 (default "WhisperX transcription + speaker diarization API."))
  (auto-start? transcriber-configuration-auto-start?
               (default #t))

  ;; System account.
  (create-account? transcriber-configuration-create-account?
                   (default #t))
  (user transcriber-configuration-user
        (default "transcriber"))
  (group transcriber-configuration-group
         (default "transcriber"))
  (supplementary-groups transcriber-configuration-supplementary-groups
                        (default '("video")))

  ;; uv runtime (same contract as alltalk/comfyui services).
  (uv-command transcriber-configuration-uv-command
              (default "/run/current-system/profile/bin/uv"))
  (uv-package transcriber-configuration-uv-package
              (default (@ (gnu packages build-tools) uv)))
  ;; Python 3.11: whisperx requires >=3.10,<3.14, and cp311 wheels exist for
  ;; the whole stack (torch 2.8, ctranslate2, onnxruntime, torchcodec).
  (python-package transcriber-configuration-python-package
                  (default (@ (gnu packages python) python-3.11)))
  ;; Where the .venv lives.  Put it on the data volume in the system config
  ;; (the venv is ~10 GB with the cu128 torch stack).
  (project-dir transcriber-configuration-project-dir
               (default "/srv/transcriber"))

  ;; Torch stack — whisperx 3.8.x pins torch~=2.8.0 / torchvision~=0.23.0
  ;; and pyannote.audio needs torchaudio; install all three from the cu128
  ;; wheel index BEFORE whisperx so its pins resolve against an
  ;; already-satisfied CUDA-enabled torch instead of PyPI's default build.
  (torch-version transcriber-configuration-torch-version
                 (default "2.8.0"))
  (torchvision-version transcriber-configuration-torchvision-version
                       (default "0.23.0"))
  (torchaudio-version transcriber-configuration-torchaudio-version
                      (default "2.8.0"))
  (torch-index-url transcriber-configuration-torch-index-url
                   (default "https://download.pytorch.org/whl/cu128"))

  ;; Installed after the torch stack, in one joint uv call.  whisperx is
  ;; pinned because its transitive pins (torch~=2.8, pyannote-audio>=4)
  ;; move fast; the web stack is what transcriber-server.py runs on
  ;; (whisperx itself brings neither fastapi nor uvicorn).
  (pip-packages transcriber-configuration-pip-packages
                (default (list "whisperx==3.8.6"
                               "fastapi"
                               "uvicorn"
                               "python-multipart")))

  ;; The single-file FastAPI app, co-located with this module (same pattern
  ;; as home/services/firefox.scm's profile-launcher).  Resolved through
  ;; source-path rather than a bare relative local-file: the default thunk
  ;; is evaluated in the INSTANTIATING module's context (comfyui.scm), where
  ;; a relative path would resolve against peteches/systems/ (hit live:
  ;; "No such file or directory: .../systems/transcriber-server.py").
  ;; source-path, not repo-directory: this is a file, not a directory.
  (server-file transcriber-configuration-server-file
               (default (local-file
                         (source-path
                          "peteches/services/transcriber-server.py"))))

  ;; Network.
  (port transcriber-configuration-port
        (default 8787))

  ;; transcriber-server.py knobs.
  (model-size transcriber-configuration-model-size
              (default "large-v3"))
  (compute-type transcriber-configuration-compute-type
                (default "float16"))
  (batch-size transcriber-configuration-batch-size
              (default 4))
  ;; Minutes of idleness before the models are evicted from VRAM (the card
  ;; is shared with ComfyUI + AllTalk).  0 = never unload.
  (idle-unload-minutes transcriber-configuration-idle-unload-minutes
                       (default 10))
  (max-upload-mb transcriber-configuration-max-upload-mb
                 (default 500))
  ;; SOPS-decrypted HF read token (owner must be the service user).  Read at
  ;; REQUEST time by the server, so creating the secret needs no restart.
  (hf-token-file transcriber-configuration-hf-token-file
                 (default "/run/secrets/hf-token"))

  ;; Paths.  Defaults are derived from service-name when #f.
  (state-dir transcriber-configuration-state-dir
             (default #f))
  (cache-dir transcriber-configuration-cache-dir
             (default #f))
  (log-dir transcriber-configuration-log-dir
           (default #f))
  (log-file transcriber-configuration-log-file
            (default #f))

  ;; Environment.  CUDA device 0 = the RTX 4090 on the comfyui VM.
  (cuda-visible-devices transcriber-configuration-cuda-visible-devices
                        (default "0"))
  (ld-library-paths transcriber-configuration-ld-library-paths
                    (default (list "/run/current-system/profile/lib"
                                   "/run/current-system/profile/lib64"
                                   ;; torchcodec (whisperx.load_audio) dlopens
                                   ;; libavcodec/libavutil/... through the venv
                                   ;; Python's Guix loader — LD_LIBRARY_PATH is
                                   ;; the only mechanism that reaches it (same
                                   ;; class of problem as opencv/glib in
                                   ;; comfyui.scm and portaudio in alltalk.scm).
                                   #~(string-append
                                      #$(@ (gnu packages video) ffmpeg)
                                      "/lib"))))
  (extra-environment-variables
   transcriber-configuration-extra-environment-variables
   (default (list)))

  ;; Extra Guix packages declared inside the FHS container (base-distro
  ;; names only — resolved by `guix shell' at runtime).  ffmpeg covers both
  ;; torchcodec's library dlopen (via ld-library-paths above) and any
  ;; ffmpeg-binary fallbacks.
  (container-extra-packages transcriber-configuration-container-extra-packages
                            (default (list (@ (gnu packages video) ffmpeg))))
  ;; Extra host paths writably bind-mounted / read-only exposed.
  (container-extra-shares transcriber-configuration-container-extra-shares
                          (default (list)))
  (container-extra-exposes transcriber-configuration-container-extra-exposes
                           (default (list)))
  ;; System-profile packages (uv lives here — uv-command's default).
  (runtime-packages transcriber-configuration-runtime-packages
                    (default (list)))

  (open-firewall? transcriber-configuration-open-firewall?
                  (default #f)))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (transcriber-resolved-state-dir config)
  (or (transcriber-configuration-state-dir config)
      (string-append "/var/lib/transcriber/"
                     (transcriber-configuration-service-name config))))

(define (transcriber-resolved-cache-dir config)
  (or (transcriber-configuration-cache-dir config)
      (string-append "/var/cache/transcriber/"
                     (transcriber-configuration-service-name config))))

(define (transcriber-resolved-log-dir config)
  (or (transcriber-configuration-log-dir config)
      (string-append "/var/log/transcriber")))

(define (transcriber-resolved-log-file config)
  (or (transcriber-configuration-log-file config)
      (string-append (transcriber-resolved-log-dir config) "/"
                     (transcriber-configuration-service-name config) ".log")))

(define (transcriber-resolved-sync-log-file config)
  (string-append (transcriber-resolved-log-dir config) "/sync.log"))

(define (transcriber-venv-dir config)
  (string-append (transcriber-configuration-project-dir config) "/.venv"))

(define (transcriber-uploads-dir config)
  (string-append (transcriber-resolved-state-dir config) "/uploads"))

;;; ── Environment rendering ────────────────────────────────────────────────

(define (transcriber-environment config)
  (let* ((state-dir (transcriber-resolved-state-dir config))
         (cache-dir (transcriber-resolved-cache-dir config))
         (hf-home (string-append cache-dir "/huggingface"))
         (uv-cache-dir (string-append cache-dir "/uv"))
         (xdg-cache-home (string-append cache-dir "/xdg"))
         (cuda-devices (transcriber-configuration-cuda-visible-devices config))
         (ld-paths (transcriber-configuration-ld-library-paths config)))
    (append (list (string-append "HOME=" state-dir)
             "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
             "UV_PYTHON_DOWNLOADS=never"
             ;; The Whisper/pyannote/wav2vec2 model zoo (~4 GB) downloads
             ;; here on first request — keep it on the data volume.
             (string-append "HF_HOME=" hf-home)
             "HF_HUB_DISABLE_TELEMETRY=1"
             (string-append "NLTK_DATA=" cache-dir "/nltk")
             (string-append "UV_CACHE_DIR=" uv-cache-dir)
             (string-append "XDG_CACHE_HOME=" xdg-cache-home))
            (if cuda-devices
                (list (string-append "CUDA_VISIBLE_DEVICES=" cuda-devices)
                      "CUDA_DEVICE_ORDER=PCI_BUS_ID"
                      "TRITON_LIBCUDA_PATH=/run/current-system/profile/lib")
                '())
            (if (null? ld-paths)
                '()
                (list #~(string-append "LD_LIBRARY_PATH="
                                       #$(join-paths-gexp ld-paths))))
            (transcriber-configuration-extra-environment-variables config))))

;;; ── FHS container wrapper ────────────────────────────────────────────────

;; Same mechanism and same sharp edges as alltalk/comfyui's runners (see
;; those modules): --expose=/gnu/store for raw store paths (venv interpreter,
;; uv-command, LD_LIBRARY_PATH entries, the server file), nvidia device nodes
;; discovered at wrapper runtime, container packages resolved by NAME against
;; the base distro only.  /run/secrets is exposed read-only so the daemon can
;; reach the sops-decrypted HF token; per-file permissions still apply (the
;; secret must be owned by the service user).
(define (transcriber-container-runner-file config suffix env inner-command)
  (let* ((service-name (transcriber-configuration-service-name config))
         (project-dir (transcriber-configuration-project-dir config))
         (state-dir (transcriber-resolved-state-dir config))
         (cache-dir (transcriber-resolved-cache-dir config))
         (uv-pkg (transcriber-configuration-uv-package config))
         (extra-pkgs (transcriber-configuration-container-extra-packages config))
         (extra-shares (transcriber-configuration-container-extra-shares config))
         (extra-exposes (transcriber-configuration-container-extra-exposes config))
         (package-names (map package-name
                             (append (list (@ (gnu packages base) coreutils)
                                           (@ (gnu packages bash) bash)
                                           (@ (gnu packages commencement) gcc-toolchain)
                                           (@ (gnu packages linux) linux-libre-headers)
                                           (@ (gnu packages nss) nss-certs)
                                           uv-pkg)
                                     extra-pkgs)))
         (static-args (append
                       (list "--container" "--emulate-fhs" "--network"
                             "--expose=/gnu/store"
                             (string-append "--share=" project-dir)
                             (string-append "--share=" state-dir)
                             (string-append "--share=" cache-dir)
                             "--expose=/run/current-system/profile"
                             "--expose=/run/secrets")
                       (map (lambda (s) (string-append "--share=" s)) extra-shares)
                       (map (lambda (s) (string-append "--expose=" s)) extra-exposes)
                       package-names))
         (guix-pkg (@ (gnu packages package-management) guix)))
    (program-file
     (string-append service-name suffix "-container-runner")
     #~(begin
         (use-modules (ice-9 ftw) (srfi srfi-1) (srfi srfi-13))
         ;; Preserve env by NAME only, never "VAR=VALUE" — see the long
         ;; comment in comfyui-container-runner-file.
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

;;; ── Sync steps ───────────────────────────────────────────────────────────

(define (transcriber-sync-steps-gexp config)
  (let* ((uv-command (transcriber-configuration-uv-command config))
         (project-dir (transcriber-configuration-project-dir config))
         (venv-dir (transcriber-venv-dir config))
         (python-pkg (transcriber-configuration-python-package config))
         (python-bin (file-append python-pkg "/bin/python3"))
         (venv-python (string-append venv-dir "/bin/python"))
         (torch-version (transcriber-configuration-torch-version config))
         (torchvision-version (transcriber-configuration-torchvision-version config))
         (torchaudio-version (transcriber-configuration-torchaudio-version config))
         (torch-index-url (transcriber-configuration-torch-index-url config))
         (pip-packages (transcriber-configuration-pip-packages config)))
    #~(begin
        (unless (file-exists? (string-append #$venv-dir "/pyvenv.cfg"))
          (let ((ret (system* #$uv-command "venv"
                              "--python" #$python-bin #$venv-dir)))
            (unless (zero? ret)
              (error "uv venv failed with exit code" ret))))
        ;; Torch first from the CUDA wheel index — so whisperx's
        ;; torch~=2.8.0 pin resolves against the cu128 build already
        ;; installed instead of PyPI's default (see the field comment).
        (let ((ret (system* #$uv-command "pip" "install"
                            "--python" #$venv-python
                            #$(string-append "torch==" torch-version)
                            #$(string-append "torchvision==" torchvision-version)
                            #$(string-append "torchaudio==" torchaudio-version)
                            "--index-url" #$torch-index-url)))
          (unless (zero? ret)
            (error "torch install failed with exit code" ret)))
        #$@(if (null? pip-packages)
               '()
               (list #~(let ((ret (apply system* #$uv-command "pip" "install"
                                         "--python" #$venv-python
                                         (list #$@pip-packages))))
                         (unless (zero? ret)
                           (error "pip-packages install failed with exit code"
                                  ret))))))))

(define (transcriber-sync-shepherd-service config)
  (let* ((service-name (transcriber-configuration-service-name config))
         (sync-name (string->symbol (string-append service-name "-sync")))
         (project-dir (transcriber-configuration-project-dir config))
         (user (transcriber-configuration-user config))
         (group (transcriber-configuration-group config))
         (log-file (transcriber-resolved-sync-log-file config))
         (python-pkg (transcriber-configuration-python-package config))
         (env (append (transcriber-environment config)
                      (list #~(string-append "UV_PYTHON="
                                             #$(file-append python-pkg
                                                            "/bin/python3")))))
         (runner (program-file (string-append service-name "-sync-runner")
                               #~(begin
                                   #$(transcriber-sync-steps-gexp config))))
         (container-runner (transcriber-container-runner-file config "-sync" env
                                                               (list runner))))
    (shepherd-service (provision (list sync-name))
                      (documentation (string-append
                                      "Bootstrap/update the transcriber uv "
                                      "environment for " service-name "."))
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

(define (transcriber-daemon-shepherd-service config)
  (let* ((service-name (transcriber-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (project-dir (transcriber-configuration-project-dir config))
         (user (transcriber-configuration-user config))
         (group (transcriber-configuration-group config))
         (log-file (transcriber-resolved-log-file config))
         (venv-dir (transcriber-venv-dir config))
         (server-file (transcriber-configuration-server-file config))
         (env (transcriber-environment config))
         (inner-command (list (string-append venv-dir "/bin/python")
                              server-file
                              "--port"
                              (number->string
                               (transcriber-configuration-port config))
                              "--model-size"
                              (transcriber-configuration-model-size config)
                              "--compute-type"
                              (transcriber-configuration-compute-type config)
                              "--batch-size"
                              (number->string
                               (transcriber-configuration-batch-size config))
                              "--idle-unload-minutes"
                              (number->string
                               (transcriber-configuration-idle-unload-minutes config))
                              "--max-upload-mb"
                              (number->string
                               (transcriber-configuration-max-upload-mb config))
                              "--hf-token-file"
                              (transcriber-configuration-hf-token-file config)
                              "--uploads-dir"
                              (transcriber-uploads-dir config)))
         (container-runner (transcriber-container-runner-file config "" env
                                                               inner-command)))
    (shepherd-service (provision (list provision-name))
                      (documentation (transcriber-configuration-documentation
                                      config))
                      (requirement '(networking file-systems))
                      (auto-start? (transcriber-configuration-auto-start? config))
                      (start #~(make-forkexec-constructor (list #$container-runner)
                                                          #:directory #$project-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env)))
                      (stop #~(make-kill-destructor)))))

(define (transcriber-shepherd-services configs)
  (append (map transcriber-sync-shepherd-service configs)
          (map transcriber-daemon-shepherd-service configs)))

;;; ── Accounts / activation / firewall / profile ──────────────────────────

(define (unique-configs-by proc configs)
  (fold-right (lambda (cfg acc)
                (if (any (lambda (existing)
                           (equal? (proc cfg)
                                   (proc existing))) acc) acc
                    (cons cfg acc)))
              '() configs))

(define (transcriber-accounts configs)
  (let* ((account-configs (filter transcriber-configuration-create-account?
                                  configs))
         (group-configs (unique-configs-by transcriber-configuration-group
                                           account-configs))
         (user-configs (unique-configs-by transcriber-configuration-user
                                          account-configs)))
    (append (map (lambda (config)
                   (user-group
                     (name (transcriber-configuration-group config))
                     (system? #t)))
                 group-configs)
            (map (lambda (config)
                   (user-account
                     (name (transcriber-configuration-user config))
                     (group (transcriber-configuration-group config))
                     (system? #t)
                     (comment "WhisperX transcriber daemon")
                     (home-directory (transcriber-resolved-state-dir config))
                     (supplementary-groups
                      (transcriber-configuration-supplementary-groups config))
                     (shell (file-append shadow "/sbin/nologin"))))
                 user-configs))))

(define (transcriber-instance-activation config)
  (let* ((user (transcriber-configuration-user config))
         (state-dir (transcriber-resolved-state-dir config))
         (cache-dir (transcriber-resolved-cache-dir config))
         (log-dir (transcriber-resolved-log-dir config))
         (project-dir (transcriber-configuration-project-dir config))
         (dirs (list state-dir
                     (transcriber-uploads-dir config)
                     cache-dir
                     (string-append cache-dir "/huggingface")
                     (string-append cache-dir "/uv")
                     (string-append cache-dir "/xdg")
                     (string-append cache-dir "/nltk")
                     log-dir
                     project-dir)))
    #~(let* ((pw (getpwnam #$user))
             (uid (passwd:uid pw))
             (gid (passwd:gid pw)))
        (for-each (lambda (dir)
                    (mkdir-p dir)
                    (chown dir uid gid))
                  (list #$@dirs)))))

(define (transcriber-activation configs)
  #~(begin
      (use-modules (guix build utils))
      #$@(map transcriber-instance-activation configs)))

(define (transcriber-firewall-rules configs)
  (nftables-rules
   (input (append-map (lambda (config)
                        (if (transcriber-configuration-open-firewall? config)
                            (list (string-append "tcp dport "
                                   (number->string
                                    (transcriber-configuration-port config))
                                   " accept comment \""
                                   (transcriber-configuration-service-name
                                    config) "\""))
                            '()))
                      configs))))

(define (transcriber-profile-entries configs)
  (delete-duplicates
   (append-map (lambda (config)
                 (filter (lambda (x) x)
                         (cons (transcriber-configuration-uv-package config)
                               (transcriber-configuration-runtime-packages
                                config))))
               configs)))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public transcriber-service-type
  (service-type (name 'transcriber)
                (compose append)
                (extend append)
                (extensions (list (service-extension account-service-type
                                                     transcriber-accounts)
                                  (service-extension activation-service-type
                                                     transcriber-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   transcriber-shepherd-services)
                                  (service-extension firewall-service-type
                                                     transcriber-firewall-rules)
                                  (service-extension profile-service-type
                                                     transcriber-profile-entries)))
                (default-value '())
                (description
                 "Run WhisperX transcription + speaker-diarization servers via uv-managed Python environments.")))
