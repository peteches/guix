;; peteches/services/colibri.scm — Colibrì MoE inference engine service.
;;
;; Runs `coli serve` (peteches packages colibri) as a plain forkexec'd daemon
;; — no container needed, unlike comfyui.scm: the engine is a static-ish C
;; binary with a stdlib-only Python launcher, so there is no messy FHS-wheel
;; dependency tree to sandbox around.
;;
;; GPU contention with ComfyUI: `gpu` defaults to "none" here (still the
;; documented "canonical hard off-switch on every platform" per
;; docs/ENVIRONMENT.md), so plugging in the CPU-only `colibri-engine` keeps
;; this service structurally incapable of touching the GPU. Deployments that
;; use `colibri-engine-cuda` instead (see peteches/packages/colibri.scm) must
;; opt in explicitly by setting `gpu` and `vram-gb`; when they do, `vram-gb`
;; should be sized to leave real headroom for ComfyUI rather than trusting
;; `--gpu auto`'s own free-VRAM measurement, which knows nothing about what
;; ComfyUI will need later in the same boot.
;;
;; RAM: left at the engine's own auto budget (~88% of free RAM at startup,
;; self-trimmed against an RSS guard and an OOM pre-check) unless `ram-gb` is
;; set explicitly. On a box also running ComfyUI, pin `ram-gb` to leave
;; headroom rather than trusting the 88%-of-free measurement taken at
;; colibri's own startup, which knows nothing about what ComfyUI will need
;; later in the same boot.
;;
;; The model directory is never chowned recursively — colibri only ever
;; writes three flat files at its root (.coli_kv, .coli_usage, .coli_ssd), so
;; activation chowns just the top-level directory, not the (potentially
;; hundreds-of-GB) shard tree inside it. The directory and its model shards
;; must be placed there out of band (download/convert); this service does not
;; manage model acquisition.

(define-module (peteches services colibri)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (peteches packages colibri)
  #:use-module (peteches services firewall)
  #:export (colibri-configuration colibri-configuration? colibri-service-type))

;;; ── Utilities ────────────────────────────────────────────────────────────

(define (maybe-option flag value)
  (if value
      (list flag (if (number? value) (number->string value) value))
      '()))

(define (repeated-option flag values)
  (append-map (lambda (v) (list flag v)) values))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <colibri-configuration> colibri-configuration
                     make-colibri-configuration
  colibri-configuration?
  (package colibri-configuration-package
           (default colibri-engine))
  (service-name colibri-configuration-service-name
                (default "colibri"))
  (documentation colibri-configuration-documentation
                 (default "Colibrì MoE inference server."))
  ;; Off by default: obtaining/converting a model is a large, manual,
  ;; out-of-band step, and a boot-time crash loop over a missing model helps
  ;; no one. Flip on once model-dir is actually populated.
  (auto-start? colibri-configuration-auto-start?
               (default #f))

  (create-account? colibri-configuration-create-account?
                   (default #t))
  (user colibri-configuration-user
        (default "colibri"))
  (group colibri-configuration-group
         (default "colibri"))

  ;; Directory holding the converted model shards (config/tokenizer/
  ;; *.safetensors). Must exist and be populated before auto-start? is
  ;; enabled; see the module comment on why it is chowned non-recursively.
  (model-dir colibri-configuration-model-dir)
  ;; Optional second copy of the model on another drive — split reads across
  ;; both (COLI_MODEL_MIRROR).
  (model-mirror colibri-configuration-model-mirror
                (default #f))
  ;; O_DIRECT/unbuffered reads for expert slabs (DIRECT). Upstream: "drive-
  ;; dependent — measure it on your hardware" — a real win on NVMe with DRAM
  ;; cache and headroom, neutral-to-negative on QLC/DRAM-less drives or slow/
  ;; virtualised disks. Recommended specifically when model-mirror is set, so
  ;; the primary and mirror reads don't compete for page cache. Default #f
  ;; matches the engine's own default (buffered reads).
  (direct-io? colibri-configuration-direct-io?
              (default #f))
  (model-id colibri-configuration-model-id
            (default #f))

  (host colibri-configuration-host
        (default "127.0.0.1"))
  (port colibri-configuration-port
        (default 8000))

  ;; Hard off-switch for GPU use, kept "none" here deliberately — see the
  ;; module comment. Has no effect on the CPU-only `colibri-engine` (no
  ;; backend is compiled in to enable); only matters when `package` is set to
  ;; `colibri-engine-cuda`.
  (gpu colibri-configuration-gpu
       (default "none"))
  ;; VRAM budget in GB for the GPU expert-cache tier (--vram; 0 = auto-size
  ;; from free VRAM). Only meaningful with `colibri-engine-cuda` and `gpu` set
  ;; to something other than "none". #f omits the flag (engine default: no
  ;; VRAM tier unless --auto-tier is also passed).
  (vram-gb colibri-configuration-vram-gb
           (default #f))
  ;; CUDA_VISIBLE_DEVICES for the GPU expert-cache tier. Only applied (as an
  ;; environment variable) when `gpu` is not "none".
  (cuda-visible-devices colibri-configuration-cuda-visible-devices
                        (default "0"))

  ;; RAM budget in GB for the resident/streamed expert working set. #f = the
  ;; engine's own auto-sizing (~88% of free RAM at startup). See the module
  ;; comment: pin this explicitly to coexist predictably with ComfyUI.
  (ram-gb colibri-configuration-ram-gb
          (default #f))
  (ctx colibri-configuration-ctx
       (default #f))
  (kv-slots colibri-configuration-kv-slots
            (default 1))
  (max-queue colibri-configuration-max-queue
             (default 8))
  (queue-timeout colibri-configuration-queue-timeout
                 (default 300))

  ;; A file containing the bearer API key expected of clients (read at
  ;; service start, like nzbget's password-file). #f disables auth.
  (api-key-file colibri-configuration-api-key-file
                (default #f))

  (cors-origins colibri-configuration-cors-origins
                (default '()))
  (allowed-hosts colibri-configuration-allowed-hosts
                (default '()))

  (extra-args colibri-configuration-extra-args
              (default '()))
  (extra-environment-variables
   colibri-configuration-extra-environment-variables
   (default '()))

  (state-dir colibri-configuration-state-dir
             (default #f))
  (log-file colibri-configuration-log-file
            (default #f))

  ;; Only open the firewall when intentionally serving beyond localhost.
  (open-firewall? colibri-configuration-open-firewall?
                  (default #f)))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (colibri-resolved-state-dir config)
  (or (colibri-configuration-state-dir config)
      (string-append "/var/lib/colibri/"
                     (colibri-configuration-service-name config))))

(define (colibri-resolved-log-file config)
  (or (colibri-configuration-log-file config)
      (string-append "/var/log/colibri/"
                     (colibri-configuration-service-name config) ".log")))

;;; ── coli invocation ──────────────────────────────────────────────────────

(define (colibri-serve-args config)
  (append (list "serve"
                "--model" (colibri-configuration-model-dir config)
                "--host" (colibri-configuration-host config)
                "--port" (number->string (colibri-configuration-port config))
                "--gpu" (colibri-configuration-gpu config)
                "--kv-slots" (number->string (colibri-configuration-kv-slots
                                              config))
                "--max-queue" (number->string (colibri-configuration-max-queue
                                               config))
                "--queue-timeout" (number->string
                                    (colibri-configuration-queue-timeout
                                     config)))
          (maybe-option "--ram" (colibri-configuration-ram-gb config))
          (maybe-option "--vram" (colibri-configuration-vram-gb config))
          (maybe-option "--ctx" (colibri-configuration-ctx config))
          (maybe-option "--model-id" (colibri-configuration-model-id config))
          (repeated-option "--cors-origin"
                           (colibri-configuration-cors-origins config))
          (repeated-option "--allowed-host"
                           (colibri-configuration-allowed-hosts config))
          (colibri-configuration-extra-args config)))

(define (colibri-environment config)
  (let ((state-dir (colibri-resolved-state-dir config))
        (model-mirror (colibri-configuration-model-mirror config))
        (direct-io? (colibri-configuration-direct-io? config))
        (gpu (colibri-configuration-gpu config))
        (cuda-devices (colibri-configuration-cuda-visible-devices config)))
    (append (list (string-append "HOME=" state-dir)
                  "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin")
            (if model-mirror
                (list (string-append "COLI_MODEL_MIRROR=" model-mirror))
                '())
            (if direct-io?
                (list "DIRECT=1")
                '())
            ;; libcuda.so.1 (the driver's userspace lib, not part of
            ;; cuda-toolkit) lives under the system profile via nonguix's
            ;; nvidia driver — same paths ComfyUI's CUDA/Triton setup already
            ;; relies on for this machine (see peteches/services/comfyui.scm).
            (if (not (string=? gpu "none"))
                (list (string-append "CUDA_VISIBLE_DEVICES=" cuda-devices)
                      "CUDA_DEVICE_ORDER=PCI_BUS_ID"
                      "LD_LIBRARY_PATH=/run/current-system/profile/lib:/run/current-system/profile/lib64")
                '())
            (colibri-configuration-extra-environment-variables config))))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (colibri-accounts config)
  (if (colibri-configuration-create-account? config)
      (list (user-group (name (colibri-configuration-group config))
                        (system? #t))
            (user-account (name (colibri-configuration-user config))
                          (group (colibri-configuration-group config))
                          (system? #t)
                          (comment "Colibrì inference server daemon")
                          (home-directory (colibri-resolved-state-dir config))
                          (shell (file-append shadow "/sbin/nologin"))))
      '()))

(define (colibri-activation config)
  (let* ((user (colibri-configuration-user config))
         (state-dir (colibri-resolved-state-dir config))
         (log-file (colibri-resolved-log-file config))
         (log-dir (dirname log-file))
         (model-dir (colibri-configuration-model-dir config))
         (model-mirror (colibri-configuration-model-mirror config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw (getpwnam #$user))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    (list #$state-dir #$log-dir))
          ;; Non-recursive on purpose — see the module comment.
          (mkdir-p #$model-dir)
          (chown #$model-dir uid gid)
          #$@(if model-mirror
                 (list #~(begin
                           (mkdir-p #$model-mirror)
                           (chown #$model-mirror uid gid)))
                 '())))))

(define (colibri-shepherd-service config)
  (let* ((service-name (colibri-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (pkg (colibri-configuration-package config))
         (bin (file-append pkg "/bin/coli"))
         (user (colibri-configuration-user config))
         (group (colibri-configuration-group config))
         (log-file (colibri-resolved-log-file config))
         (api-key-file (colibri-configuration-api-key-file config))
         (env (colibri-environment config))
         (args (colibri-serve-args config)))
    (list (shepherd-service
           (provision (list provision-name))
           (documentation (colibri-configuration-documentation config))
           ;; sops-secrets' own requirement is user-processes, a fairly
           ;; late boot milestone, not networking/file-systems — it never
           ;; runs on its own in time unless something actually depends
           ;; on it. Without this, reading api-key-file before it's been
           ;; decrypted fails as "open-fdes: No such file or directory"
           ;; from inside call-with-input-file (hit for real with the
           ;; identical bug in sillytavern.scm's daemon service).
           (requirement (if api-key-file
                            '(networking file-systems sops-secrets)
                            '(networking file-systems)))
           (auto-start? (colibri-configuration-auto-start? config))
           ;; The key goes in via COLI_API_KEY (coli serve's own env-var
           ;; fallback for --api-key), never as a CLI argument: shepherd's
           ;; own `herd status` prints the full argv verbatim, which would
           ;; put the plaintext key on screen (and in scrollback, logs,
           ;; anything that captures that output) every time anyone checks
           ;; on the service. Environment variables don't appear there.
           (start #~(lambda _
                      (let* ((key-file #$api-key-file)
                             (key-env (if key-file
                                          (list
                                           (string-append
                                            "COLI_API_KEY="
                                            (string-trim-right
                                             (call-with-input-file key-file
                                               (@ (ice-9 rdelim) read-line)))))
                                          '())))
                        ((make-forkexec-constructor
                          (append (list #$bin #$@args))
                          #:user #$user
                          #:group #$group
                          #:log-file #$log-file
                          #:environment-variables
                          (append (list #$@env) key-env))))))
           (stop #~(make-kill-destructor))))))

(define (colibri-firewall-rules config)
  (if (colibri-configuration-open-firewall? config)
      (nftables-rules
       (input (list (string-append "tcp dport "
                                   (number->string (colibri-configuration-port
                                                    config))
                                   " accept comment \""
                                   (colibri-configuration-service-name config)
                                   "\""))))
      (nftables-rules (input '()))))

(define (colibri-profile config)
  ;; glibc for `ldd`: coli's Linux CUDA-detection (cuda_binary() in the coli
  ;; script) shells out to `ldd` on the engine binary and treats any failure
  ;; to even run it — including a plain "not found on PATH" — as "not a
  ;; CUDA build", silently falling back to CPU-only. The shepherd service's
  ;; #:environment-variables replaces the process environment outright
  ;; rather than extending it, so PATH must actually carry everything the
  ;; engine or its launcher shells out to, not just the engine itself.
  (list (colibri-configuration-package config) glibc))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public colibri-service-type
  (service-type (name 'colibri)
                (description
                 "Run the Colibrì MoE inference server (`coli serve`).")
                (extensions (list (service-extension account-service-type
                                                     colibri-accounts)
                                  (service-extension activation-service-type
                                                     colibri-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   colibri-shepherd-service)
                                  (service-extension firewall-service-type
                                                     colibri-firewall-rules)
                                  (service-extension profile-service-type
                                                     colibri-profile)))))
