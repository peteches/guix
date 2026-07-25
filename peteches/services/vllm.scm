;; peteches/services/vllm.scm — Guix service type for vLLM via uv.
;;
;; This deliberately does not package vLLM as a Guix package.  Guix owns the
;; service boundary, accounts, paths, environment, ports and supervision; uv owns
;; the Python/vLLM wheel environment declared by pyproject.toml + uv.lock.

(define-module (peteches services vllm)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (srfi srfi-1)
  #:use-module (peteches services firewall)
  #:export (vllm-configuration vllm-configuration?
                               vllm-service-type))

;;; ── Utilities ────────────────────────────────────────────────────────────

(define (maybe-option flag value)
  (if value
      (list flag (if (number? value) (number->string value) value))
      '()))

(define (maybe-flag flag enabled?)
  (if enabled? (list flag) '()))

(define (join-paths paths)
  (if (null? paths)
      ""
      (let loop ((rest (cdr paths))
                 (acc (car paths)))
        (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc ":" (car rest)))))))

(define (unique-configs-by proc configs)
  (fold-right (lambda (cfg acc)
                (if (any (lambda (existing)
                           (equal? (proc cfg) (proc existing)))
                         acc)
                    acc
                    (cons cfg acc)))
              '()
              configs))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <vllm-configuration> vllm-configuration
                     make-vllm-configuration
  vllm-configuration?
  ;; Shepherd identity.  Use several instances with auto-start? #f, then start
  ;; exactly one instance per workflow lane from your hotswap controller.
  (service-name vllm-configuration-service-name
                (default "vllm-code-agent"))
  (documentation vllm-configuration-documentation
                 (default "vLLM OpenAI-compatible model server."))
  (auto-start? vllm-configuration-auto-start?
               (default #f))

  ;; System account.  Multiple instances can share the same account; the service
  ;; type de-duplicates account declarations across configured instances.
  (create-account? vllm-configuration-create-account?
                   (default #t))
  (user vllm-configuration-user
        (default "vllm"))
  (group vllm-configuration-group
         (default "vllm"))
  (supplementary-groups vllm-configuration-supplementary-groups
                        (default '("video")))

  ;; uv runtime.  Keep uv and Python in the Guix system profile, and keep the
  ;; Python dependency graph in pyproject.toml + uv.lock under uv-project-dir.
  (uv-command vllm-configuration-uv-command
              (default "/run/current-system/profile/bin/uv"))
  (uv-project-dir vllm-configuration-uv-project-dir
                  (default "/srv/llm/vllm"))
  (uv-run-args vllm-configuration-uv-run-args
               (default '("--frozen" "--no-sync")))
  (uv-sync-args vllm-configuration-uv-sync-args
                (default '("--frozen")))
  ;; When set, uv will use this virtualenv path instead of <project>/.venv.
  (uv-project-environment vllm-configuration-uv-project-environment
                          (default #f))
  ;; Packages installed into the Guix system profile for this service, e.g.
  ;; `(list uv python)` from the modules your pinned channels provide.
  (runtime-packages vllm-configuration-runtime-packages
                    (default '()))

  ;; vLLM serving identity.
  (model vllm-configuration-model)
  (served-model-name vllm-configuration-served-model-name
                     (default "code-agent"))
  (host vllm-configuration-host
        (default "127.0.0.1"))
  (port vllm-configuration-port
        (default 8001))

  ;; vLLM model/runtime flags.  Generation defaults such as temperature/top-p
  ;; should normally live in the gateway/client profile, not here.
  (max-model-len vllm-configuration-max-model-len
                 (default #f))
  (gpu-memory-utilization vllm-configuration-gpu-memory-utilization
                          (default #f))
  (tensor-parallel-size vllm-configuration-tensor-parallel-size
                        (default #f))
  (dtype vllm-configuration-dtype
         (default #f))
  (quantization vllm-configuration-quantization
                (default #f))
  (device vllm-configuration-device
          (default #f))
  (trust-remote-code? vllm-configuration-trust-remote-code?
                      (default #f))
  (enable-prefix-caching? vllm-configuration-enable-prefix-caching?
                          (default #f))
  (disable-log-requests? vllm-configuration-disable-log-requests?
                         (default #f))
  (extra-args vllm-configuration-extra-args
              (default '()))

  ;; Paths.  Defaults are resolved from service-name when the field is #f.
  (state-dir vllm-configuration-state-dir
             (default #f))
  (cache-dir vllm-configuration-cache-dir
             (default #f))
  (log-dir vllm-configuration-log-dir
           (default #f))
  (log-file vllm-configuration-log-file
            (default #f))

  ;; Environment.  These defaults are intentionally practical for CUDA wheels on
  ;; Guix/Nonguix.  Override/extend in extra-environment-variables when needed.
  (cuda-visible-devices vllm-configuration-cuda-visible-devices
                        (default "0"))
  (ld-library-paths vllm-configuration-ld-library-paths
                    (default '("/run/current-system/profile/lib"
                               "/run/current-system/profile/lib64")))
  (extra-environment-variables
   vllm-configuration-extra-environment-variables
   (default '()))

  ;; By default vLLM is intended to be consumed locally by a gateway.  Only open
  ;; the firewall when intentionally binding beyond localhost.
  (open-firewall? vllm-configuration-open-firewall?
                  (default #f)))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (vllm-resolved-state-dir config)
  (or (vllm-configuration-state-dir config)
      (string-append "/var/lib/vllm/"
                     (vllm-configuration-service-name config))))

(define (vllm-resolved-cache-dir config)
  (or (vllm-configuration-cache-dir config)
      (string-append "/var/cache/vllm/"
                     (vllm-configuration-service-name config))))

(define (vllm-resolved-log-dir config)
  (or (vllm-configuration-log-dir config)
      (string-append "/var/log/vllm/"
                     (vllm-configuration-service-name config))))

(define (vllm-resolved-log-file config)
  (or (vllm-configuration-log-file config)
      (string-append (vllm-resolved-log-dir config) "/server.log")))

(define (vllm-resolved-sync-log-file config)
  (string-append (vllm-resolved-log-dir config) "/sync.log"))

(define (vllm-resolved-uv-project-environment config)
  (vllm-configuration-uv-project-environment config))

;;; ── vLLM command/environment rendering ───────────────────────────────────

(define (vllm-serve-args config)
  (append (list "serve"
                (vllm-configuration-model config)
                "--host"
                (vllm-configuration-host config)
                "--port"
                (number->string (vllm-configuration-port config))
                "--served-model-name"
                (vllm-configuration-served-model-name config))
          (maybe-option "--max-model-len"
                        (vllm-configuration-max-model-len config))
          (maybe-option "--gpu-memory-utilization"
                        (vllm-configuration-gpu-memory-utilization config))
          (maybe-option "--tensor-parallel-size"
                        (vllm-configuration-tensor-parallel-size config))
          (maybe-option "--dtype"
                        (vllm-configuration-dtype config))
          (maybe-option "--quantization"
                        (vllm-configuration-quantization config))
          (maybe-option "--device"
                        (vllm-configuration-device config))
          (maybe-flag "--trust-remote-code"
                      (vllm-configuration-trust-remote-code? config))
          (maybe-flag "--enable-prefix-caching"
                      (vllm-configuration-enable-prefix-caching? config))
          (maybe-flag "--disable-log-requests"
                      (vllm-configuration-disable-log-requests? config))
          (vllm-configuration-extra-args config)))

(define (vllm-environment config)
  (let* ((state-dir (vllm-resolved-state-dir config))
         (cache-dir (vllm-resolved-cache-dir config))
         (hf-home (string-append cache-dir "/huggingface"))
         (vllm-cache-root (string-append cache-dir "/vllm"))
         (uv-cache-dir (string-append cache-dir "/uv"))
         (xdg-cache-home (string-append cache-dir "/xdg"))
         (cuda-devices (vllm-configuration-cuda-visible-devices config))
         (ld-paths (vllm-configuration-ld-library-paths config))
         (uv-env (vllm-resolved-uv-project-environment config)))
    (append (list (string-append "HOME=" state-dir)
                  "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin:/run/setuid-programs"
                  "UV_PYTHON_DOWNLOADS=never"
                  (string-append "HF_HOME=" hf-home)
                  (string-append "VLLM_CACHE_ROOT=" vllm-cache-root)
                  (string-append "UV_CACHE_DIR=" uv-cache-dir)
                  (string-append "XDG_CACHE_HOME=" xdg-cache-home))
            (if cuda-devices
                (list (string-append "CUDA_VISIBLE_DEVICES=" cuda-devices)
                      "CUDA_DEVICE_ORDER=PCI_BUS_ID")
                '())
            (if (null? ld-paths)
                '()
                (list (string-append "LD_LIBRARY_PATH="
                                     (join-paths ld-paths))))
            (if uv-env
                (list (string-append "UV_PROJECT_ENVIRONMENT=" uv-env))
                '())
            (vllm-configuration-extra-environment-variables config))))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (vllm-accounts configs)
  (let* ((account-configs (filter vllm-configuration-create-account? configs))
         (group-configs (unique-configs-by vllm-configuration-group
                                           account-configs))
         (user-configs (unique-configs-by vllm-configuration-user
                                          account-configs)))
    (append (map (lambda (config)
                   (user-group
                     (name (vllm-configuration-group config))
                     (system? #t)))
                 group-configs)
            (map (lambda (config)
                   (user-account
                     (name (vllm-configuration-user config))
                     (group (vllm-configuration-group config))
                     (system? #t)
                     (comment "vLLM model server daemon")
                     (home-directory (vllm-resolved-state-dir config))
                     (supplementary-groups
                      (vllm-configuration-supplementary-groups config))
                     (shell (file-append shadow "/sbin/nologin"))))
                 user-configs))))

(define (vllm-instance-activation config)
  (let* ((user (vllm-configuration-user config))
         (state-dir (vllm-resolved-state-dir config))
         (cache-dir (vllm-resolved-cache-dir config))
         (log-dir (vllm-resolved-log-dir config))
         (uv-project-dir (vllm-configuration-uv-project-dir config))
         (uv-env (vllm-resolved-uv-project-environment config))
         (dirs (append (list state-dir cache-dir log-dir uv-project-dir)
                       (if uv-env (list uv-env) '()))))
    #~(let* ((pw (getpwnam #$user))
             (uid (passwd:uid pw))
             (gid (passwd:gid pw)))
        (for-each (lambda (dir)
                    (mkdir-p dir)
                    (chown dir uid gid))
                  (list #$@dirs)))))

(define (vllm-activation configs)
  #~(begin
      (use-modules (guix build utils))
      #$@(map vllm-instance-activation configs)))

(define (vllm-sync-shepherd-service config)
  (let* ((service-name (vllm-configuration-service-name config))
         (sync-name (string->symbol (string-append service-name "-sync")))
         (uv-command (vllm-configuration-uv-command config))
         (uv-sync-args (vllm-configuration-uv-sync-args config))
         (uv-project-dir (vllm-configuration-uv-project-dir config))
         (user (vllm-configuration-user config))
         (group (vllm-configuration-group config))
         (log-file (vllm-resolved-sync-log-file config))
         (env (vllm-environment config)))
    (shepherd-service
     (provision (list sync-name))
     (documentation (string-append "Synchronise the uv environment for "
                                   service-name "."))
     (requirement '(networking file-systems))
     (one-shot? #t)
     (auto-start? #f)
     (start #~(make-forkexec-constructor (list #$uv-command
                                               "sync"
                                               #$@uv-sync-args)
                                         #:directory #$uv-project-dir
                                         #:user #$user
                                         #:group #$group
                                         #:log-file #$log-file
                                         #:environment-variables
                                         (list #$@env)))
     (stop #~(lambda _ #t)))))

(define (vllm-daemon-shepherd-service config)
  (let* ((service-name (vllm-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (uv-command (vllm-configuration-uv-command config))
         (uv-run-args (vllm-configuration-uv-run-args config))
         (uv-project-dir (vllm-configuration-uv-project-dir config))
         (user (vllm-configuration-user config))
         (group (vllm-configuration-group config))
         (log-file (vllm-resolved-log-file config))
         (env (vllm-environment config))
         (serve-args (vllm-serve-args config)))
    (shepherd-service
     (provision (list provision-name))
     (documentation (vllm-configuration-documentation config))
     (requirement '(networking file-systems))
     (auto-start? (vllm-configuration-auto-start? config))
     (start #~(make-forkexec-constructor (list #$uv-command
                                               "run"
                                               #$@uv-run-args
                                               "vllm"
                                               #$@serve-args)
                                         #:directory #$uv-project-dir
                                         #:user #$user
                                         #:group #$group
                                         #:log-file #$log-file
                                         #:environment-variables
                                         (list #$@env)))
     (stop #~(make-kill-destructor)))))

(define (vllm-shepherd-services configs)
  (append (map vllm-sync-shepherd-service configs)
          (map vllm-daemon-shepherd-service configs)))

(define (vllm-firewall-rules configs)
  (nftables-rules
   (input (append-map (lambda (config)
                        (if (vllm-configuration-open-firewall? config)
                            (list (string-append "tcp dport "
                                                 (number->string
                                                  (vllm-configuration-port config))
                                                 " accept comment \""
                                                 (vllm-configuration-service-name
                                                  config)
                                                 "\""))
                            '()))
                      configs))))

(define (vllm-profile-entries configs)
  (delete-duplicates
   (append-map vllm-configuration-runtime-packages configs)))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public vllm-service-type
  (service-type (name 'vllm)
                (compose append)
                (extend append)
                (extensions (list (service-extension account-service-type
                                                     vllm-accounts)
                                  (service-extension activation-service-type
                                                     vllm-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   vllm-shepherd-services)
                                  (service-extension firewall-service-type
                                                     vllm-firewall-rules)
                                  (service-extension profile-service-type
                                                     vllm-profile-entries)))
                (default-value '())
                (description
                 "Run one or more vLLM OpenAI-compatible servers via uv-managed Python environments.")))
