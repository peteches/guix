;;; peteches/services/llama-server.scm
;;;
;;; llama.cpp `llama-server` (OpenAI-compatible) as a system service, run
;;; on the Intel Arc B70 via ggml's Vulkan backend.
;;;
;;; The default package is (peteches packages llama-cpp-qwen4)'s
;;; llama-cpp-qwen4: llama.cpp at b12211 (the stock channel package is
;;; b10318 and rejects this model's `qwen4exp' architecture), built with
;;; the *vendored* ggml (LLAMA_USE_SYSTEM_GGML=OFF — b12211 needs the
;;; newer ggml API) and -DGGML_VULKAN=ON, so the Vulkan backend is a
;;; libggml-vulkan.so built into that package's own lib dir.  ggml
;;; dlopens backends from next to the main libggml.so, so no
;;; GGML_BACKEND_DIR wiring is needed.
;;;
;;; This is the replacement for the strata engine on the comfyui box: the
;;; Q4_K_XL-V2 abliterated GGUF is a *standard* GGUF (no GSQ-RCO native
;;; pack, no MTP/spec), so the strata service's native-pack machinery does
;;; not apply.  llama.cpp's `llama-server` loads the GGUF directly and
;;; exposes the OpenAI `/v1/chat/completions` API that pi drives.
;;;
;;; Vulkan ICD discovery: the (stock Khronos) Vulkan loader reads
;;; VK_ADD_DRIVER_FILES (a colon-separated list of *extra directories* to
;;; search for ICD manifests) to find the Mesa ANV driver
;;; (intel_icd.x86_64.json).  A dedicated-account shepherd service does NOT
;;; inherit the profile environment, so we set VK_ADD_DRIVER_FILES
;;; explicitly here, pointing at the profile's share/vulkan/icd.d
;;; directory.  Without this the loader only searches its compiled-in
;;; paths (/usr/share/vulkan/icd.d, absent on Guix) and finds no driver.
;;;
;;; auto-start? defaults to #f: the model file is a large, manually-placed,
;;; out-of-band artifact, and a boot-time crash loop over a missing model
;;; helps no one.  Start it explicitly with `herd start llama-server`.
(define-module (peteches services llama-server)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module (peteches packages llama-cpp-qwen4)
  #:use-module (peteches services firewall)
  #:export (llama-server-configuration llama-server-configuration?
            llama-server-service-type))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <llama-server-configuration> llama-server-configuration
                     make-llama-server-configuration
  llama-server-configuration?
  (package llama-server-configuration-package
           (default llama-cpp-qwen4))
  (service-name llama-server-configuration-service-name
                (default "llama-server"))
  (documentation llama-server-configuration-documentation
                 (default "llama.cpp llama-server (OpenAI-compatible, Vulkan)."))
  (auto-start? llama-server-configuration-auto-start?
               (default #f))

  (create-account? llama-server-configuration-create-account?
                   (default #t))
  (user llama-server-configuration-user
        (default "llama-server"))
  (group llama-server-configuration-group
         (default "llama-server"))

  ;; Full path to the .gguf model file (out-of-band artifact on
  ;; /media/models).  Required.
  (model-path llama-server-configuration-model-path)

  (host llama-server-configuration-host
        (default "127.0.0.1"))
  (port llama-server-configuration-port
        (default 8000))

  ;; --ctx-size.  #f omits the flag (llama-server's own default applies).
  (ctx-size llama-server-configuration-ctx-size
            (default #f))
  ;; --n-gpu-layers.  #f omits the flag (llama-server defaults to
  ;; offloading everything it can).  For a 32GB B70 serving a ~105GB
  ;; MoE model, the layers that fit in VRAM are offloaded and the rest
  ;; are mmap'd from disk (file cache) — same architecture as the strata
  ;; engine's --mmap-experts.
  (gpu-layers llama-server-configuration-gpu-layers
              (default #f))
  ;; --threads.  #f omits the flag.
  (threads llama-server-configuration-threads
           (default #f))
  ;; --parallel.  #f omits the flag.
  (parallel llama-server-configuration-parallel
            (default #f))

  (extra-args llama-server-configuration-extra-args
              (default '()))
  (extra-environment-variables
   llama-server-configuration-extra-environment-variables
   (default '()))

  (state-dir llama-server-configuration-state-dir
             (default #f))
  (log-file llama-server-configuration-log-file
            (default #f))

  ;; Only open the firewall when intentionally serving beyond localhost.
  (open-firewall? llama-server-configuration-open-firewall?
                  (default #f)))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (llama-server-resolved-state-dir config)
  (or (llama-server-configuration-state-dir config)
      (string-append "/var/lib/llama-server/"
                     (llama-server-configuration-service-name config))))

(define (llama-server-resolved-log-file config)
  (or (llama-server-configuration-log-file config)
      (string-append "/var/log/llama-server/"
                     (llama-server-configuration-service-name config) ".log")))

;;; ── llama-server invocation ──────────────────────────────────────────────

(define (maybe-option flag value)
  (if value
      (list flag (if (number? value) (number->string value) value))
      '()))

(define (llama-server-args config)
  (append (list "--host" (llama-server-configuration-host config)
                "--port" (number->string (llama-server-configuration-port
                                          config))
                "--model" (llama-server-configuration-model-path config))
          (maybe-option "--ctx-size"
                        (llama-server-configuration-ctx-size config))
          (maybe-option "--n-gpu-layers"
                        (llama-server-configuration-gpu-layers config))
          (maybe-option "--threads"
                        (llama-server-configuration-threads config))
          (maybe-option "--parallel"
                        (llama-server-configuration-parallel config))
          (llama-server-configuration-extra-args config)))

(define (llama-server-environment config)
  (let ((state-dir (llama-server-resolved-state-dir config)))
    (append (list (string-append "HOME=" state-dir)
                  "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin"
                  ;; The Guix-patched Vulkan loader reads this to find the
                  ;; Mesa ANV driver (intel_icd.x86_64.json).  The profile
                  ;; environment sets it, but a dedicated-account shepherd
                  ;; service does not inherit the profile environment, so
                  ;; set it explicitly.  Point at the profile's icd.d
                  ;; directory — the loader reads every *.json in it.
                  "VK_ADD_DRIVER_FILES=/run/current-system/profile/share/vulkan/icd.d"
                  ;; Make sure the loader (and the ANV ICD it loads) can
                  ;; find their shared libraries in the profile.
                  "LD_LIBRARY_PATH=/run/current-system/profile/lib")
            (llama-server-configuration-extra-environment-variables config))))

;;; ── Service extension helpers ────────────────────────────────────────────

(define (llama-server-accounts config)
  (if (llama-server-configuration-create-account? config)
      (list (user-group (name (llama-server-configuration-group config))
                        (system? #t))
            (user-account (name (llama-server-configuration-user config))
                          (group (llama-server-configuration-group config))
                          (system? #t)
                          (comment "llama.cpp llama-server daemon")
                          (home-directory (llama-server-resolved-state-dir
                                           config))
                          (shell (file-append shadow "/sbin/nologin"))))
      '()))

(define (llama-server-activation config)
  (let* ((user (llama-server-configuration-user config))
         (state-dir (llama-server-resolved-state-dir config))
         (log-file (llama-server-resolved-log-file config))
         (log-dir (dirname log-file)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw (getpwnam #$user))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    (list #$state-dir #$log-dir))))))

(define (llama-server-shepherd-service config)
  (let* ((service-name (llama-server-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (pkg (llama-server-configuration-package config))
         (bin (file-append pkg "/bin/llama-server"))
         (user (llama-server-configuration-user config))
         (group (llama-server-configuration-group config))
         (log-file (llama-server-resolved-log-file config))
         (env (llama-server-environment config))
         (args (llama-server-args config)))
    (list (shepherd-service
           (provision (list provision-name))
           (documentation (llama-server-configuration-documentation config))
           (requirement '(networking file-systems))
           (auto-start? (llama-server-configuration-auto-start? config))
           (start #~(make-forkexec-constructor
                     (append (list #$bin) (list #$@args))
                     #:user #$user
                     #:group #$group
                     #:log-file #$log-file
                     ;; The vendored ggml's Vulkan backend (libggml-vulkan.so)
                     ;; is built into the package's own lib dir, next to
                     ;; libggml.so; ggml dlopens backends from there, so no
                     ;; GGML_BACKEND_DIR wiring is needed.
                     #:environment-variables (list #$@env)))
           (stop #~(make-kill-destructor))))))

(define (llama-server-firewall-rules config)
  (if (llama-server-configuration-open-firewall? config)
      (nftables-rules
       (input (list (string-append "tcp dport "
                                   (number->string
                                    (llama-server-configuration-port config))
                                   " accept comment \""
                                   (llama-server-configuration-service-name
                                    config)
                                   "\""))))
      (nftables-rules (input '()))))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public llama-server-service-type
  (service-type (name 'llama-server)
                (description
                 "Run llama.cpp llama-server (Vulkan) as a system service.")
                (extensions (list (service-extension account-service-type
                                                     llama-server-accounts)
                                  (service-extension activation-service-type
                                                     llama-server-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   llama-server-shepherd-service)
                                  (service-extension firewall-service-type
                                                     llama-server-firewall-rules)))))