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
  #:export (strata-configuration strata-configuration?
            strata-service-type))

;;; ── Utilities ────────────────────────────────────────────────────────────

(define (maybe-option flag value)
  (if value
      (list flag (if (number? value) (number->string value) value))
      '()))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <strata-configuration> strata-configuration
                     make-strata-configuration
  strata-configuration?
  (package strata-configuration-package
           (default strata))
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
  (context strata-configuration-context
           (default #f))
  (kv strata-configuration-kv
      (default #f))
  (vision strata-configuration-vision
          (default #f))
  (parallel strata-configuration-parallel
            (default #f))
  (extra-args strata-configuration-extra-args
              (default '()))

  ;; Strata's per-user state (runtime/, tuning/, profiles/, components/
  ;; — the engine download).  Kept off /var to survive a /var rebuild and
  ;; to keep the big engine package out of the root fs.
  (state-dir strata-configuration-state-dir
             (default "/var/lib/strata"))
  (log-file strata-configuration-log-file
            (default #f)))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (strata-resolved-log-file config)
  (or (strata-configuration-log-file config)
      (string-append "/var/log/strata/"
                     (strata-configuration-service-name config) ".log")))

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
         (script (strata-wrapper-script config)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw (getpwnam #$user))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          ;; State dirs: runtime/ + tuning/ under XDG_STATE_HOME, the
          ;; engine components/ under XDG_DATA_HOME, profiles/ under
          ;; XDG_CONFIG_HOME, plus the model data dir and the log dir.
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    (list #$state-dir
                          (string-append #$state-dir "/share")
                          (string-append #$state-dir "/state")
                          (string-append #$state-dir "/config")
                          (string-append #$state-dir "/state/strata/runtime")
                          #$log-dir
                          #$data-dir))
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
(define (strata-firewall-rules config)
  (if (strata-configuration-open-firewall? config)
      (nftables-rules
       (input (list (string-append "tcp dport "
                                   (number->string
                                    (strata-configuration-port config))
                                   " accept comment \""
                                   (strata-configuration-service-name
                                    config)
                                   "\""))))
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
                                                     strata-firewall-rules)))))