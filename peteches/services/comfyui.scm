;; peteches/services/comfyui.scm — Guix service type for ComfyUI via uv.
;;
;; This deliberately does not package ComfyUI as a Guix package.  Guix owns the
;; service boundary, accounts, paths, environment, ports and supervision; uv owns
;; the Python/ComfyUI wheel environment declared by pyproject.toml + uv.lock.
;;
;; The ComfyUI source tree (git clone of https://github.com/Comfy-Org/ComfyUI)
;; must live at uv-project-dir with a pyproject.toml and uv.lock already in
;; place before the service starts.
;;
;; Everything (the daemon, and the sync/update one-shots) runs inside a
;; `guix shell --container --emulate-fhs' sandbox rather than directly on the
;; host.  ComfyUI's dependency tree (PyTorch, Triton, ComfyUI-Manager, ...)
;; ships prebuilt binaries and hardcodes FHS assumptions — /sbin/ldconfig,
;; /lib64/ld-linux-x86-64.so.2, a `cc' just sitting on PATH — none of which
;; exist on Guix.  Emulating FHS inside a throwaway container fixes the whole
;; class of problem at once instead of patching each symptom (missing
;; ldconfig, missing compiler, prebuilt ELF binaries with the wrong
;; interpreter, ...) as it's discovered.  See comfyui-container-runner-file.

(define-module (peteches services comfyui)
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
  #:export (comfyui-configuration comfyui-configuration? comfyui-service-type
            comfyui-custom-node comfyui-custom-node?))

;;; ── Utilities ────────────────────────────────────────────────────────────

(define (maybe-option flag value)
  (if value
      (list flag
            (if (number? value)
                (number->string value) value))
      '()))

(define (maybe-flag flag enabled?)
  (if enabled?
      (list flag)
      '()))

(define (join-paths paths)
  (if (null? paths) ""
      (let loop
        ((rest (cdr paths))
         (acc (car paths)))
        (if (null? rest) acc
            (loop (cdr rest)
                  (string-append acc ":"
                                 (car rest)))))))

(define (unique-configs-by proc configs)
  (fold-right (lambda (cfg acc)
                (if (any (lambda (existing)
                           (equal? (proc cfg)
                                   (proc existing))) acc) acc
                    (cons cfg acc)))
              '() configs))

;;; ── Custom node record type ──────────────────────────────────────────────

;; git-ref names a branch or tag to track, not a commit — deliberately not
;; pinned, so that `herd start <service-name>-update' can fast-forward it
;; the same way it does the ComfyUI checkout itself (see
;; comfyui-custom-nodes-update-gexp).  #f tracks the repo's default branch.
(define-record-type* <comfyui-custom-node> comfyui-custom-node
                     make-comfyui-custom-node
  comfyui-custom-node?
  (name comfyui-custom-node-name)
  (git-repo-url comfyui-custom-node-git-repo-url)
  (git-ref comfyui-custom-node-git-ref
           (default #f))
  ;; Whether to `uv pip install -r requirements.txt' from the node's own
  ;; directory, when present, after cloning/pulling it.
  (install-requirements? comfyui-custom-node-install-requirements?
                         (default #t))
  ;; Additional requirements-*.txt filenames (relative to the node's own
  ;; directory) to `uv pip install -r' after requirements.txt, when
  ;; present. Some node packs split optional/heavy deps (e.g. a GGUF/
  ;; llama.cpp backend) into a separate file precisely so a plain install
  ;; doesn't pull them in — list it here to opt in. Ignored when
  ;; install-requirements? is #f.
  (extra-requirements-files comfyui-custom-node-extra-requirements-files
                            (default '())))

;;; ── Record type ──────────────────────────────────────────────────────────

(define-record-type* <comfyui-configuration> comfyui-configuration
                     make-comfyui-configuration
  comfyui-configuration?
  ;; Shepherd identity.  Multiple instances can coexist with distinct names
  ;; (e.g. a CUDA instance and a CPU-only fallback).
  (service-name comfyui-configuration-service-name
                (default "comfyui"))
  (documentation comfyui-configuration-documentation
                 (default "ComfyUI AI generation server."))
  (auto-start? comfyui-configuration-auto-start?
               (default #t))

  ;; System account.  Multiple instances can share the same account; the service
  ;; type de-duplicates account declarations across configured instances.
  (create-account? comfyui-configuration-create-account?
                   (default #t))
  (user comfyui-configuration-user
        (default "comfyui"))
  (group comfyui-configuration-group
         (default "comfyui"))
  (supplementary-groups comfyui-configuration-supplementary-groups
                        (default '("video")))

  ;; uv runtime.  Keep uv and Python in the Guix system profile; keep the
  ;; Python dependency graph in pyproject.toml + uv.lock under uv-project-dir.
  ;; uv-project-dir must contain the ComfyUI source tree.
  (uv-command comfyui-configuration-uv-command
              (default "/run/current-system/profile/bin/uv"))
  (uv-project-dir comfyui-configuration-uv-project-dir
                  (default "/srv/comfyui"))
  (uv-run-args comfyui-configuration-uv-run-args
               (default '("--frozen" "--no-sync")))
  (uv-sync-args comfyui-configuration-uv-sync-args
                (default '("--frozen")))
  ;; When set, passed as UV_PYTHON so uv uses a specific interpreter path.
  ;; Leave #f to let uv discover python3 via PATH (python-package installs it).
  (uv-python comfyui-configuration-uv-python
             (default #f))
  ;; Git source.  Cloned into uv-project-dir by the sync service when
  ;; pyproject.toml is absent, making the service self-bootstrapping.
  (git-repo-url comfyui-configuration-git-repo-url
                (default "https://github.com/Comfy-Org/ComfyUI"))
  (git-command comfyui-configuration-git-command
               (default (file-append git "/bin/git")))
  ;; When set, uv will use this path as the virtualenv instead of <project>/.venv.
  (uv-project-environment comfyui-configuration-uv-project-environment
                          (default #f))
  ;; Guix package providing the Python interpreter uv will use.  Its store
  ;; path is passed as UV_PYTHON so uv finds it without needing it in PATH,
  ;; and it's declared as a package in the FHS container (see
  ;; comfyui-container-runner-file) so the venv's own bin/python3 symlink —
  ;; which points straight at this package's store output — resolves there.
  (python-package comfyui-configuration-python-package
                  (default (@ (gnu packages python) python)))
  ;; Guix package providing uv.  Installed into the system profile so the
  ;; default uv-command path (/run/current-system/profile/bin/uv) resolves.
  (uv-package comfyui-configuration-uv-package
              (default (@ (gnu packages rust-apps) uv)))
  ;; Guix package providing a standalone C compiler.  Declared as a package
  ;; in the FHS container so Triton (used by Triton-backed nodes/kernels,
  ;; e.g. MiniMaxH3) can find a working `cc' on PATH the same way it would
  ;; on a normal FHS distro, instead of hand-wiring CC/CPATH/PATH ourselves.
  (c-compiler-package comfyui-configuration-c-compiler-package
                      (default (@ (gnu packages commencement) gcc-toolchain)))
  ;; Extra Guix packages declared inside the FHS container alongside the
  ;; baseline (coreutils, bash, git, c-compiler-package,
  ;; linux-libre-headers, python-package, uv-package).  Add whatever a
  ;; custom node's own compiled extensions need to find on PATH.
  ;; MUST come from the base Guix distribution ((gnu packages ...)) — these
  ;; are resolved by name at container-runtime against whatever plain guix
  ;; binary this system was built with, which knows nothing about
  ;; externally-pulled channels (guix-science-nonfree, nonguix, ...); such a
  ;; package resolves fine at reconfigure time (this module's own
  ;; #:use-module sees it) but fails at runtime with "unknown package". For
  ;; an external-channel package only needed via a known absolute path
  ;; rather than on $PATH, reference it directly in
  ;; extra-environment-variables instead (see install-sage-attention?'s
  ;; CUDA_HOME example) — --expose=/gnu/store already makes any store path
  ;; reachable inside the container without guix shell resolving it by name.
  (container-extra-packages comfyui-configuration-container-extra-packages
                            (default (list)))
  ;; Extra host paths writably bind-mounted into the container, beyond the
  ;; baseline (uv-project-dir, state-dir, cache-dir, uv-project-environment
  ;; when set).  Needed for anything referenced by extra-model-paths-config,
  ;; e.g. an external model drive.
  (container-extra-shares comfyui-configuration-container-extra-shares
                          (default (list)))
  ;; Same as container-extra-shares but read-only.
  (container-extra-exposes comfyui-configuration-container-extra-exposes
                           (default (list)))
  ;; Additional packages installed into the system profile for this service.
  (runtime-packages comfyui-configuration-runtime-packages
                    (default (list)))

  ;; ComfyUI network binding.
  (listen comfyui-configuration-listen
          (default "127.0.0.1"))
  (port comfyui-configuration-port
        (default 8188))

  ;; ComfyUI path overrides.  When #f each defaults to a subdirectory of
  ;; state-dir so all runtime data stays under one managed tree.
  (output-directory comfyui-configuration-output-directory
                    (default #f))
  (input-directory comfyui-configuration-input-directory
                   (default #f))
  (temp-directory comfyui-configuration-temp-directory
                  (default #f))
  (user-directory comfyui-configuration-user-directory
                  (default #f))
  ;; Path to an extra_model_paths.yaml file for additional model locations.
  (extra-model-paths-config comfyui-configuration-extra-model-paths-config
                            (default #f))

  ;; GPU / VRAM settings.
  ;; vram-mode is a symbol: 'highvram 'normalvram 'lowvram 'novram, or #f.
  (vram-mode comfyui-configuration-vram-mode
             (default #f))
  (cpu? comfyui-configuration-cpu?
        (default #f))
  ;; GB of VRAM to reserve for the OS/other processes.
  (reserve-vram comfyui-configuration-reserve-vram
                (default #f))

  ;; Latent preview method: "auto", "latent2rgb", "taesd", "none", or #f.
  (preview-method comfyui-configuration-preview-method
                  (default #f))

  ;; Suppress browser auto-launch — always sensible for a headless server.
  (disable-auto-launch? comfyui-configuration-disable-auto-launch?
                        (default #t))

  ;; Install comfyui-manager and pass --enable-manager to the daemon.
  (enable-manager? comfyui-configuration-enable-manager?
                   (default #t))

  ;; Install SageAttention (github.com/thu-ml/SageAttention) into the venv.
  ;; Deliberately decoupled from enable-sage-attention? below: a per-workflow
  ;; node (e.g. KJNodes' "Patch Sage Attention") calls the same `sageattention'
  ;; library directly and needs it present regardless of whether ComfyUI's
  ;; own global CLI flag is used — and several popular video-model wrappers
  ;; (WanVideoWrapper, HunyuanVideoWrapper) implement their own attention
  ;; path that only responds to their node, making the CLI flag a no-op for
  ;; them anyway. SageAttention ships no universal prebuilt wheel — installing
  ;; it compiles a CUDA extension via nvcc at sync time, so the FHS container
  ;; needs a real CUDA toolkit's nvcc reachable via CUDA_HOME (torch's own
  ;; nvcc discovery checks CUDA_HOME before ever falling back to $PATH). Set
  ;; CUDA_HOME in extra-environment-variables to a direct package reference
  ;; (e.g. #~(string-append "CUDA_HOME=" #$(@ (guix-science-nonfree packages
  ;; cuda) cuda)), the same package colibri-engine-cuda builds against — see
  ;; peteches/packages/colibri.scm) — do NOT add that package to
  ;; container-extra-packages, since guix shell can't resolve an
  ;; external-channel package by name inside this FHS container (see
  ;; container-extra-packages' own field comment) and --expose=/gnu/store
  ;; already makes the referenced store path reachable regardless. The
  ;; baseline container only ships a plain C compiler (c-compiler-package),
  ;; not nvcc. Also verify that toolkit's version against SageAttention's
  ;; per-architecture CUDA floor (>=12.4 for FP8 on Ada, >=12.3 on Hopper,
  ;; >=12.0 on Ampere, >=12.8 for Blackwell/SageAttention2++).
  (install-sage-attention? comfyui-configuration-install-sage-attention?
                           (default #f))
  ;; Git tag pin for the install above. PyPI's `sageattention' package tops
  ;; out at 1.0.6 — the 2.x line upstream's own README documents installing
  ;; is git/source-only, never published to PyPI, so this installs directly
  ;; from the tag `v<sage-attention-version>' on thu-ml/SageAttention rather
  ;; than a PyPI version pin.
  (sage-attention-version comfyui-configuration-sage-attention-version
                          (default "2.2.0"))
  ;; Pass --use-sage-attention to the daemon, patching it in globally for
  ;; every workflow instead of leaving attention-backend selection to a node.
  ;; Requires install-sage-attention? #t (the flag alone does nothing without
  ;; the package). Part of ComfyUI's mutually exclusive attention-backend
  ;; argument group (--use-split-cross-attention, --use-quad-cross-attention,
  ;; --use-pytorch-cross-attention, --use-flash-attention,
  ;; --use-ck-attention) — don't also pass one of those via extra-args.
  (enable-sage-attention? comfyui-configuration-enable-sage-attention?
                          (default #f))

  ;; Custom ComfyUI node packs to clone into <uv-project-dir>/custom_nodes,
  ;; a list of comfyui-custom-node records.  Cloned (if missing) and had
  ;; their own requirements.txt installed by both the sync and update
  ;; shepherd services; the update service additionally fetches and
  ;; fast-forwards (--ff-only) each one it already has a checkout of, same
  ;; as it does for ComfyUI itself, so `herd start <service-name>-update'
  ;; pulls custom nodes' latest commits too — see comfyui-custom-node's
  ;; git-ref field comment for why these track a branch rather than pin.
  (custom-nodes comfyui-configuration-custom-nodes
                (default (list)))

  ;; Verbatim extra CLI arguments appended to the python main.py invocation.
  (extra-args comfyui-configuration-extra-args
              (default (list)))

  ;; Paths.  Defaults are derived from service-name when #f.
  (state-dir comfyui-configuration-state-dir
             (default #f))
  (cache-dir comfyui-configuration-cache-dir
             (default #f))
  (log-dir comfyui-configuration-log-dir
           (default #f))
  (log-file comfyui-configuration-log-file
            (default #f))

  ;; Environment.  Defaults target CUDA wheels on Guix/Nonguix.
  (cuda-visible-devices comfyui-configuration-cuda-visible-devices
                        (default "0"))
  (ld-library-paths comfyui-configuration-ld-library-paths
                    (default '("/run/current-system/profile/lib"
                               "/run/current-system/profile/lib64")))
  (extra-environment-variables
   comfyui-configuration-extra-environment-variables
   (default (list)))

  ;; TLS.  When both ssl-certfile and ssl-keyfile are set, ComfyUI serves HTTPS.
  (ssl-certfile comfyui-configuration-ssl-certfile
                (default #f))
  (ssl-keyfile comfyui-configuration-ssl-keyfile
               (default #f))

  ;; Only open the firewall when intentionally serving beyond localhost.
  (open-firewall? comfyui-configuration-open-firewall?
                  (default #f)))

;;; ── Resolved defaults ────────────────────────────────────────────────────

(define (comfyui-resolved-state-dir config)
  (or (comfyui-configuration-state-dir config)
      (string-append "/var/lib/comfyui/"
                     (comfyui-configuration-service-name config))))

(define (comfyui-resolved-cache-dir config)
  (or (comfyui-configuration-cache-dir config)
      (string-append "/var/cache/comfyui/"
                     (comfyui-configuration-service-name config))))

(define (comfyui-resolved-log-dir config)
  (or (comfyui-configuration-log-dir config)
      (string-append "/var/log/comfyui/"
                     (comfyui-configuration-service-name config))))

(define (comfyui-resolved-log-file config)
  (or (comfyui-configuration-log-file config)
      (string-append (comfyui-resolved-log-dir config) "/server.log")))

(define (comfyui-resolved-sync-log-file config)
  (string-append (comfyui-resolved-log-dir config) "/sync.log"))

(define (comfyui-resolved-update-log-file config)
  (string-append (comfyui-resolved-log-dir config) "/update.log"))

(define (comfyui-resolved-output-directory config)
  (or (comfyui-configuration-output-directory config)
      (string-append (comfyui-resolved-state-dir config) "/output")))

(define (comfyui-resolved-input-directory config)
  (or (comfyui-configuration-input-directory config)
      (string-append (comfyui-resolved-state-dir config) "/input")))

(define (comfyui-resolved-temp-directory config)
  (or (comfyui-configuration-temp-directory config)
      (string-append (comfyui-resolved-state-dir config) "/temp")))

(define (comfyui-resolved-user-directory config)
  (or (comfyui-configuration-user-directory config)
      (string-append (comfyui-resolved-state-dir config) "/user")))

;;; ── ComfyUI command / environment rendering ──────────────────────────────

(define (comfyui-main-args config)
  (append (list "python"
                "main.py"
                "--listen"
                (comfyui-configuration-listen config)
                "--port"
                (number->string (comfyui-configuration-port config)))
          (list "--output-directory"
                (comfyui-resolved-output-directory config))
          (list "--input-directory"
                (comfyui-resolved-input-directory config))
          (list "--temp-directory"
                (comfyui-resolved-temp-directory config))
          (list "--user-directory"
                (comfyui-resolved-user-directory config))
          ;; ComfyUI's --database-url default is hardcoded relative to
          ;; main.py's own location (<uv-project-dir>/user/comfyui.db),
          ;; independent of --user-directory — so left unset, it tries to
          ;; open a database under uv-project-dir, which has no "user"
          ;; subdirectory (that name is only created, and chowned, under
          ;; state-dir by comfyui-instance-activation). Point it at the
          ;; directory that actually exists instead.
          (list "--database-url"
                (string-append "sqlite:///"
                               (comfyui-resolved-user-directory config)
                               "/comfyui.db"))
          (maybe-option "--extra-model-paths-config"
                        (comfyui-configuration-extra-model-paths-config config))
          ;; VRAM mode: exactly one flag from the set, or none.
          (let ((mode (comfyui-configuration-vram-mode config)))
            (cond
              ((eq? mode
                    'highvram)
               '("--highvram"))
              ((eq? mode
                    'normalvram)
               '("--normalvram"))
              ((eq? mode
                    'lowvram)
               '("--lowvram"))
              ((eq? mode
                    'novram)
               '("--novram"))
              (else '())))
          (maybe-flag "--cpu"
                      (comfyui-configuration-cpu? config))
          (maybe-option "--reserve-vram"
                        (comfyui-configuration-reserve-vram config))
          (maybe-option "--preview-method"
                        (comfyui-configuration-preview-method config))
          (maybe-flag "--disable-auto-launch"
                      (comfyui-configuration-disable-auto-launch? config))
          (maybe-flag "--enable-manager"
                      (comfyui-configuration-enable-manager? config))
          (maybe-flag "--use-sage-attention"
                      (comfyui-configuration-enable-sage-attention? config))
          (maybe-option "--tls-certfile"
                        (comfyui-configuration-ssl-certfile config))
          (maybe-option "--tls-keyfile"
                        (comfyui-configuration-ssl-keyfile config))
          (comfyui-configuration-extra-args config)))

(define (comfyui-environment config)
  (let* ((state-dir (comfyui-resolved-state-dir config))
         (cache-dir (comfyui-resolved-cache-dir config))
         (hf-home (string-append cache-dir "/huggingface"))
         (uv-cache-dir (string-append cache-dir "/uv"))
         (xdg-cache-home (string-append cache-dir "/xdg"))
         (cuda-devices (comfyui-configuration-cuda-visible-devices config))
         (ld-paths (comfyui-configuration-ld-library-paths config))
         (uv-env (comfyui-configuration-uv-project-environment config))
         (uv-python (comfyui-configuration-uv-python config))
         (git-command (comfyui-configuration-git-command config)))
    (append (list (string-append "HOME=" state-dir)
             "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
             "UV_PYTHON_DOWNLOADS=never"
             (string-append "HF_HOME=" hf-home)
             "HF_HUB_DISABLE_TELEMETRY=1"
             (string-append "UV_CACHE_DIR=" uv-cache-dir)
             (string-append "XDG_CACHE_HOME=" xdg-cache-home))
            ;; ComfyUI-Manager's GitPython dependency searches $PATH for
            ;; `git' (or respects this variable).  Redundant now that git
            ;; is declared as a container package and reachable on PATH
            ;; there too, but harmless and cheap insurance.
            (list #~(string-append "GIT_PYTHON_GIT_EXECUTABLE="
                                   #$git-command))
            (if uv-python
                (list (string-append "UV_PYTHON=" uv-python))
                '())
            (if cuda-devices
                (list (string-append "CUDA_VISIBLE_DEVICES=" cuda-devices)
                      "CUDA_DEVICE_ORDER=PCI_BUS_ID"
                      ;; Belt-and-suspenders: the FHS container's own
                      ;; ldconfig genuinely works (glibc-for-fhs ships a
                      ;; real one), but it only knows about the container's
                      ;; own lib dirs, not the bind-mounted
                      ;; /run/current-system/profile/lib holding
                      ;; libcuda.so.1.  This skips Triton's ldconfig call
                      ;; entirely rather than relying on it finding the
                      ;; right cache entry.
                      "TRITON_LIBCUDA_PATH=/run/current-system/profile/lib")
                '())
            (if (null? ld-paths)
                '()
                (list (string-append "LD_LIBRARY_PATH="
                                     (join-paths ld-paths))))
            (if uv-env
                (list (string-append "UV_PROJECT_ENVIRONMENT=" uv-env))
                '())
            (comfyui-configuration-extra-environment-variables config))))

;;; ── FHS container wrapper ────────────────────────────────────────────────

;; Builds a program-file that, when run, execs into
;; `guix shell --container --emulate-fhs' with INNER-COMMAND as the
;; containerized command.  Used to launch the daemon and the sync/update
;; one-shots alike (see the module comment at the top of this file for why).
;;
;; The nvidia device list is discovered at the *runtime* of the generated
;; script, not at reconfigure time, since it has to reflect whatever GPU(s)
;; are actually present on the machine that ends up running it, not the
;; machine that happened to build/reconfigure it (nug offloads builds).
;;
;; `--container' only bind-mounts store items that are part of the
;; requested package closure — *not* the whole store — so anything
;; referenced only via a raw store path (uv-command's default, the venv's
;; own bin/python3 symlink, an already's-been-through-uv package) would
;; otherwise be invisible inside the container.  `--expose=/gnu/store'
;; sidesteps having to enumerate every such path by hand.
;;
;; Packages are passed to `guix shell' as bare names, resolved by the
;; invoked guix-pkg binary against its own bundled (gnu packages ...) tree
;; — NOT against whatever channels this system happens to have pulled, and
;; NOT via --profile=, which `guix shell --emulate-fhs' can never accept
;; (it unconditionally injects an ad-hoc glibc-for-fhs package internally,
;; and guix rejects any combination of --profile with other package
;; options, no matter how few). Concretely: only packages from the base
;; Guix distribution belong in container-extra-packages / c-compiler-package
;; / python-package / uv-package — anything from an external channel
;; (guix-science-nonfree, nonguix, ...) fails at runtime with "unknown
;; package" even though it resolved fine at reconfigure time. For a package
;; that's only needed via a known absolute path rather than on $PATH (e.g.
;; CUDA_HOME for nvcc — see install-sage-attention?), reference it directly
;; in extra-environment-variables instead: --expose=/gnu/store already makes
;; any store path reachable inside the container without guix shell ever
;; needing to resolve it by name.
(define (comfyui-container-runner-file config suffix env inner-command)
  (let* ((service-name (comfyui-configuration-service-name config))
         (uv-project-dir (comfyui-configuration-uv-project-dir config))
         (state-dir (comfyui-resolved-state-dir config))
         (cache-dir (comfyui-resolved-cache-dir config))
         (uv-env (comfyui-configuration-uv-project-environment config))
         (python-pkg (comfyui-configuration-python-package config))
         (uv-pkg (comfyui-configuration-uv-package config))
         (c-compiler-pkg (comfyui-configuration-c-compiler-package config))
         (extra-pkgs (comfyui-configuration-container-extra-packages config))
         (extra-shares (comfyui-configuration-container-extra-shares config))
         (extra-exposes (comfyui-configuration-container-extra-exposes config))
         (package-names (map package-name
                             (append (list (@ (gnu packages base) coreutils)
                                           (@ (gnu packages bash) bash)
                                           (@ (gnu packages version-control) git)
                                           c-compiler-pkg
                                           (@ (gnu packages linux) linux-libre-headers)
                                           ;; Without this, SSL_CERT_FILE
                                           ;; (see comfyui-environment) points at
                                           ;; an /etc/ssl/certs/ca-certificates.crt
                                           ;; that --emulate-fhs never populates,
                                           ;; and git fetches inside the
                                           ;; container fail TLS verification.
                                           (@ (gnu packages nss) nss-certs)
                                           python-pkg
                                           uv-pkg)
                                     extra-pkgs)))
         (static-args (append
                       (list "--container" "--emulate-fhs" "--network"
                             "--expose=/gnu/store"
                             (string-append "--share=" uv-project-dir)
                             (string-append "--share=" state-dir)
                             (string-append "--share=" cache-dir)
                             "--expose=/run/current-system/profile")
                       (if uv-env
                           (list (string-append "--share=" uv-env))
                           '())
                       (map (lambda (s) (string-append "--share=" s)) extra-shares)
                       (map (lambda (s) (string-append "--expose=" s)) extra-exposes)
                       package-names))
         (guix-pkg (@ (gnu packages package-management) guix)))
    (program-file
     (string-append service-name suffix "-container-runner")
     #~(begin
         (use-modules (ice-9 ftw) (srfi srfi-1) (srfi srfi-13))
         ;; `guix shell -E VAR=VALUE' (the "set directly" form documented as
         ;; "if REGEXP contains `=', set VAR to VALUE instead") does not
         ;; reliably inject VALUE when guix's own process environment is
         ;; sparse — exactly what shepherd's #:environment-variables
         ;; produces for this wrapper.  Verified live on nug: under a
         ;; stripped `env -i' environment, `-E LD_LIBRARY_PATH=/foo/bar'
         ;; forwarded nothing (torch.cuda.is_available() => False), while
         ;; the plain-regex form `-E '^LD_LIBRARY_PATH$'` correctly forwards
         ;; whatever value the variable already holds in this wrapper's own
         ;; environment (torch.cuda.is_available() => True) — and it
         ;; already does, since forkexec-constructor's
         ;; #:environment-variables sets it here before execlp inherits it
         ;; into guix.  So: preserve by name only, never by "VAR=VALUE".
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

;;; ── Service extension helpers ────────────────────────────────────────────

(define (comfyui-accounts configs)
  (let* ((account-configs (filter comfyui-configuration-create-account?
                                  configs))
         (group-configs (unique-configs-by comfyui-configuration-group
                                           account-configs))
         (user-configs (unique-configs-by comfyui-configuration-user
                                          account-configs)))
    (append (map (lambda (config)
                   (user-group
                     (name (comfyui-configuration-group config))
                     (system? #t))) group-configs)
            (map (lambda (config)
                   (user-account
                     (name (comfyui-configuration-user config))
                     (group (comfyui-configuration-group config))
                     (system? #t)
                     (comment "ComfyUI AI generation server daemon")
                     (home-directory (comfyui-resolved-state-dir config))
                     (supplementary-groups (comfyui-configuration-supplementary-groups
                                            config))
                     (shell (file-append shadow "/sbin/nologin"))))
                 user-configs))))

(define (comfyui-instance-activation config)
  (let* ((user (comfyui-configuration-user config))
         (state-dir (comfyui-resolved-state-dir config))
         (cache-dir (comfyui-resolved-cache-dir config))
         (log-dir (comfyui-resolved-log-dir config))
         (uv-project-dir (comfyui-configuration-uv-project-dir config))
         (uv-env (comfyui-configuration-uv-project-environment config))
         (dirs (append (list state-dir
                             (string-append state-dir "/output")
                             (string-append state-dir "/input")
                             (string-append state-dir "/temp")
                             (string-append state-dir "/user")
                             cache-dir
                             (string-append cache-dir "/huggingface")
                             (string-append cache-dir "/uv")
                             (string-append cache-dir "/xdg")
                             log-dir
                             uv-project-dir)
                       (if uv-env
                           (list uv-env)
                           '()))))
    #~(let* ((pw (getpwnam #$user))
             (uid (passwd:uid pw))
             (gid (passwd:gid pw)))
        (for-each (lambda (dir)
                    (mkdir-p dir)
                    (chown dir uid gid))
                  (list #$@dirs)))))

(define (comfyui-activation configs)
  #~(begin
      (use-modules (guix build utils))
      #$@(map comfyui-instance-activation configs)))

(define (comfyui-custom-node-directory uv-project-dir node)
  (string-append uv-project-dir "/custom_nodes/"
                 (comfyui-custom-node-name node)))

;; Clone NODE into uv-project-dir/custom_nodes if not already present.
;; Idempotent — safe to call on every sync/update run.
(define (comfyui-custom-node-clone-gexp uv-project-dir git-command node)
  (let* ((dir (comfyui-custom-node-directory uv-project-dir node))
         (ref (comfyui-custom-node-git-ref node))
         (clone-args (append (if ref (list "--branch" ref) '())
                             (list (comfyui-custom-node-git-repo-url node) dir))))
    #~(unless (file-exists? (string-append #$dir "/.git"))
        (let ((ret (system* #$git-command "clone" #$@clone-args)))
          (unless (zero? ret)
            (error "custom node clone failed with exit code" ret #$dir))))))

;; Fetch and fast-forward an already-cloned NODE.  Silently does nothing if
;; NODE has no checkout yet — comfyui-custom-node-clone-gexp (always run
;; right after this, in comfyui-sync-steps-gexp) bootstraps that case
;; instead, mirroring how the main ComfyUI checkout is handled.
(define (comfyui-custom-node-pull-gexp uv-project-dir git-command node)
  (let ((dir (comfyui-custom-node-directory uv-project-dir node)))
    #~(when (file-exists? (string-append #$dir "/.git"))
        (let ((ret (system* #$git-command "-C" #$dir "fetch" "--tags")))
          (unless (zero? ret)
            (error "custom node fetch failed with exit code" ret #$dir)))
        (let ((ret (system* #$git-command "-C" #$dir "pull" "--ff-only")))
          (unless (zero? ret)
            (error "custom node pull failed with exit code" ret #$dir))))))

(define (comfyui-custom-node-requirements-gexp uv-project-dir uv-command venv-dir node)
  (let ((dir (comfyui-custom-node-directory uv-project-dir node))
        (extra-files (comfyui-custom-node-extra-requirements-files node)))
    #~(begin
        (let ((req (string-append #$dir "/requirements.txt")))
          (when (file-exists? req)
            (let ((ret (system* #$uv-command "pip" "install"
                                "--python" #$venv-dir "-r" req)))
              (unless (zero? ret)
                (error "custom node requirements install failed with exit code"
                       ret #$dir)))))
        #$@(map (lambda (filename)
                  #~(let ((req (string-append #$dir "/" #$filename)))
                      (when (file-exists? req)
                        (let ((ret (system* #$uv-command "pip" "install"
                                            "--python" #$venv-dir "-r" req)))
                          (unless (zero? ret)
                            (error "custom node extra requirements install failed with exit code"
                                   ret #$dir))))))
                extra-files))))

;; Fetch+fast-forward every custom node config already has a checkout of.
;; Only meaningful from the update shepherd service — the sync service never
;; calls this, since on a first run nothing is cloned yet to pull.
(define (comfyui-custom-nodes-update-gexp config)
  (let ((uv-project-dir (comfyui-configuration-uv-project-dir config))
        (git-command (comfyui-configuration-git-command config))
        (custom-nodes (comfyui-configuration-custom-nodes config)))
    #~(begin
        #$@(map (lambda (node)
                  (comfyui-custom-node-pull-gexp uv-project-dir git-command node))
                custom-nodes))))

;; Shared by the bootstrap sync runner and the manual update runner below:
;; resolve the uv environment against whatever commit is currently checked
;; out at uv-project-dir.
(define (comfyui-sync-steps-gexp config venv-dir)
  (let* ((uv-command (comfyui-configuration-uv-command config))
         (uv-sync-args (comfyui-configuration-uv-sync-args config))
         (uv-project-dir (comfyui-configuration-uv-project-dir config))
         (git-command (comfyui-configuration-git-command config))
         (enable-manager? (comfyui-configuration-enable-manager? config))
         (install-sage-attention? (comfyui-configuration-install-sage-attention? config))
         (sage-attention-version (comfyui-configuration-sage-attention-version config))
         (custom-nodes (comfyui-configuration-custom-nodes config)))
  #~(begin
      ;; Drop --frozen when no lockfile exists yet
      ;; (e.g. after a fresh clone of a repo that
      ;; doesn't ship uv.lock).
      (let* ((have-lock? (file-exists? (string-append #$uv-project-dir
                                        "/uv.lock")))
             (sync-args (if have-lock?
                            (list #$@uv-sync-args)
                            (filter (lambda (a)
                                      (not (equal?
                                            a
                                            "--frozen")))
                                    (list #$@uv-sync-args))))
             (ret (apply system*
                         #$uv-command "sync"
                         sync-args)))
        (unless (zero? ret)
          (error "uv sync failed with exit code"
                 ret)))
      ;; Install from requirements.txt when present
      ;; (ComfyUI declares deps there, not in pyproject.toml).
      ;; Use --python <venv> so uv installs into the project
      ;; venv rather than the read-only store Python.
      (let ((req (string-append #$uv-project-dir
                  "/requirements.txt")))
        (when (file-exists? req)
          (let ((ret (system* #$uv-command
                              "pip"
                              "install"
                              "--python"
                              #$venv-dir
                              "-r"
                              req)))
            (unless (zero? ret)
              (error
               "uv pip install failed with exit code"
               ret)))))
      ;; Install comfyui-manager from ComfyUI's own
      ;; manager_requirements.txt when enabled.  ComfyUI
      ;; imports the Python module `comfyui_manager' by
      ;; name; if the module isn't in the venv, main.py
      ;; silently disables the --enable-manager flag.
      ;; Using ComfyUI's shipped requirements file pins
      ;; to the version main.py was built against.
      ;; Use #$@(if ...) rather than (when #$bool ...)
      ;; because Guix gexps don't splice plain booleans
      ;; reliably with #$ — the when block was silently dropped.
      #$@(if enable-manager?
             (list #~(let ((mgr-req (string-append #$uv-project-dir
                                     "/manager_requirements.txt")))
                       (if (file-exists? mgr-req)
                           (let ((ret (system* #$uv-command
                                       "pip"
                                       "install"
                                       "--python"
                                       #$venv-dir
                                       "-U"
                                       "-r"
                                       mgr-req)))
                             (unless (zero? ret)
                               (error
                                "comfyui-manager install failed with exit code"
                                ret)))
                           (error
                            "enable-manager? is #t but manager_requirements.txt not found at"
                            mgr-req))))
             '())
      ;; SageAttention has no universal prebuilt wheel and PyPI's release
      ;; never went past 1.0.6, so this installs straight from a git tag
      ;; (see sage-attention-version's field comment) and compiles a CUDA
      ;; extension via nvcc — see the install-sage-attention? field comment
      ;; for what the container needs for that to succeed.
      ;; --no-build-isolation matches upstream's own install instructions
      ;; (it needs the venv's already-installed torch visible at build time).
      #$@(if install-sage-attention?
             (list #~(let ((ret (system* #$uv-command
                                 "pip"
                                 "install"
                                 "--python"
                                 #$venv-dir
                                 "--no-build-isolation"
                                 #$(string-append
                                    "git+https://github.com/thu-ml/SageAttention.git@v"
                                    sage-attention-version))))
                       (unless (zero? ret)
                         (error
                          "sageattention install failed with exit code"
                          ret))))
             '())
      ;; Bootstrap any custom node not yet cloned, then install its
      ;; requirements.txt if it has one.  Fetching/pulling ones already
      ;; cloned is the update service's job (comfyui-custom-nodes-update-gexp),
      ;; not this shared step — matches how the main ComfyUI checkout itself
      ;; is only ever cloned here, never pulled.
      #$@(append-map
          (lambda (node)
            (append (list (comfyui-custom-node-clone-gexp uv-project-dir
                                                           git-command node))
                    (if (comfyui-custom-node-install-requirements? node)
                        (list (comfyui-custom-node-requirements-gexp
                               uv-project-dir uv-command venv-dir node))
                        '())))
          custom-nodes))))

(define (comfyui-sync-shepherd-service config)
  (let* ((service-name (comfyui-configuration-service-name config))
         (sync-name (string->symbol (string-append service-name "-sync")))
         (uv-project-dir (comfyui-configuration-uv-project-dir config))
         (git-command (comfyui-configuration-git-command config))
         (git-repo-url (comfyui-configuration-git-repo-url config))
         (user (comfyui-configuration-user config))
         (group (comfyui-configuration-group config))
         (log-file (comfyui-resolved-sync-log-file config))
         (python-pkg (comfyui-configuration-python-package config))
         (uv-env (comfyui-configuration-uv-project-environment config))
         (venv-dir (or uv-env
                       (string-append uv-project-dir "/.venv")))
         ;; The store interpreter, used to *create* the venv — as opposed
         ;; to the daemon's UV_PYTHON, which points at the venv itself
         ;; once it exists (see comfyui-daemon-shepherd-service).
         (python-env (list #~(string-append "UV_PYTHON="
                                            #$(file-append python-pkg
                                                           "/bin/python3"))))
         (env (append (comfyui-environment config) python-env))
         (runner (program-file (string-append service-name "-sync-runner")
                               #~(begin
                                   (unless (file-exists? (string-append #$uv-project-dir
                                                          "/pyproject.toml"))
                                     (let ((ret (system* #$git-command "clone"
                                                         #$git-repo-url ".")))
                                       (unless (zero? ret)
                                         (error
                                          "git clone failed with exit code"
                                          ret))))
                                   #$(comfyui-sync-steps-gexp config venv-dir))))
         (container-runner (comfyui-container-runner-file config "-sync" env
                                                           (list runner))))
    (shepherd-service (provision (list sync-name))
                      (documentation (string-append
                                      "Synchronise the uv environment for "
                                      service-name "."))
                      (requirement '(networking file-systems))
                      (one-shot? #t)
                      (auto-start? #f)
                      (start #~(make-forkexec-constructor (list #$container-runner)
                                                          #:directory #$uv-project-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env)))
                      (stop #~(lambda _
                                #t)))))

;; Manual-only version bump: pulls the latest commit onto the existing
;; checkout and re-resolves the uv environment against it.  `auto-start?' is
;; #f, one-shot services never restart on their own once they exit, and
;; nothing else in this file lists it as a `requirement' — so unlike the
;; daemon, the only way this ever runs is an explicit
;; `herd start <service-name>-update'.  It does not restart the daemon
;; itself; follow up with `herd restart <service-name>' once it succeeds.
(define (comfyui-update-shepherd-service config)
  (let* ((service-name (comfyui-configuration-service-name config))
         (update-name (string->symbol (string-append service-name "-update")))
         (uv-project-dir (comfyui-configuration-uv-project-dir config))
         (git-command (comfyui-configuration-git-command config))
         (user (comfyui-configuration-user config))
         (group (comfyui-configuration-group config))
         (log-file (comfyui-resolved-update-log-file config))
         (python-pkg (comfyui-configuration-python-package config))
         (uv-env (comfyui-configuration-uv-project-environment config))
         (venv-dir (or uv-env
                       (string-append uv-project-dir "/.venv")))
         (python-env (list #~(string-append "UV_PYTHON="
                                            #$(file-append python-pkg
                                                           "/bin/python3"))))
         (env (append (comfyui-environment config) python-env))
         (runner (program-file (string-append service-name "-update-runner")
                               #~(begin
                                   (unless (file-exists? (string-append #$uv-project-dir
                                                          "/.git"))
                                     (error
                                      "comfyui-update: no git checkout found at"
                                      #$uv-project-dir))
                                   (let ((ret (system* #$git-command "-C" #$uv-project-dir
                                                       "fetch" "--tags")))
                                     (unless (zero? ret)
                                       (error "git fetch failed with exit code"
                                              ret)))
                                   ;; --ff-only refuses to update a checkout with local
                                   ;; commits/edits or one left on a detached HEAD (e.g.
                                   ;; after a manual `git checkout <tag>') rather than
                                   ;; silently discarding state — surface that as a
                                   ;; failed run instead of guessing what to do about it.
                                   (let ((ret (system* #$git-command "-C" #$uv-project-dir
                                                       "pull" "--ff-only")))
                                     (unless (zero? ret)
                                       (error "git pull failed with exit code"
                                              ret)))
                                   #$(comfyui-custom-nodes-update-gexp config)
                                   #$(comfyui-sync-steps-gexp config venv-dir))))
         (container-runner (comfyui-container-runner-file config "-update" env
                                                           (list runner))))
    (shepherd-service (provision (list update-name))
                      (documentation (string-append
                                      "Manually pull the latest ComfyUI commit and "
                                      "re-sync the uv environment for "
                                      service-name ".  Never starts on its own."))
                      (requirement '(networking file-systems))
                      (one-shot? #t)
                      (auto-start? #f)
                      (start #~(make-forkexec-constructor (list #$container-runner)
                                                          #:directory #$uv-project-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env)))
                      (stop #~(lambda _
                                #t)))))

(define (comfyui-daemon-shepherd-service config)
  (let* ((service-name (comfyui-configuration-service-name config))
         (provision-name (string->symbol service-name))
         (uv-command (comfyui-configuration-uv-command config))
         (uv-run-args (comfyui-configuration-uv-run-args config))
         (uv-project-dir (comfyui-configuration-uv-project-dir config))
         (user (comfyui-configuration-user config))
         (group (comfyui-configuration-group config))
         (log-file (comfyui-resolved-log-file config))
         (uv-env (comfyui-configuration-uv-project-environment config))
         (venv-dir (or uv-env
                       (string-append uv-project-dir "/.venv")))
         ;; Point UV_PYTHON at the already-provisioned venv here, not at the
         ;; store interpreter used by the sync/update runners to *create*
         ;; that venv.  ComfyUI-Manager shells out to a bare `uv pip
         ;; install <pkg>` (no --python) to self-repair packages at
         ;; startup; that call inherits this env var, and the store path is
         ;; read-only, so it must resolve to the writable venv instead.
         (python-env (list #~(string-append "UV_PYTHON=" #$venv-dir
                                            "/bin/python3")))
         (env (append (comfyui-environment config) python-env))
         (main-args (comfyui-main-args config))
         (inner-command (append (list uv-command "run") uv-run-args main-args))
         (container-runner (comfyui-container-runner-file config "" env
                                                           inner-command)))
    (shepherd-service (provision (list provision-name))
                      (documentation (comfyui-configuration-documentation
                                      config))
                      (requirement '(networking file-systems))
                      (auto-start? (comfyui-configuration-auto-start? config))
                      (start #~(make-forkexec-constructor (list #$container-runner)
                                                          #:directory #$uv-project-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env)))
                      (stop #~(make-kill-destructor)))))

(define (comfyui-shepherd-services configs)
  (append (map comfyui-sync-shepherd-service configs)
          (map comfyui-update-shepherd-service configs)
          (map comfyui-daemon-shepherd-service configs)))

(define (comfyui-firewall-rules configs)
  (nftables-rules (input (append-map (lambda (config)
                                       (if (comfyui-configuration-open-firewall?
                                            config)
                                           (list (string-append "tcp dport "
                                                  (number->string (comfyui-configuration-port
                                                                   config))
                                                  " accept comment \""
                                                  (comfyui-configuration-service-name
                                                   config) "\""))
                                           '())) configs))))

(define (comfyui-profile-entries configs)
  (delete-duplicates (append-map (lambda (config)
                                   (filter (lambda (x)
                                             x)
                                           (append (list (comfyui-configuration-python-package
                                                          config)
                                                         (comfyui-configuration-uv-package
                                                          config))
                                                   (comfyui-configuration-runtime-packages
                                                    config)))) configs)))

;;; ── Service type ─────────────────────────────────────────────────────────

(define-public comfyui-service-type
  (service-type (name 'comfyui)
                (compose append)
                (extend append)
                (extensions (list (service-extension account-service-type
                                                     comfyui-accounts)
                                  (service-extension activation-service-type
                                                     comfyui-activation)
                                  (service-extension
                                   shepherd-root-service-type
                                   comfyui-shepherd-services)
                                  (service-extension firewall-service-type
                                                     comfyui-firewall-rules)
                                  (service-extension profile-service-type
                                                     comfyui-profile-entries)))
                (default-value '())
                (description
                 "Run one or more ComfyUI AI generation servers via uv-managed Python environments.")))
