;; peteches/services/comfyui.scm — Guix service type for ComfyUI via uv.
;;
;; This deliberately does not package ComfyUI as a Guix package.  Guix owns the
;; service boundary, accounts, paths, environment, ports and supervision; uv owns
;; the Python/ComfyUI wheel environment declared by pyproject.toml + uv.lock.
;;
;; The ComfyUI source tree (git clone of https://github.com/Comfy-Org/ComfyUI)
;; must live at uv-project-dir with a pyproject.toml and uv.lock already in
;; place before the service starts.

(define-module (peteches services comfyui)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages version-control)
  #:use-module (srfi srfi-1)
  #:use-module (peteches services firewall)
  #:export (comfyui-configuration comfyui-configuration? comfyui-service-type))

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
  ;; path is passed as UV_PYTHON so uv finds it without needing it in PATH.
  (python-package comfyui-configuration-python-package
                  (default (@ (gnu packages python) python)))
  ;; Guix package providing uv.  Installed into the system profile so the
  ;; default uv-command path (/run/current-system/profile/bin/uv) resolves.
  (uv-package comfyui-configuration-uv-package
              (default (@ (gnu packages rust-apps) uv)))
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
         (uv-python (comfyui-configuration-uv-python config)))
    (append (list (string-append "HOME=" state-dir)
             "PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin:/run/setuid-programs"
             "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
             "UV_PYTHON_DOWNLOADS=never"
             (string-append "HF_HOME=" hf-home)
             "HF_HUB_DISABLE_TELEMETRY=1"
             (string-append "UV_CACHE_DIR=" uv-cache-dir)
             (string-append "XDG_CACHE_HOME=" xdg-cache-home))
            (if uv-python
                (list (string-append "UV_PYTHON=" uv-python))
                '())
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
            (comfyui-configuration-extra-environment-variables config))))

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

;; Shared by the bootstrap sync runner and the manual update runner below:
;; resolve the uv environment against whatever commit is currently checked
;; out at uv-project-dir.
(define (comfyui-sync-steps-gexp uv-command uv-sync-args uv-project-dir
                                 venv-dir enable-manager?)
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
             '())))

(define (comfyui-sync-shepherd-service config)
  (let* ((service-name (comfyui-configuration-service-name config))
         (sync-name (string->symbol (string-append service-name "-sync")))
         (uv-command (comfyui-configuration-uv-command config))
         (uv-sync-args (comfyui-configuration-uv-sync-args config))
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
         (enable-manager? (comfyui-configuration-enable-manager? config))
         (python-env (if python-pkg
                         (list #~(string-append "UV_PYTHON="
                                                #$(file-append python-pkg
                                                               "/bin/python3")))
                         '()))
         (env (comfyui-environment config))
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
                                   #$(comfyui-sync-steps-gexp uv-command uv-sync-args
                                                              uv-project-dir venv-dir
                                                              enable-manager?)))))
    (shepherd-service (provision (list sync-name))
                      (documentation (string-append
                                      "Synchronise the uv environment for "
                                      service-name "."))
                      (requirement '(networking file-systems))
                      (one-shot? #t)
                      (auto-start? #f)
                      (start #~(make-forkexec-constructor (list #$runner)
                                                          #:directory #$uv-project-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env
                                                                #$@python-env)))
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
         (uv-command (comfyui-configuration-uv-command config))
         (uv-sync-args (comfyui-configuration-uv-sync-args config))
         (uv-project-dir (comfyui-configuration-uv-project-dir config))
         (git-command (comfyui-configuration-git-command config))
         (user (comfyui-configuration-user config))
         (group (comfyui-configuration-group config))
         (log-file (comfyui-resolved-update-log-file config))
         (python-pkg (comfyui-configuration-python-package config))
         (uv-env (comfyui-configuration-uv-project-environment config))
         (venv-dir (or uv-env
                       (string-append uv-project-dir "/.venv")))
         (enable-manager? (comfyui-configuration-enable-manager? config))
         (python-env (if python-pkg
                         (list #~(string-append "UV_PYTHON="
                                                #$(file-append python-pkg
                                                               "/bin/python3")))
                         '()))
         (env (comfyui-environment config))
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
                                   #$(comfyui-sync-steps-gexp uv-command uv-sync-args
                                                              uv-project-dir venv-dir
                                                              enable-manager?)))))
    (shepherd-service (provision (list update-name))
                      (documentation (string-append
                                      "Manually pull the latest ComfyUI commit and "
                                      "re-sync the uv environment for "
                                      service-name ".  Never starts on its own."))
                      (requirement '(networking file-systems))
                      (one-shot? #t)
                      (auto-start? #f)
                      (start #~(make-forkexec-constructor (list #$runner)
                                                          #:directory #$uv-project-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env
                                                                #$@python-env)))
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
         (python-pkg (comfyui-configuration-python-package config))
         (python-env (if python-pkg
                         (list #~(string-append "UV_PYTHON="
                                                #$(file-append python-pkg
                                                               "/bin/python3")))
                         '()))
         (env (comfyui-environment config))
         (main-args (comfyui-main-args config)))
    (shepherd-service (provision (list provision-name))
                      (documentation (comfyui-configuration-documentation
                                      config))
                      (requirement '(networking file-systems))
                      (auto-start? (comfyui-configuration-auto-start? config))
                      (start #~(make-forkexec-constructor (list #$uv-command
                                                                "run"
                                                                #$@uv-run-args
                                                                #$@main-args)
                                                          #:directory #$uv-project-dir
                                                          #:user #$user
                                                          #:group #$group
                                                          #:log-file #$log-file
                                                          #:environment-variables
                                                          (list #$@env
                                                                #$@python-env)))
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
