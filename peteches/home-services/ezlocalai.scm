;; peteches/home-services/ezlocalai.scm  -*- scheme -*-
;; Simple ezlocalai Home service: clone repo, install reqs, run API (8091) + UI (8502).

(define-module (peteches home-services ezlocalai)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages version-control)  ; git
  #:use-module (gnu packages python)
  #:use-module (gnu packages audio)           ; portaudio
  #:use-module (gnu packages commencement)    ; gcc-toolchain
  #:use-module (gnu packages pkg-config)      ; pkg-config
  #:use-module (gnu packages linux)           ; alsa-lib (headers)
  #:export (ezlocalai-direct-home-configuration
            ezlocalai-direct-home-configuration?
            ezlocalai-direct-home-service-type))

;; ---------------- Configuration ----------------------------------------------

(define-record-type* <ezlocalai-direct-home-configuration>
  ezlocalai-direct-home-configuration make-ezlocalai-direct-home-configuration
  ezlocalai-direct-home-configuration?
  ;; state & secrets (under $HOME):
  (state-relpath       ez-state-rel    (default "/.local/share/ezlocalai"))
  (venv-relpath        ez-venv-rel     (default "/.local/share/ezlocalai/venv"))
  (secrets-relpath     ez-secrets-rel  (default "/.local/share/ezlocalai/secrets.env"))
  ;; ports
  (http-port           ez-http-port    (default 8091))
  (ui-port             ez-ui-port      (default 8502))
  ;; auto-update: re-run pip on start
  (auto-update?        ez-auto-update? (default #t))
  ;; extra requirements written to ~/.config/ezlocalai/requirements.txt
  ;; Provide ORT by default to fix Embedding import.
  (extra-requirements  ez-extra-reqs   (default
                                        '("optimum[onnxruntime]==1.18.1"
                                          "onnxruntime>=1.16,<2"
                                          "transformers==4.39.3"
                                          "diffusers==0.29.2"
                                          "llama-cpp-python==0.2.82"
                                          "faster-whisper==1.0.2"
                                          "requests==2.32.0")))
  ;; non-secret env pairs written to ~/.config/ezlocalai/ezlocalai.env
  (env-overrides       ez-env-over     (default '())))

;; ---------------- Files we write under XDG -----------------------------------

(define (ezlocalai-env-file cfg)
  (let* ((pairs (ez-env-over cfg))
         (->line (lambda (p)
                   (unless (and (pair? p) (string? (car p)) (string? (cdr p)))
                     (error "env-overrides must be list of (STRING . STRING) pairs" p))
                   (string-append (car p) "=" (cdr p)))))
    (plain-file "ezlocalai.env"
                (string-join (map ->line pairs) "
"))))

(define (extra-reqs-file cfg)
  (let* ((lines (ez-extra-reqs cfg))
         (content (string-join lines "
")))
    (plain-file "requirements.txt" content)))

;; ---------------- run.sh (XDG: ezlocalai/run.sh) -----------------------------

(define (ezlocalai-run-script cfg)
  (let* ((http    (number->string (ez-http-port cfg)))
         (ui      (number->string (ez-ui-port cfg)))
         (state   (ez-state-rel cfg))
         (venv    (ez-venv-rel cfg))
         (secrets (ez-secrets-rel cfg))
         (auto?   (if (ez-auto-update? cfg) "true" "false")))
    (computed-file "run-ezlocalai.sh"
      #~(begin
          (let* ((content
                  (string-append
                   "#!" #$(file-append bash "/bin/sh") "\n"
"set -eu\n"
"\n"
"# ---- config from Guix ----\n"
"HTTP_PORT=\"" #$http "\"\n"
"UI_PORT=\""   #$ui   "\"\n"
"STATE_DIR=\"$HOME" #$state "\"\n"
"VENV_DIR=\"$HOME"  #$venv  "\"\n"
"ENV_FILE=\"$HOME/.config/ezlocalai/ezlocalai.env\"\n"
"SECRETS_FILE=\"$HOME" #$secrets "\"\n"
"EXTRA_REQS=\"$HOME/.config/ezlocalai/requirements.txt\"\n"
"AUTO_UPDATE=\"" #$auto? "\"\n"
"\n"
"LOG=\"$STATE_DIR/ezlocalai.log\"\n"
"mkdir -p \"$STATE_DIR\" \"$HOME/.config/ezlocalai\"\n"
": >\"$LOG\"\n"
"exec >>\"$LOG\" 2>&1\n"
"\n"
"# ---- environment bootstrap (useful in Shepherd) ----\n"
"PATH=\"$HOME/.guix-home/profile/bin:$HOME/.guix-home/profile/sbin:$HOME/.guix-profile/bin:$HOME/.guix-profile/sbin:/run/current-system/profile/bin:/run/current-system/profile/sbin${PATH:+:$PATH}\"\n"
"export PATH\n"
"export CPATH=\"$HOME/.guix-home/profile/include${CPATH:+:$CPATH}\"\n"
"export LIBRARY_PATH=\"$HOME/.guix-home/profile/lib${LIBRARY_PATH:+:$LIBRARY_PATH}\"\n"
"export LD_LIBRARY_PATH=\"$HOME/.guix-home/profile/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"\n"
"\n"
"# ---- non-secret + secret env (if present) ----\n"
"[ -r \"$ENV_FILE\" ]     && { set -a; . \"$ENV_FILE\";     set +a; }\n"
"[ -r \"$SECRETS_FILE\" ] && { set -a; . \"$SECRETS_FILE\"; set +a; }\n"
"\n"
"# ---- venv ----\n"
"if [ ! -x \"$VENV_DIR/bin/python\" ]; then\n"
"  umask 022\n"
"  python3 -m venv \"$VENV_DIR\" 2>/dev/null || python -m venv \"$VENV_DIR\"\n"
"fi\n"
"PY=\"$VENV_DIR/bin/python\"\n"
"$PY -m pip install --upgrade pip wheel setuptools scikit-build-core\n"
"\n"
"# ---- source checkout ----\n"
"SRC_DIR=\"$STATE_DIR/src\"\n"
"if [ ! -d \"$SRC_DIR/.git\" ]; then\n"
"  echo \"[ezlocalai] cloning https://github.com/DevXT-LLC/ezlocalai -> $SRC_DIR\"\n"
"  rm -rf \"$SRC_DIR\"\n"
"  git clone https://github.com/DevXT-LLC/ezlocalai \"$SRC_DIR\"\n"
"else\n"
"  echo \"[ezlocalai] updating $SRC_DIR\"\n"
"  (cd \"$SRC_DIR\" && git pull --ff-only || true)\n"
"fi\n"
"\n"
"# ---- helper: install with system toolchain via guix shell, no build isolation ----\n"
"install_with_toolchain() {\n"
"  # Packages: cmake+ninja+gcc-toolchain for building; openblas+pkg-config for BLAS build flags\n"
"  guix shell cmake ninja gcc-toolchain openblas pkg-config -- \\\n"
"    env PIP_NO_BUILD_ISOLATION=1 \\\n"
"        CMAKE_ARGS=\"${CMAKE_ARGS:-\"-DLLAMA_BLAS=ON -DLLAMA_BLAS_VENDOR=OpenBLAS\"}\" \\\n"
"        \"$PY\" -m pip install --no-cache-dir \"$@\"\n"
"}\n"
"\n"
"# ---- install requirements ----\n"
"if [ \"$AUTO_UPDATE\" = \"true\" ] || [ ! -e \"$STATE_DIR/.installed\" ]; then\n"
"  if [ -f \"$SRC_DIR/requirements.txt\" ]; then\n"
"    echo \"[ezlocalai] installing repo requirements (with toolchain)…\"\n"
"    install_with_toolchain -r \"$SRC_DIR/requirements.txt\"\n"
"  fi\n"
"  if [ -s \"$EXTRA_REQS\" ]; then\n"
"    echo \"[ezlocalai] installing extra requirements (with toolchain)…\"\n"
"    install_with_toolchain -r \"$EXTRA_REQS\"\n"
"  fi\n"
"  date > \"$STATE_DIR/.installed\"\n"
"fi\n"
"\n"
"# ---- start API (background) ----\n"
"cleanup(){ [ -f \"$STATE_DIR/uvicorn.pid\" ] && kill \"$(cat \"$STATE_DIR/uvicorn.pid\")\" 2>/dev/null || true; }\n"
"trap cleanup EXIT INT TERM\n"
"(cd \"$SRC_DIR\" && exec \"$VENV_DIR/bin/uvicorn\" app:app --host 0.0.0.0 --port \"$HTTP_PORT\") &\n"
"echo $! > \"$STATE_DIR/uvicorn.pid\"\n"
"\n"
"# ---- start UI (foreground) ----\n"
"cd \"$SRC_DIR\"\n"
"exec \"$VENV_DIR/bin/streamlit\" run ui.py --server.port \"$UI_PORT\" --server.headless true\n"
))
                 (out #$output))
            (call-with-output-file out (lambda (p) (display content p)))
            (chmod out #o755))))))

;; ---------------- Shepherd service -------------------------------------------

(define (svc cfg)
  (shepherd-service
   (documentation "ezlocalai: Uvicorn API + Streamlit UI from a local venv.")
   (provision '(ezlocalai))
   (start
    #~(make-forkexec-constructor
       (list #$(file-append bash "/bin/sh")
             "-lc"
             (string-append "$HOME/.config/ezlocalai/run.sh"))
       #:log-file (string-append (getenv "HOME") #$(ez-state-rel cfg) "/shepherd.log")))
   (stop  #~(make-kill-destructor))
   (respawn? #t)))

;; ---------------- Service type ------------------------------------------------

(define ezlocalai-direct-home-service-type
  (service-type
   (name 'ezlocalai-direct-home)
   (extensions
    (list
     ;; Tools in Home profile
     (service-extension home-profile-service-type
                        (lambda (_cfg)
                          (list
                           python
                           git
                           gcc-toolchain
                           pkg-config
                           portaudio
                           alsa-lib)))
     ;; Write files under ~/.config
     (service-extension home-xdg-configuration-files-service-type
                        (lambda (cfg)
                          `(("ezlocalai/ezlocalai.env" ,(ezlocalai-env-file cfg))
                            ("ezlocalai/requirements.txt" ,(extra-reqs-file cfg))
                            ("ezlocalai/run.sh" ,(ezlocalai-run-script cfg)))))
     ;; Shepherd service
     (service-extension home-shepherd-service-type
                        (lambda (cfg) (list (svc cfg))))))
   (default-value (ezlocalai-direct-home-configuration))
   (description "Run ezlocalai directly via Python/uvicorn + Streamlit; simple and reproducible.")))
