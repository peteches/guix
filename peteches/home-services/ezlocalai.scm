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
                   "#!" #$(file-append bash "/bin/sh") "
"
"set -eu

"
"# ---- config from Guix ----
"
"HTTP_PORT=\"" #$http "\"
"
"UI_PORT=\""   #$ui   "\"
"
"STATE_DIR=\"$HOME" #$state "\"
"
"VENV_DIR=\"$HOME"  #$venv  "\"
"
"ENV_FILE=\"$HOME/.config/ezlocalai/ezlocalai.env\"
"
"SECRETS_FILE=\"$HOME" #$secrets "\"
"
"EXTRA_REQS=\"$HOME/.config/ezlocalai/requirements.txt\"
"
"AUTO_UPDATE=\"" #$auto? "\"

"
"LOG=\"$STATE_DIR/ezlocalai.log\"
"
"mkdir -p \"$STATE_DIR\" \"$HOME/.config/ezlocalai\"
"
": >\"$LOG\"
"
"exec >>\"$LOG\" 2>&1

"
"# ---- environment bootstrap (useful in Shepherd) ----
"
"PATH=\"$HOME/.guix-home/profile/bin:$HOME/.guix-home/profile/sbin:$HOME/.guix-profile/bin:$HOME/.guix-profile/sbin:/run/current-system/profile/bin:/run/current-system/profile/sbin${PATH:+:$PATH}\"
"
"export PATH
"
"export CPATH=\"$HOME/.guix-home/profile/include${CPATH:+:$CPATH}\"
"
"export LIBRARY_PATH=\"$HOME/.guix-home/profile/lib${LIBRARY_PATH:+:$LIBRARY_PATH}\"
"
"export LD_LIBRARY_PATH=\"$HOME/.guix-home/profile/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"

"
"# ---- non-secret + secret env (if present) ----
"
"[ -r \"$ENV_FILE\" ]     && { set -a; . \"$ENV_FILE\";     set +a; }
"
"[ -r \"$SECRETS_FILE\" ] && { set -a; . \"$SECRETS_FILE\"; set +a; }

"
"# ---- venv ----
"
"if [ ! -x \"$VENV_DIR/bin/python\" ]; then
"
"  umask 022
"
"  python3 -m venv \"$VENV_DIR\" 2>/dev/null || python -m venv \"$VENV_DIR\"
"
"fi
"
"PY=\"$VENV_DIR/bin/python\"
"
"\"$PY\" -m pip install --upgrade pip wheel setuptools

"
"# ---- source checkout ----
"
"SRC_DIR=\"$STATE_DIR/src\"
"
"if [ ! -d \"$SRC_DIR/.git\" ]; then
"
"  echo \"[ezlocalai] cloning https://github.com/DevXT-LLC/ezlocalai -> $SRC_DIR\"
"
"  rm -rf \"$SRC_DIR\"
"
"  git clone https://github.com/DevXT-LLC/ezlocalai \"$SRC_DIR\"
"
"else
"
"  echo \"[ezlocalai] updating $SRC_DIR\"
"
"  (cd \"$SRC_DIR\" && git pull --ff-only || true)
"
"fi

"
"# ---- install requirements ----
"
"if [ \"$AUTO_UPDATE\" = \"true\" ] || [ ! -e \"$STATE_DIR/.installed\" ]; then
"
"  [ -f \"$SRC_DIR/requirements.txt\" ] && \"$PY\" -m pip install -r \"$SRC_DIR/requirements.txt\" || true
"
"  # extra requirements supplied by Home config (can be empty)
"
"  [ -s \"$EXTRA_REQS\" ] && \"$PY\" -m pip install -r \"$EXTRA_REQS\" || true
"
"  date > \"$STATE_DIR/.installed\"
"
"fi

"
"# ---- start API (background) ----
"

"cleanup(){ [ -f \"$STATE_DIR/uvicorn.pid\" ] && kill \"$(cat \"$STATE_DIR/uvicorn.pid\")\" 2>/dev/null || true; }
"
"trap cleanup EXIT INT TERM
"
"(cd \"$SRC_DIR\" && \"$VENV_DIR/bin/uvicorn\" app:app --host 0.0.0.0 --port \"$HTTP_PORT\") &
"
"echo $! > \"$STATE_DIR/uvicorn.pid\"

"
"# ---- start UI (foreground) ----
"
"cd \"$SRC_DIR\"
"
"exec \"$VENV_DIR/bin/streamlit\" run ui.py --server.port \"$UI_PORT\" --server.headless true
"))
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
