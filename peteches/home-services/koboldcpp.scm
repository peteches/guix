;; home-services/xtts.scm
(define-module (home-services xtts)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix build utils)
  #:export (xtts-configuration
            xtts-service-type))

;; Configuration record (no Guix packages involved)
(define-record-type* <xtts-configuration>
  xtts-configuration make-xtts-configuration
  xtts-configuration?
  (venv-dir             xtts-configuration-venv-dir             ; #f -> defaults to $HOME/.local/xtts-venv
                        (default #f))
  (python-bin           xtts-configuration-python-bin           ; which python to use to create/upgrade the venv
                        (default "python3"))
  (use-cuda?            xtts-configuration-use-cuda?            ; add CUDA wheels + LD_LIBRARY_PATH if true
                        (default #t))
  (cuda-lib-dir         xtts-configuration-cuda-lib-dir         ; ignored if use-cuda? = #f
                        (default "/opt/cuda/targets/x86_64-linux/lib"))
  (pytorch-extra-index  xtts-configuration-pytorch-extra-index  ; wheel index for torch
                        (default "https://download.pytorch.org/whl/cu121"))
  (pip-packages         xtts-configuration-pip-packages         ; what to install into the venv
                        (default '("xtts-api" "torch" "xformers")))
  (extra-pip-args       xtts-configuration-extra-pip-args       ; any extra args you want to pass to pip install
                        (default '()))
  (command              xtts-configuration-command              ; how to start the server
                        ;; If you prefer a console-script, set to '("xtts-api" "--host" "0.0.0.0" "--port" "8020")
                        (default '("python" "-m" "xtts_api" "--host" "0.0.0.0" "--port" "8020")))
  (env                  xtts-configuration-env                  ; extra env for the daemon
                        (default '())))

;; Replace $HOME in a string (done at activation/runtime, not at evaluation)
(define (home-subst s)
  #~(let* ((home (getenv "HOME")))
      (if (and home #$s)
          (string-replace-substring #$s "$HOME" home)
          #$s)))

(define (activation-gexp cfg)
  (let ((venv-dir   (home-subst (xtts-configuration-venv-dir cfg)))
        (python-bin (xtts-configuration-python-bin cfg))
        (pip-pkgs   (xtts-configuration-pip-packages cfg))
        (extra-args (xtts-configuration-extra-pip-args cfg))
        (use-cuda?  (xtts-configuration-use-cuda? cfg))
        (pt-index   (xtts-configuration-pytorch-extra-index cfg)))
    #~(begin
        (use-modules (guix build utils))
        (let* ((venv (or #$venv-dir
                         (string-append (getenv "HOME") "/.local/xtts-venv")))
               (py   #$python-bin)
               (venv-python (string-append venv "/bin/python"))
               (venv-pip    (string-append venv "/bin/pip")))
          (mkdir-p venv)
          ;; Create venv if missing
          (unless (file-exists? (string-append venv "/bin/activate"))
            (format #t "Creating venv at ~a using ~a~%" venv py)
            (invoke py "-m" "venv" venv))
          ;; Upgrade bootstrap tools
          (invoke venv-python "-m" "pip" "install" "--upgrade" "pip" "setuptools" "wheel")
          ;; Build pip install command
          (let* ((install-cmd
                  (append
                   (list venv-pip "install")
                   (if #$use-cuda?
                       (list "--extra-index-url" #$pt-index)
                       '())
                   '#$extra-args
                   '#$pip-pkgs)))
            (format #t "Installing/Updating XTTS & deps in ~a ...~%" venv)
            (apply invoke install-cmd))))))

(define (service-gexp cfg)
  (let* ((venv-dir   (home-subst (xtts-configuration-venv-dir cfg)))
         (command    (xtts-configuration-command cfg))
         (env-extra  (xtts-configuration-env cfg))
         (use-cuda?  (xtts-configuration-use-cuda? cfg))
         (cuda-lib   (xtts-configuration-cuda-lib-dir cfg)))
    #~(let* ((home (getenv "HOME"))
             (venv (or #$venv-dir
                       (string-append home "/.local/xtts-venv")))
             (bin  (string-append venv "/bin/"))
             (cmd  '#$command)
             ;; Resolve first argv to the venv (either python or a console script)
             (exe  (let ((head (car cmd)))
                     (cond
                      ((string=? head "python")  (string-append bin "python"))
                      ((string=? head "python3") (string-append bin "python"))
                      (else                      (string-append bin head)))))
             (argv (cons exe (cdr cmd)))
             (env-base (list
                        (string-append "PATH=" bin ":" (or (getenv "PATH") ""))
                        "PYTHONUNBUFFERED=1"))
             (env-cuda (if #$use-cuda?
                           (list (string-append "LD_LIBRARY_PATH="
                                                #$cuda-lib
                                                ":" (or (getenv "LD_LIBRARY_PATH") "")))
                           '()))
             (env-user (map (lambda (kv)
                              (string-append (car kv) "=" (cdr kv)))
                            '#$env-extra)))
        (list argv (append env-base env-cuda env-user)))))

(define xtts-service-type
  (service-type
   (name 'xtts)
   (extensions
    (list
     ;; Bootstrap/upgrade the venv on each `guix home reconfigure`
     (service-extension home-activation-service-type
                        (lambda (cfgs)
                          (mlet %store-monad ((gexps (sequence %store-monad
                                                               (map activation-gexp cfgs))))
                            (return (apply with-imported-modules '((guix build utils))
                                           (begin
                                             #$@gexps))))))
     ;; Shepherd service that runs from the venv
     (service-extension home-shepherd-service-type
                        (lambda (cfgs)
                          (list
                           (shepherd-service
                            (provision '(xtts))
                            (documentation "XTTS API server (venv-managed).")
                            (requirement '(networking))
                            (start
                             #~(let* ((pair  #$(service-gexp (car cfgs)))
                                      (argv  (car pair))
                                      (env   (cadr pair)))
                                 (make-forkexec-constructor argv
                                                            #:directory (getenv "HOME")
                                                            #:environment-variables env)))
                            (stop #~(make-kill-destructor))))))))
   (default-value (xtts-configuration))
   (description "Run an XTTS API server from a self-managed Python venv (created and updated during home activation).")))
