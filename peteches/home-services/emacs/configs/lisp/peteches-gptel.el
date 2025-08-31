;;; peteches-gptel.el --- GPTel: OpenAI, Copilot (personal+enterprise), KoboldCPP  -*- lexical-binding: t; -*-

;; One file to wire up:
;; - OpenAI/ChatGPT  (curl + streaming)
;; - GitHub Copilot Chat: Personal & Enterprise (curl + streaming; SSE headers)
;; - Local KoboldCPP (curl + streaming)
;;
;; Design choices:
;; - **No model is set anywhere** — pick per-buffer via `M-x gptel-menu`.
;; - Copilot must stream; some proxies choke on compression. We therefore:
;;   • force curl transport + streaming
;;   • inject SSE-friendly flags/headers
;;   • strip `--compressed` and any `Accept-Encoding:` header
;;   • scrub GH_TOKEN/GITHUB_TOKEN from env for the request
;; - We do this robustly with process-level advice that rewrites curl invocations,
;;   even if gptel uses an absolute curl path (e.g. /gnu/store/.../curl).
;; - Keymap under C-c a g; log helpers; command to copy the Copilot token.

;;;; --- deps ------------------------------------------------------------------
(require 'seq)
(require 'subr-x)               ;; string-trim, string-empty-p
(require 'auth-source-pass)
(auth-source-pass-enable)
(setq auth-sources '(password-store))

(require 'gptel)                ;; core
(require 'gptel-openai)         ;; OpenAI factories
(require 'gptel-gh)             ;; Copilot Chat factory
(require 'gptel-curl)           ;; transport via curl
(require 'transient)            ;; for gptel-menu
(require 'gptel-transient)
(require 'gptel-rewrite)
(require 'gptel-context)

;;;; --- gptel simple extensions
(require 'peteches-gptel-commit)

;;;; --- pass helper -----------------------------------------------------------
(defun peteches/pass (entry &optional field)
  "Return secret from pass ENTRY; with FIELD use auth-source-pass FIELD."
  (let ((v (if field
               (auth-source-pass-get field entry)
             (auth-source-pass-get 'secret entry))))
    (when (functionp v) (setq v (funcall v)))
    (and (stringp v) (string-trim v))))

;;;; --- backends --------------------------------------------------------------

;; 1) ChatGPT / OpenAI
(setq peteches/gptel-openai
      (gptel-make-openai "OpenAI"
        :key (lambda () (or (peteches/pass "openai.com/chatgpt@peteches.co.uk" "emacs_api_key")
                            (getenv "OPENAI_API_KEY")))
        :host "api.openai.com"
        :endpoint "/v1/chat/completions"
        :stream t))

;; 2) GitHub Copilot Chat (two backends)
(setq peteches/gptel-copilot-personal
      (gptel-make-gh-copilot "Copilot (Personal)"
                             :host "api.githubcopilot.com"
                             :endpoint "/chat/completions"))

(setq peteches/gptel-copilot-enterprise
      (gptel-make-gh-copilot "Copilot (Enterprise)"
                             :host "proxy.enterprise.githubcopilot.com"
                             :endpoint "/chat/completions"))

;; 3) Local KoboldCPP (OpenAI-compatible)
(setq peteches/gptel-kobold
      (gptel-make-openai "KoboldCPP"
        :protocol "http"
        :host "127.0.0.1:5001"
        :endpoint "/v1/chat/completions"
        :stream t))

;;;; --- Copilot-specific helpers ----------------------------------------------
(defun peteches--copilot-backend-p (&optional backend)
  "Non-nil when BACKEND (or current) is one of the Copilot backends."
  (let ((bk (or backend gptel-backend)))
    (or (eq bk peteches/gptel-copilot-personal)
        (eq bk peteches/gptel-copilot-enterprise))))

(defun peteches--copilot-clean-env (env)
  "Strip PAT env that can break Copilot chat auth (safe even if unset)."
  (seq-remove (lambda (s)
                (string-match-p "\\`\\(GH_TOKEN\\|GITHUB_TOKEN\\)=" s))
              env))

;;;; --- Process-level enforcement for Copilot curl invocations ----------------
(defvar peteches--copilot-force-wrapper nil
  "When non-nil, rewrite curl args for Copilot SSE inside process creation.")

(defun peteches--curl-like-program-p (prog)
  "Return non-nil if PROG is curl (absolute or basename)."
  (and (stringp prog)
       (string= (downcase (file-name-nondirectory prog)) "curl")))

(defun peteches--filter-curl-args (args)
  "Return ARGS sans `--compressed` and any `-H` Accept-Encoding/Accept, then add SSE flags."
  (let ((out '())
        (i 0)
        (n (length args)))
    (while (< i n)
      (let ((a (nth i args)))
        (cond
         ;; drop --compressed
         ((string= a "--compressed"))
         ;; drop: -H 'Accept-Encoding: ...'
         ((and (string= a "-H") (< (1+ i) n)
               (string-match-p "\\`[Aa]ccept-[Ee]ncoding:" (nth (1+ i) args)))
          (setq i (1+ i))) ; skip header value too (incremented again below)
         ;; drop: -H 'Accept: ...' (we'll supply our own)
         ((and (string= a "-H") (< (1+ i) n)
               (string-match-p "\\`[Aa]ccept:" (nth (1+ i) args)))
          (setq i (1+ i))) ; skip header value
         ;; keep pair -H '...'
         ((and (string= a "-H") (< (1+ i) n))
          (push (nth (1+ i) args) out)
          (push "-H" out)
          (setq i (1+ i)))
         ;; keep everything else
         (t (push a out))))
      (setq i (1+ i)))
    (setq out (nreverse out))
    ;; append SSE-friendly flags & headers
    (append out
            '("--http1.1" "--no-buffer" "-N"
              "-H" "Accept: text/event-stream"
              "-H" "Accept-Encoding: identity"))))

;; Advice `make-process` to rewrite curl command when talking to Copilot
(defun peteches--adv-make-process (orig &rest args)
  (if (and peteches--copilot-force-wrapper
           (plist-get args :command))
      (let* ((cmd (plist-get args :command)))
        (when (and (listp cmd) (peteches--curl-like-program-p (car cmd)))
          (setq args (plist-put args :command
                                (cons (car cmd)
                                      (peteches--filter-curl-args (cdr cmd))))))
        (apply orig args))
    (apply orig args)))

;; Advice `start-process` (some gptel versions may use it)
(defun peteches--adv-start-process (orig name buffer program &rest program-args)
  (if (and peteches--copilot-force-wrapper
           (peteches--curl-like-program-p program))
      (apply orig name buffer program
             (peteches--filter-curl-args program-args))
    (apply orig name buffer program program-args)))

(advice-add 'make-process  :around #'peteches--adv-make-process)
(advice-add 'start-process :around #'peteches--adv-start-process)

;;;; --- per-backend prefs (transport only; **no model pinning**) ---------------
(defun peteches-apply-backend-prefs ()
  "Set buffer-local transport defaults for the active `gptel-backend`."
  (cond
   ;; OpenAI / ChatGPT — curl + streaming
   ((eq gptel-backend peteches/gptel-openai)
    (setq-local gptel-use-curl t
                gptel-stream t))
   ;; KoboldCPP — curl + streaming
   ((eq gptel-backend peteches/gptel-kobold)
    (setq-local gptel-use-curl t
                gptel-stream t))
   ;; Copilot (both) — must STREAM with curl; process advice injects SSE flags
   ((peteches--copilot-backend-p)
    (setq-local gptel-use-curl t
                gptel-stream t))))
(add-hook 'gptel-mode-hook #'peteches-apply-backend-prefs)

;; Enforce Copilot env + process rewriting for the *first* request, before hooks run.
(defun peteches--first-request-copilot-guard (orig &rest args)
  (if (peteches--copilot-backend-p)
      (let ((gptel-use-curl t)
            (gptel-stream t)
            (peteches--copilot-force-wrapper t)
            (process-environment (peteches--copilot-clean-env process-environment)))
        ;; Don't carry a model across backends; choose via `gptel-menu`
        (kill-local-variable 'gptel-model)
        (unwind-protect
            (apply orig args)
          (setq peteches--copilot-force-wrapper nil)))
    (apply orig args)))
(advice-add 'gptel-send    :around #'peteches--first-request-copilot-guard)
(advice-add 'gptel-request :around #'peteches--first-request-copilot-guard)

;;;; --- handy entry points ----------------------------------------------------
(defun peteches-gptel-openai-chat ()
  "Open an OpenAI chat buffer (curl+stream). Pick the model via `gptel-menu`."
  (interactive)
  (kill-local-variable 'gptel-model)
  (setq gptel-backend peteches/gptel-openai)
  (call-interactively #'gptel))

(defun peteches-gptel-kobold-chat ()
  "Open a KoboldCPP chat buffer (curl+stream). Pick the model via `gptel-menu`."
  (interactive)
  (kill-local-variable 'gptel-model)
  (setq gptel-backend peteches/gptel-kobold)
  (call-interactively #'gptel))

(defun peteches-gptel-copilot-chat-personal ()
  "Open a Copilot (Personal) chat; curl streaming, SSE headers via process advice; cleaned env.
Use `gptel-menu` to select the model."
  (interactive)
  (kill-local-variable 'gptel-model)
  (setq gptel-backend peteches/gptel-copilot-personal)
  (let ((process-environment (peteches--copilot-clean-env process-environment)))
    (call-interactively #'gptel)))

(defun peteches-gptel-copilot-chat-enterprise ()
  "Open a Copilot (Enterprise) chat; curl streaming, SSE headers via process advice; cleaned env.
Use `gptel-menu` to select the model."
  (interactive)
  (kill-local-variable 'gptel-model)
  (setq gptel-backend peteches/gptel-copilot-enterprise)
  (let ((process-environment (peteches--copilot-clean-env process-environment)))
    (call-interactively #'gptel)))

;;;; --- optional: show what models a backend reports (may be empty on Copilot) -
(defun peteches-gptel-show-models (&optional backend)
  "Show models reported by BACKEND (or current `gptel-backend`)."
  (interactive)
  (let* ((bk (or backend gptel-backend))
         (lst (ignore-errors
                (when (fboundp 'gptel-backend-models)
                  (gptel-backend-models bk))))
         (names (and (listp lst)
                     (mapcar (lambda (m) (format "%s" m)) lst))))
    (if names
        (message "Backend models: %s" (string-join names ", "))
      (message "This backend didn’t report a model list (common with Copilot)."))))

;;;; --- log helpers + token copier --------------------------------------------
(defgroup peteches-gptel-log nil
  "Logging helpers for gptel."
  :group 'external)

(defcustom peteches-gptel-log-level-cycle '(nil info debug)
  "Levels to cycle through when toggling `gptel-log-level`."
  :type '(repeat (choice (const nil) (const info) (const debug)))
  :group 'peteches-gptel-log)

(defun peteches-gptel-toggle-log-level (&optional prompt)
  "Cycle `gptel-log-level` through `peteches-gptel-log-level-cycle`.
With PROMPT (prefix arg), prompt for an explicit level."
  (interactive "P")
  (if prompt
      (let* ((choices (mapcar (lambda (x) (if x (symbol-name x) "nil"))
                              peteches-gptel-log-level-cycle))
             (sel (completing-read "gptel-log-level: " choices nil t
                                   (if gptel-log-level (symbol-name gptel-log-level) "nil"))))
        (setq gptel-log-level (if (string= sel "nil") nil (intern sel))))
    (let* ((cycle peteches-gptel-log-level-cycle)
           (cur   gptel-log-level)
           (tail  (member cur cycle))
           (next  (car (or (cdr tail) cycle))))
      (setq gptel-log-level next)))
  (message "gptel-log-level => %s" (or gptel-log-level 'nil)))

(defun peteches-gptel-open-log ()
  "Show the *gptel-log* buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create "*gptel-log*")))

(defun peteches-gptel-copy-copilot-token ()
  "Copy the most recent Copilot token from *gptel-log* to the kill-ring."
  (interactive)
  (let ((buf (get-buffer "*gptel-log*")))
    (unless buf (user-error "No *gptel-log* buffer yet; send a Copilot message first"))
    (with-current-buffer buf
      (goto-char (point-max))
      (if (re-search-backward "\"authorization\"\\s-*:\\s-*\"Bearer \\([^\"]+\\)\"" nil t)
          (let ((tok (match-string 1)))
            (kill-new tok)
            (message "COPILOT_TOKEN copied to kill-ring (%d chars)" (length tok)))
        (user-error "Couldn’t find an Authorization: Bearer … line in *gptel-log*")))))

;;;; --- backend switcher and keys under C-c a g --------------------------------
(defun peteches-gptel-switch-backend ()
  "Switch GPTel backend and apply transport prefs (models via `gptel-menu`)."
  (interactive)
  (let* ((pairs '(("OpenAI"               . peteches/gptel-openai)
                  ("Copilot (Personal)"   . peteches/gptel-copilot-personal)
                  ("Copilot (Enterprise)" . peteches/gptel-copilot-enterprise)
                  ("KoboldCPP"            . peteches/gptel-kobold)))
         (name (completing-read "gptel backend: " (mapcar #'car pairs) nil t))
         (sym  (alist-get name pairs nil nil #'string=)))
    (kill-local-variable 'gptel-model)                 ;; let menu decide
    (setq gptel-backend (symbol-value sym))
    (peteches-apply-backend-prefs)
    (message "gptel backend => %s (model: %s, stream:%s, curl:%s)"
             name (or (and (boundp 'gptel-model) gptel-model) "<unset>")
             (bound-and-true-p gptel-stream)
             (bound-and-true-p gptel-use-curl))))

;; Ensure a C-c a prefix exists and add a g submap
(defun peteches--ensure-c-c-a-prefix ()
  (let ((cur (key-binding (kbd "C-c a") t)))
    (if (keymapp cur) cur
      (let ((m (make-sparse-keymap))) (define-key global-map (kbd "C-c a") m) m))))

(let* ((a (peteches--ensure-c-c-a-prefix))
       (g (or (lookup-key a (kbd "g"))
              (let ((m (make-sparse-keymap))) (define-key a (kbd "g") m) m))))
  (define-key g (kbd "g") #'gptel)                                ;; current backend
  (define-key g (kbd "s") #'peteches-gptel-switch-backend)        ;; switcher
  (define-key g (kbd "m") #'gptel-menu)                           ;; menu (pick model here)
  (define-key g (kbd "o") #'peteches-gptel-openai-chat)           ;; OpenAI
  (define-key g (kbd "k") #'peteches-gptel-kobold-chat)           ;; Kobold
  (define-key g (kbd "p") #'peteches-gptel-copilot-chat-personal) ;; Copilot Personal
  (define-key g (kbd "e") #'peteches-gptel-copilot-chat-enterprise) ;; Copilot Enterprise
  (define-key g (kbd "l") #'peteches-gptel-toggle-log-level)      ;; cycle log level
  (define-key g (kbd "L") #'peteches-gptel-open-log)              ;; open *gptel-log*
  (define-key g (kbd "y") #'peteches-gptel-copy-copilot-token)    ;; copy token
  (define-key g (kbd "?") #'peteches-gptel-show-models))          ;; show backend models (if any)

;;;; --- defaults on load ------------------------------------------------------
(setq gptel-backend peteches/gptel-openai)
;; **Do not set `gptel-model` here** — choose it with `M-x gptel-menu` per buffer.
(setq-default gptel-model nil)


(require 'peteches-gptel-presets)
(require 'peteches-mcp)
(require 'gptel-integrations)

(when (and (fboundp 'gptel-mcp-connect) (fboundp 'mcp-hub-servers))
  (gptel-mcp-connect))

(provide 'peteches-gptel)
;;; peteches-gptel.el ends here
