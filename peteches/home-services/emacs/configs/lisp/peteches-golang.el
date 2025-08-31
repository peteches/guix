;;; peteches-golang.el --- Supercharged Go dev kit (Projectile + GPTel + Treesit) -*- lexical-binding: t; -*-

;; Author: You <you@example.com>
;; Version: 0.3
;; Keywords: languages, go, tools
;; URL: n/a
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; Drop this on your load-path and `(require 'peteches-golang)`, then
;; `(peteches-go-activate)` to wire everything up.
;;
;; Highlights
;; - Works with go-mode or go-ts-mode (treesit).
;; - Projectile-first project awareness (falls back to project.el / VC).
;; - Manual-only Company completion; LSP-aware (company-lsp + CAPF).
;; - Format-on-save: gofumpt -> goimports -> gofmt (first found wins).
;; - Test helpers: nearest/pkg/all, bench, coverage, dlv debug, gotests.
;; - Test switching matches your convention: prefer `int_test.go` for internal,
;;   otherwise `*_test.go` for external (<pkg>_test).
;; - GPTel helpers (explain/review/tests/refactor).
;; - Tool management: status, idempotent install, upgrade to @latest.
;; - Transient menu on `C-c g` (or call `M-x peteches-go-menu`).

;;; Code:

(require 'cl-lib)
(require 'compile)

;;;; Customization -----------------------------------------------------------

(defgroup peteches-go nil
  "peteches Go dev kit."
  :group 'languages
  :prefix "peteches-go-")

(defcustom peteches-go-enable-lsp t
  "If non-nil, start LSP in Go buffers when available."
  :type 'boolean :group 'peteches-go)

(defcustom peteches-go-completion-manual-only t
  "If non-nil, Company uses manual trigger (no idle popup)."
  :type 'boolean :group 'peteches-go)

(defcustom peteches-go-format-preference '(gofumpt goimports gofmt)
  "Preferred formatters, first available wins."
  :type '(repeat (choice (const gofumpt) (const goimports) (const gofmt)))
  :group 'peteches-go)

(defcustom peteches-go-run-race t
  "If non-nil, include -race in test/build where it makes sense."
  :type 'boolean :group 'peteches-go)

(defcustom peteches-go-test-args ""
  "Extra args appended to `go test` commands."
  :type 'string :group 'peteches-go)

(defcustom peteches-go-bench-args "-bench . -benchmem"
  "Arguments used for benchmark runs."
  :type 'string :group 'peteches-go)

(defcustom peteches-go-coverage-file (locate-user-emacs-file "go-cover.out")
  "Path for the generated coverage profile."
  :type 'file :group 'peteches-go)

(defcustom peteches-go-set-compile-command t
  "If non-nil, set a sensible `compile-command` in Go buffers."
  :type 'boolean :group 'peteches-go)

(defcustom peteches-go-company-use-lsp t
  "If non-nil and company is present, add LSP backend to Go buffers."
  :type 'boolean :group 'peteches-go)

(defcustom peteches-go-auto-organize-imports t
  "If non-nil, try to organize imports on save (via LSP if available)."
  :type 'boolean :group 'peteches-go)

(defcustom peteches-go-switch-test-prefer-internal t
  "If non-nil, when on a source file prefer int_test.go over *_test.go."
  :type 'boolean :group 'peteches-go)

(defcustom peteches-go-gptel-style
  "You are a senior Go reviewer. Prefer idiomatic, race-free code. Offer small, actionable diffs."
  "System style used in GPTel helper buffers."
  :type 'string :group 'peteches-go)

;;;; Utilities ---------------------------------------------------------------

(defun peteches-go--executable-p (name)
  "Return NAME if `executable-find' NAME else nil."
  (when (executable-find name) name))

(defun peteches-go--project-root ()
  "Find the current project root. Prefer Projectile, then fallback."
  (cond
   ((and (featurep 'projectile) (fboundp 'projectile-project-root))
    (or (ignore-errors (projectile-project-root)) default-directory))
   ((fboundp 'project-current)
    (or (when-let ((proj (project-current))) (project-root proj)) default-directory))
   (t (or (vc-root-dir) default-directory))))

(defun peteches-go--in-go-module-root-p ()
  "Heuristic: is there a go.mod at the project root?"
  (let ((root (peteches-go--project-root)))
    (file-exists-p (expand-file-name "go.mod" root))))

(defun peteches-go--buffer-package-dir ()
  "Directory where go commands should run for this buffer."
  (or (locate-dominating-file default-directory "go.mod")
      default-directory))

(defun peteches-go--call-in (dir cmd)
  "Run shell CMD with `default-directory' bound to DIR via `compile'."
  (let ((default-directory dir))
    (compile cmd)))

;;;; Treesit / go-ts-mode ----------------------------------------------------

(defun peteches-go-install-treesit-grammars ()
  "Install Go-related tree-sitter grammars if Emacs supports treesit."
  (interactive)
  (unless (fboundp 'treesit-install-language-grammar)
    (user-error "Your Emacs doesn't have built-in tree-sitter (treesit) support"))
  (let* ((alist (or (bound-and-true-p treesit-language-source-alist) '()))
         (sources '((go     . ("https://github.com/tree-sitter/tree-sitter-go"))
                    (gomod  . ("https://github.com/camdencheek/tree-sitter-go-mod"))
                    (gowork . ("https://github.com/omertuc/tree-sitter-go-work"))))
         (merged (cl-loop for (lang . src) in sources
                          do (setf (alist-get lang alist) src)
                          finally return alist)))
    (setq treesit-language-source-alist merged)
    (dolist (lang '(go gomod gowork))
      (treesit-install-language-grammar lang))))

;;;; File associations (prefer -ts modes) --------------------------------
;; Ensure .go / go.mod / go.work open in a Go mode (ts if available).
;; Without this, remapping go-mode→go-ts-mode never triggers and buffers
;; can end up in Fundamental.
(defun peteches-go--treesit-ready-p ()
  "Return non-nil if go-ts-mode is available (grammar present if required)."
  (and (fboundp 'go-ts-mode)
       (cond
        ((fboundp 'treesit-available-p) (treesit-available-p))
        (t t))
       (or (not (fboundp 'treesit-language-available-p))
           (treesit-language-available-p 'go))))

(defun peteches-go--associate-modes ()
  "Associate Go-related filenames with appropriate major modes.
Prefers tree-sitter modes when available; otherwise falls back."
  (let ((ts (peteches-go--treesit-ready-p)))
    (if ts
        (progn
          (add-to-list 'auto-mode-alist '("\\.go\\'"     . go-ts-mode))
          (add-to-list 'auto-mode-alist '("go\\.mod\\'"  . go-mod-ts-mode))
          (add-to-list 'auto-mode-alist '("go\\.work\\'" . go-mod-ts-mode)))
      (progn
        (add-to-list 'auto-mode-alist '("\\.go\\'"     . go-mode))
        (add-to-list 'auto-mode-alist '("go\\.mod\\'"  . go-mod-mode))
        (add-to-list 'auto-mode-alist '("go\\.work\\'" . go-mod-mode))))
    ;; Handle templated names like foo.tmpl.go to still enter Go mode
    (add-to-list 'auto-mode-alist
                 '("\\.go\\.[^/]+\\'" . (lambda ()
                                          (if (peteches-go--treesit-ready-p)
                                              (go-ts-mode)
                                            (go-mode)))))))

(when (boundp 'major-mode-remap-alist)
  ;; Prefer treesit modes when available.
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))

;;;; Formatting & save -------------------------------------------------------

(defun peteches-go-choose-formatter ()
  "Return the first available formatter from `peteches-go-format-preference'."
  (or (cl-loop for f in peteches-go-format-preference
               for exe = (pcase f
                           ('gofumpt (peteches-go--executable-p "gofumpt"))
                           ('goimports (peteches-go--executable-p "goimports"))
                           ('gofmt (peteches-go--executable-p "gofmt")))
               when exe return exe)
      "gofmt"))

;;;; Keep point when formatting on save -----------------------------------
;; Some formatters (or naive uses of `shell-command-on-region`) replace the
;; whole buffer, which moves point to the end. We wrap formatting in a
;; marker-preserving function and ensure only ONE formatter runs.

(defun peteches/go--lsp-can-format-p ()
  "Non-nil if LSP is active and formatting is supported in this buffer."
  (and (bound-and-true-p lsp-mode)
       (fboundp 'lsp-feature?)
       (lsp-feature? "textDocument/formatting")))

(defun peteches/go-format-buffer ()
  "Format current Go buffer while preserving point and window start."
  (interactive)
  (let* ((pt (copy-marker (point) t))               ; insertion-type=t -> marker follows edits
         (win (selected-window))
         (start (window-start win))
         (inhibit-redisplay t))
    (unwind-protect
        (cond
         ;; Prefer LSP (gopls keeps point very well)
         ((peteches/go--lsp-can-format-p)
          (when (fboundp 'lsp-format-buffer) (lsp-format-buffer))
          (when (fboundp 'lsp-organize-imports) (lsp-organize-imports)))
         ;; Fallback to gofmt (stable cursor)
         ((fboundp 'gofmt) (gofmt))
         ;; Last resort: indent-region (never moves point)
         (t (indent-region (point-min) (point-max))))
      ;; Restore point & window
      (when (marker-position pt) (goto-char pt))
      (set-marker pt nil)
      (when (window-live-p win) (set-window-start win start t)))))

;; Ensure only our formatter runs in Go buffers (avoid double-runs that jump)
(defun peteches/go-enable-format-on-save ()
  "Enable point-preserving format-on-save for Go buffers."
  (interactive)
  ;; Remove any existing Go formatters that might replace the whole buffer.
  (remove-hook 'before-save-hook 'gofmt-before-save t)
  (when (fboundp 'eglot-format) ; just in case
    (remove-hook 'before-save-hook #'eglot-format t))
  ;; If you had your own goimports-on-save function, disable it locally:
  (when (fboundp 'peteches/goimports-on-save)
    (remove-hook 'before-save-hook #'peteches/goimports-on-save t))
  ;; Add our safe wrapper
  (add-hook 'before-save-hook #'peteches/go-format-buffer nil t))

;; Wire it into both classic and ts modes
(add-hook 'go-mode-hook     #'peteches/go-enable-format-on-save)
(when (fboundp 'go-ts-mode)
  (add-hook 'go-ts-mode-hook #'peteches/go-enable-format-on-save))

(defun peteches-go-organize-imports ()
  "Organize imports using LSP if available; otherwise rely on goimports fmt."
  (interactive)
  (cond
   ((and (featurep 'lsp-mode) (bound-and-true-p lsp-mode))
    (when (fboundp 'lsp-organize-imports) (lsp-organize-imports)))
   (t
    (when (string-match-p "goimports" (peteches-go-choose-formatter))
      (peteches-go-format-buffer)))))

(defun peteches-go-before-save ()
  "Format and organize imports before save, as configured."
  (when (derived-mode-p 'go-mode 'go-ts-mode)
    (peteches-go-format-buffer)
    (when peteches-go-auto-organize-imports
      (peteches-go-organize-imports))))

;;;; Company (manual, LSP-aware) --------------------------------------------

(defun peteches-go--setup-company ()
  "Configure Company for Go buffers in a minimal, manual way."
  (when (require 'company nil t)
    (when peteches-go-completion-manual-only
      (setq-local company-idle-delay nil)
      (setq-local company-minimum-prefix-length 1))
    (let* ((have-lsp (and peteches-go-company-use-lsp
                           (require 'company-lsp nil t)))
           (group (delq nil (list (and have-lsp 'company-lsp)
                                  'company-capf 'company-dabbrev-code 'company-files))))
      (set (make-local-variable 'company-backends) (list group)))))

;;;; LSP (gopls) -------------------------------------------------------------

(defun peteches-go--maybe-start-lsp ()
  "Start LSP in this buffer when available and enabled."
  (when (and peteches-go-enable-lsp (require 'lsp-mode nil t))
    (when (fboundp 'lsp-deferred) (lsp-deferred))))

(ignore-errors
  (when (require 'lsp-mode nil t)
    (when (fboundp 'lsp-register-custom-settings)
      (lsp-register-custom-settings
       '(("gopls.usePlaceholders" t t)
         ("gopls.staticcheck" t t)
         ("gopls.gofumpt" t t)
         ("gopls.hints.assignVariableTypes" t t)
         ("gopls.hints.compositeLiteralFields" t t)
         ("gopls.hints.compositeLiteralTypes" t t)
         ("gopls.hints.parameterNames" t t)
         ("gopls.hints.rangeVariableTypes" t t)
         ("gopls.codelenses.generate" t t)
         ("gopls.directoryFilters" ["-**/vendor" "+"]))))))

;;;; Tests / Bench / Coverage / Lint / Vuln / DLV ----------------------------

(defun peteches-go--race-flag () (if peteches-go-run-race " -race" ""))

(defun peteches-go-run (&optional args)
  "Run `go run` on current package. With ARGS, append to command."
  (interactive "sExtra args for go run: ")
  (peteches-go--call-in (peteches-go--buffer-package-dir)
                        (format "go run%s . %s" (peteches-go--race-flag) args)))

(defun peteches-go-build (&optional args)
  "Run `go build` from project root (or package dir if no module)."
  (interactive "sExtra args for go build: ")
  (let ((dir (if (peteches-go--in-go-module-root-p)
                 (peteches-go--project-root)
               (peteches-go--buffer-package-dir))))
    (peteches-go--call-in dir (format "go build%s ./... %s" (peteches-go--race-flag) args))
    (message "Building (recursive) with race=%s" (if peteches-go-run-race "on" "off"))))

(defun peteches-go--nearest-test-name ()
  "Return the TestName at point by scanning backward."
  (save-excursion
    (when (re-search-backward "^func[[:space:]]+\(Test[[:alnum:]_]+\)\(" nil t)
      (match-string 1))))

(defun peteches-go-test-nearest ()
  "Run the nearest test function in current package."
  (interactive)
  (let* ((name (peteches-go--nearest-test-name))
         (pkgdir (peteches-go--buffer-package-dir))
         (args (string-trim (concat peteches-go-test-args)))
         (cmd (if name
                  (format "go test%s -run '^%s$' %s" (peteches-go--race-flag) name args)
                (format "go test%s %s" (peteches-go--race-flag) args))))
    (peteches-go--call-in pkgdir cmd)))

(defun peteches-go-test-package ()
  "Run all tests in the current package."
  (interactive)
  (let* ((pkgdir (peteches-go--buffer-package-dir))
         (args (string-trim (concat "-count=1 " peteches-go-test-args))))
    (peteches-go--call-in pkgdir (format "go test%s %s" (peteches-go--race-flag) args))))

(defun peteches-go-test-all ()
  "Run tests recursively from project/module root."
  (interactive)
  (let ((dir (if (peteches-go--in-go-module-root-p)
                 (peteches-go--project-root)
               (peteches-go--buffer-package-dir)))
        (args (string-trim (concat "-count=1 " peteches-go-test-args))))
    (peteches-go--call-in dir (format "go test%s ./... %s" (peteches-go--race-flag) args))))

(defun peteches-go-bench-all ()
  "Run benchmarks recursively from module root."
  (interactive)
  (let ((dir (if (peteches-go--in-go-module-root-p)
                 (peteches-go--project-root)
               (peteches-go--buffer-package-dir))))
    (peteches-go--call-in dir (format "go test %s ./..." peteches-go-bench-args))))

(defun peteches-go-coverage ()
  "Generate and open coverage report (HTML in browser + summary buffer)."
  (interactive)
  (let* ((dir (if (peteches-go--in-go-module-root-p)
                  (peteches-go--project-root)
                (peteches-go--buffer-package-dir)))
         (out (expand-file-name peteches-go-coverage-file)))
    (peteches-go--call-in dir (format "go test -coverprofile=%s ./..." (shell-quote-argument out)))
    (when (executable-find "go")
      (let ((html (concat out ".html")))
        (start-process "go-cover-html" nil "go" "tool" "cover" "-html" out "-o" html)
        (run-at-time 0.5 nil #'browse-url-of-file html)))
    (when (executable-find "go")
      (let ((buf (get-buffer-create "*Go Cover*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert (format "Coverage summary for %s

" dir))
          (call-process "go" nil buf nil "tool" "cover" "-func" out)
          (goto-char (point-min))
          (view-mode 1))
        (display-buffer buf)))))

(defun peteches-go-vulncheck ()
  "Run govulncheck over the module (if available)."
  (interactive)
  (if (peteches-go--executable-p "govulncheck")
      (peteches-go--call-in (peteches-go--project-root) "govulncheck ./...")
    (user-error "govulncheck not found; install with: go install golang.org/x/vuln/cmd/govulncheck@latest")))

(defun peteches-go-lint ()
  "Run golangci-lint or staticcheck, whichever is available."
  (interactive)
  (cond
   ((peteches-go--executable-p "golangci-lint")
    (peteches-go--call-in (peteches-go--project-root) "golangci-lint run"))
   ((peteches-go--executable-p "staticcheck")
    (peteches-go--call-in (peteches-go--project-root) "staticcheck ./..."))
   (t (user-error "No linter found: install golangci-lint or staticcheck"))))

(defun peteches-go-mod-tidy () (interactive)
  (peteches-go--call-in (peteches-go--project-root) "go mod tidy"))

(defun peteches-go-generate () (interactive)
  (peteches-go--call-in (peteches-go--project-root) "go generate ./..."))

;; DLV (debug/test)
(defun peteches-go-dlv-test-nearest ()
  "Debug the nearest test with Delve (dlv)."
  (interactive)
  (unless (peteches-go--executable-p "dlv") (user-error "dlv not found"))
  (let* ((name (or (peteches-go--nearest-test-name) ""))
         (pkgdir (peteches-go--buffer-package-dir))
         (flag (if (string-empty-p name) "" (format " -- -test.run '^%s$'" name)))
         (build (concat " --build-flags '" (string-trim (concat (peteches-go--race-flag))) "'")))
    (peteches-go--call-in pkgdir (format "dlv test%s%s" build flag))))

;;;; Switch source <-> tests (internal vs external) -------------------------

(defun peteches-go--go-src-file-p (f)
  (and f (string-match-p "\.go$" f)
       (not (string-match-p "_test\.go$" f))
       (not (string-match-p "_int_test\.go$" f))))

(defun peteches-go--int-test-file (dir)
  (expand-file-name "int_test.go" dir))

(defun peteches-go--ext-test-file (src)
  (concat (file-name-sans-extension src) "_test.go"))

(defun peteches-go-switch-file-test ()
  "Toggle between source and tests honoring int_test.go vs *_test.go."
  (interactive)
  (let* ((file (buffer-file-name))
         (dir (and file (file-name-directory file)))
         (is-int (and file (string-suffix-p "_int_test.go" file)))
         (is-ext (and file (string-suffix-p "_test.go" file)))
         (is-any-test (or is-int is-ext)))
    (cond
     (is-any-test
      (let* ((stem (file-name-sans-extension (file-name-nondirectory file)))
             (maybe-src (when is-ext
                          (concat dir (string-remove-suffix "_test" stem) ".go")))
             (target (cond ((and maybe-src (file-exists-p maybe-src)) maybe-src)
                           (t (car (seq-filter #'peteches-go--go-src-file-p
                                               (directory-files dir t "\.go$")))))))
        (if target (find-file target) (user-error "No non-test .go files here"))))
     ((peteches-go--go-src-file-p file)
      (let* ((intf (peteches-go--int-test-file dir))
             (extf (peteches-go--ext-test-file file))
             (candidate (if peteches-go-switch-test-prefer-internal
                            (if (file-exists-p intf) intf extf)
                          (if (file-exists-p extf) extf intf))))
        (cond
         ((and candidate (file-exists-p candidate)) (find-file candidate))
         ((string-suffix-p "int_test.go" candidate) (find-file-other-window candidate))
         (t (find-file-other-window candidate)))))
     (t (user-error "Not visiting a Go file")))))

;; gotests helpers
(defun peteches-go-create-tests-internal ()
  "Use `gotests` to (re)write internal tests into int_test.go."
  (interactive)
  (unless (peteches-go--executable-p "gotests")
    (user-error "gotests not found; install with: go install github.com/cweill/gotests/...@latest"))
  (let* ((file (buffer-file-name))
         (dir (file-name-directory file))
         (intf (peteches-go--int-test-file dir))
         (cmd (format "gotests -w -all %s" (shell-quote-argument file))))
    (peteches-go--call-in dir cmd)
    (when (and (file-exists-p (peteches-go--ext-test-file file))
               (not (file-exists-p intf)))
      (rename-file (peteches-go--ext-test-file file) intf t)
      (message "Moved external test to %s" intf))))

(defun peteches-go-create-tests-external ()
  "Use `gotests` to (re)write external tests into *_test.go and set package _test."
  (interactive)
  (unless (peteches-go--executable-p "gotests")
    (user-error "gotests not found; install with: go install github.com/cweill/gotests/...@latest"))
  (let* ((file (buffer-file-name))
         (dir (file-name-directory file))
         (extf (peteches-go--ext-test-file file)))
    (peteches-go--call-in dir (format "gotests -w -all %s" (shell-quote-argument file)))
    (when (file-exists-p extf)
      (with-temp-buffer
        (insert-file-contents extf)
        (goto-char (point-min))
        (when (re-search-forward "^package[[:space:]]+\([^[:space:]]+\)" nil t)
          (let* ((pkg (match-string 1))
                 (ext (concat pkg "_test")))
            (goto-char (point-min))
            (when (re-search-forward (concat "^package[[:space:]]+" (regexp-quote pkg)) nil t)
              (replace-match (concat "package " ext) t t))
            (write-region (point-min) (point-max) extf)))))
    (message "External tests written: %s" extf)))

;;;; Projectile integration --------------------------------------------------

(defun peteches-go--projectile-optimize ()
  "Opinionated Projectile speed tweaks for large Go trees."
  (when (require 'projectile nil t)
    (dolist (file '("go.sum" "go.work.sum"))
      (add-to-list 'projectile-globally-ignored-files file))
    (when (fboundp 'projectile-register-project-type)
      (projectile-register-project-type 'go '("go.mod")
                                        :project-file "go.mod"
                                        :compile "go build ./..."
                                        :test    "go test ./..."
                                        :run     "go run ."
                                        :test-suffix "_test"))))

(defun peteches-go-projectile-grep (pattern)
  "Ripgrep for PATTERN respecting .gitignore and vendor filters."
  (interactive "srg pattern: ")
  (when (and (require 'projectile nil t) (fboundp 'projectile-ripgrep))
    (projectile-ripgrep pattern)))

;;;; GPTel helpers -----------------------------------------------------------

(defun peteches-go--region-or-defun ()
  "Return (TEXT . HINT) from region or defun at point."
  (if (use-region-p)
      (cons (buffer-substring-no-properties (region-beginning) (region-end)) "region")
    (save-excursion
      (mark-defun)
      (cons (buffer-substring-no-properties (region-beginning) (region-end)) "defun"))))

(defun peteches-go--gptel-scratch (name prompt content)
  "Open a gptel buffer NAME, insert PROMPT and CONTENT, and enable gptel-mode."
  (unless (fboundp 'gptel-mode)
    (user-error "gptel not installed; see https://github.com/karthink/gptel"))
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "System: %s

" peteches-go-gptel-style))
      (insert prompt "

")
      (insert "```go
" content "
```
"))
    (with-current-buffer buf
      (gptel-mode)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "Edit as needed, then C-c C-c to send via gptel.")))

(defun peteches-go-gptel-explain () (interactive)
  (pcase-let* ((`(,text . ,which) (peteches-go--region-or-defun)))
    (peteches-go--gptel-scratch "*gptel:Go Explain*"
                                (format "Explain the following Go %s for a teammate. Include invariants, error paths, and concurrency considerations." which)
                                text)))

(defun peteches-go-gptel-review () (interactive)
  (pcase-let* ((`(,text . ,which) (peteches-go--region-or-defun)))
    (peteches-go--gptel-scratch "*gptel:Go Review*"
                                (format "Review this Go %s. Suggest small diffs for clarity, performance, and correctness. Avoid large rewrites." which)
                                text)))

(defun peteches-go-gptel-generate-tests-internal () (interactive)
  (pcase-let* ((`(,text . ,which) (peteches-go--region-or-defun)))
    (peteches-go--gptel-scratch "*gptel:Go Tests (internal)*"
                                (format "Write table-driven tests for this Go %s. Assume same package (internal tests). Use t.Run subtests and helpful names." which)
                                text)))

(defun peteches-go-gptel-generate-tests-external () (interactive)
  (pcase-let* ((`(,text . ,which) (peteches-go--region-or-defun)))
    (peteches-go--gptel-scratch "*gptel:Go Tests (external)*"
                                (format "Write tests for this Go %s as an external consumer (<pkg>_test). Avoid reaching into unexported details." which)
                                text)))

(defun peteches-go-gptel-refactor-idiomatic () (interactive)
  (pcase-let* ((`(,text . ,which) (peteches-go--region-or-defun)))
    (peteches-go--gptel-scratch "*gptel:Go Refactor*"
                                (format "Suggest idiomatic improvements to this Go %s. Respect API, keep diffs minimal, consider error handling, context, and cancellations." which)
                                text)))

;;;; Tool management ---------------------------------------------------------

(defcustom peteches-go-tool-specs
  '((:exe "gopls"          :go "golang.org/x/tools/gopls@latest"                     :desc "LSP server")
    (:exe "dlv"            :go "github.com/go-delve/delve/cmd/dlv@latest"           :desc "Delve debugger")
    (:exe "gofumpt"        :go "mvdan.cc/gofumpt@latest"                             :desc "Formatter (gofumpt)")
    (:exe "goimports"      :go "golang.org/x/tools/cmd/goimports@latest"             :desc "Imports formatter")
    (:exe "golangci-lint"  :go "github.com/golangci/golangci-lint/cmd/golangci-lint@latest" :desc "Meta linter")
    (:exe "staticcheck"    :go "honnef.co/go/tools/cmd/staticcheck@latest"           :desc "Static analysis")
    (:exe "govulncheck"    :go "golang.org/x/vuln/cmd/govulncheck@latest"            :desc "Vulnerability scanner")
    (:exe "gotests"        :go "github.com/cweill/gotests/...@latest"                :desc "Table test generator")
    (:exe "gomodifytags"   :go "github.com/fatih/gomodifytags@latest"                :desc "Struct tag editor")
    (:exe "impl"           :go "github.com/josharian/impl@latest"                    :desc "Interface impl stubs"))
  "Specification of Go tools to check/install. Each entry is a plist with :exe, :go, :desc."
  :type '(repeat plist)
  :group 'peteches-go)

(defun peteches-go--tool-installed-p (exe)
  "Return non-nil if EXE is found on PATH."
  (and (executable-find exe) t))

(defun peteches-go-tools-status ()
  "Show a summary buffer of Go tool availability and install targets."
  (interactive)
  (let ((buf (get-buffer-create "*Go Tools*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "GOBIN: %s
" (or (getenv "GOBIN") (expand-file-name "bin" (or (getenv "GOPATH") (expand-file-name "go" (getenv "HOME")))))))
      (insert (format "GOROOT: %s
" (or (getenv "GOROOT") "(env not set)")))
      (insert (format "GO Version: %s

"
                      (condition-case nil
                          (string-trim (with-output-to-string (call-process "go" nil standard-output nil "version")))
                        (error "go not found"))))
      (insert (format "%-16s %-9s %s
" "Executable" "Status" "Install path"))
      (insert (make-string 72 ?-) "
")
      (dolist (spec peteches-go-tool-specs)
        (let* ((exe (plist-get spec :exe))
               (target (plist-get spec :go))
               (ok (peteches-go--tool-installed-p exe)))
          (insert (format "%-16s %-9s %s
" exe (if ok "installed" "missing") target))))
      (goto-char (point-min))
      (view-mode 1))
    (display-buffer buf)))

(defun peteches-go-install-tool (exe &optional force)
  "Install a single tool by EXE using `go install`.
If FORCE (\[universal-argument]) is non-nil, reinstall even if already present."
  (interactive (list (completing-read "Install tool: " (mapcar (lambda (s) (plist-get s :exe)) peteches-go-tool-specs) nil t)
                     current-prefix-arg))
  (unless (executable-find "go") (user-error "'go' not found on PATH"))
  (let* ((spec (cl-find-if (lambda (s) (string= (plist-get s :exe) exe)) peteches-go-tool-specs))
         (target (plist-get spec :go)))
    (unless spec (user-error "Unknown tool: %s" exe))
    (if (and (peteches-go--tool-installed-p exe) (not force))
        (message "%s already installed. Use C-u to reinstall/upgrade." exe)
      (peteches-go--call-in (peteches-go--project-root)
                            (format "go install -v %s" target)))))

(defun peteches-go-ensure-tools (&optional install-all)
  "Ensure required Go tools are present; offer to install missing ones.
With prefix arg INSTALL-ALL, install all missing without prompting."
  (interactive "P")
  (unless (executable-find "go") (user-error "'go' not found on PATH"))
  (let* ((missing (cl-remove-if (lambda (s) (peteches-go--tool-installed-p (plist-get s :exe))) peteches-go-tool-specs))
         (targets (mapcar (lambda (s) (plist-get s :go)) missing)))
    (if (null targets)
        (progn (message "All Go tools are installed.") (peteches-go-tools-status))
      (when (or install-all (y-or-n-p (format "Install %d missing tool(s)? %s " (length targets) (mapconcat #'identity targets ", "))))
        (let ((cmd (mapconcat (lambda (tgt) (format "go install -v %s" tgt)) targets " && ")))
          (peteches-go--call-in (peteches-go--project-root) cmd))))))

(defun peteches-go-upgrade-tool (exe)
  "Upgrade a single tool EXE to @latest via `go install`."
  (interactive (list (completing-read "Upgrade tool: " (mapcar (lambda (s) (plist-get s :exe)) peteches-go-tool-specs) nil t)))
  (unless (executable-find "go") (user-error "'go' not found on PATH"))
  (let* ((spec (cl-find-if (lambda (s) (string= (plist-get s :exe) exe)) peteches-go-tool-specs))
         (target (plist-get spec :go)))
    (unless spec (user-error "Unknown tool: %s" exe))
    (peteches-go--call-in (peteches-go--project-root) (format "go install -v %s" target))))

(defun peteches-go-upgrade-tools (&optional only-installed)
  "Upgrade tools to @latest using `go install`.
With prefix arg ONLY-INSTALLED, limit to tools currently on PATH; otherwise upgrade all."
  (interactive "P")
  (unless (executable-find "go") (user-error "'go' not found on PATH"))
  (let* ((specs (if only-installed
                    (cl-remove-if-not (lambda (s) (peteches-go--tool-installed-p (plist-get s :exe))) peteches-go-tool-specs)
                  peteches-go-tool-specs))
         (targets (mapcar (lambda (s) (plist-get s :go)) specs)))
    (if (null targets)
        (message "No tools to upgrade.")
      (when (y-or-n-p (format "Upgrade %d tool(s) to @latest? %s " (length targets) (mapconcat #'identity targets ", ")))
        (let ((cmd (mapconcat (lambda (tgt) (format "go install -v %s" tgt)) targets " && ")))
          (peteches-go--call-in (peteches-go--project-root) cmd))))))

;;;; Minor mode & keymap -----------------------------------------------------

(defvar peteches-go-minor-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Transient menu
    (define-key m (kbd "C-c g .") #'peteches-go-menu)
    ;; Legacy direct bindings
    (define-key m (kbd "C-c g r") #'peteches-go-run)
    (define-key m (kbd "C-c g b") #'peteches-go-build)
    (define-key m (kbd "C-c g t") #'peteches-go-test-nearest)
    (define-key m (kbd "C-c g p") #'peteches-go-test-package)
    (define-key m (kbd "C-c g a") #'peteches-go-test-all)
    (define-key m (kbd "C-c g B") #'peteches-go-bench-all)
    (define-key m (kbd "C-c g f") #'peteches-go-format-buffer)
    (define-key m (kbd "C-c g o") #'peteches-go-organize-imports)
    (define-key m (kbd "C-c g l") #'peteches-go-lint)
    (define-key m (kbd "C-c g v") #'peteches-go-vulncheck)
    (define-key m (kbd "C-c g m") #'peteches-go-mod-tidy)
    (define-key m (kbd "C-c g g") #'peteches-go-generate)
    (define-key m (kbd "C-c g s") #'peteches-go-switch-file-test)
    (define-key m (kbd "C-c g n") #'peteches-go-create-tests-internal)
    (define-key m (kbd "C-c g N") #'peteches-go-create-tests-external)
    (define-key m (kbd "C-c g d") #'peteches-go-dlv-test-nearest)
    (define-key m (kbd "C-c g i") #'peteches-go-ensure-tools)
    m)
  "Keymap for `peteches-go-minor-mode'.")

(define-minor-mode peteches-go-minor-mode
  "Minor mode offering batteries-included Go helpers."
  :lighter " PeteGo"
  :keymap peteches-go-minor-mode-map)

(defun peteches-go--buffer-setup ()
  "Setup hooks and defaults for a Go buffer."
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 8)
  (when peteches-go-set-compile-command
    (setq-local compile-command
                (format "cd %s && go build%s ./..."
                        (shell-quote-argument (peteches-go--project-root))
                        (peteches-go--race-flag))))
  (add-hook 'before-save-hook #'peteches-go-before-save nil t)
  (peteches-go--setup-company)
  (peteches-go--maybe-start-lsp)
  (peteches-go-minor-mode 1))

;;;; Transient menu ----------------------------------------------------------

;; Exported menu entry. Works even if transient isn't installed.
;;;###autoload
(defun peteches-go-menu ()
  "Open the Go toolbox menu. Requires `transient`."
  (interactive)
  (if (require 'transient nil t)
      (call-interactively 'peteches-go-menu)
    (user-error "Package 'transient' not found. Install it and try again.")))

(when (require 'transient nil t)
  (defun peteches-go--read-module-name ()
    (let* ((root (peteches-go--project-root))
           (gomod (and root (expand-file-name "go.mod" root))))
      (when (and gomod (file-exists-p gomod))
        (with-temp-buffer
          (insert-file-contents gomod)
          (goto-char (point-min))
          (when (re-search-backward "^module[[:space:]]+\([^[:space:]]+\)" nil t)
            (match-string 1)))))))

  (defun peteches-go--status-summary ()
    (let* ((root (abbreviate-file-name (peteches-go--project-root)))
           (mod  (or (peteches-go--read-module-name) "(no go.mod)"))
           (fmt  (file-name-nondirectory (peteches-go-choose-formatter)))
           (tools (mapconcat #'identity (cl-loop for it in '("gopls" "dlv" "golangci-lint")
                                                 collect (propertize it 'face (if (peteches-go--executable-p it) 'success 'shadow))) ", ")))
      (format "Root: %s
Module: %s
Formatter: %s
Tools: %s" root mod fmt tools))

  (transient-define-prefix peteches-go-menu ()
    "Go toolbox"
    ["Status"
     ("," "show" (lambda () (interactive) (message "%s" (peteches-go--status-summary))))]
    ["Run / Build / Debug"
     ("r" "run"             peteches-go-run)
     ("b" "build"           peteches-go-build)
     ("d" "dlv test nearest" peteches-go-dlv-test-nearest)
     ("P" "ripgrep (proj)"  peteches-go-projectile-grep)]
    ["Test / Bench / Coverage"
     ("t" "test nearest"    peteches-go-test-nearest)
     ("p" "test package"    peteches-go-test-package)
     ("a" "test all"        peteches-go-test-all)
     ("B" "bench all"       peteches-go-bench-all)
     ("C" "coverage"        peteches-go-coverage)]
    ["Format / Lint / Generate"
     ("f" "format"          peteches-go-format-buffer)
     ("o" "organize imports" peteches-go-organize-imports)
     ("l" "lint"            peteches-go-lint)
     ("v" "vulncheck"       peteches-go-vulncheck)
     ("m" "mod tidy"        peteches-go-mod-tidy)
     ("g" "generate"        peteches-go-generate)]
    ["Navigate / Tools"
     ("s" "switch src⇄test" peteches-go-switch-file-test)
     ("n" "gotests internal" peteches-go-create-tests-internal)
     ("N" "gotests external" peteches-go-create-tests-external)
     ("T" "treesit grammars" peteches-go-install-treesit-grammars)
     ("I" "install tools"    peteches-go-ensure-tools)
     ("U" "upgrade tools"    peteches-go-upgrade-tools)
     ("S" "tools status"     peteches-go-tools-status)
     ("D" "go doc at point" peteches-go-godoc-at-point)]
    ["GPTel"
     ("Ge" "explain" peteches-go-gptel-explain)
     ("Gr" "review"  peteches-go-gptel-review)
     ("Gi" "tests (int)" peteches-go-gptel-generate-tests-internal)
     ("Gx" "tests (ext)" peteches-go-gptel-generate-tests-external)
     ("Gf" "refactor" peteches-go-gptel-refactor-idiomatic)]) )

;;;; Public entry points -----------------------------------------------------

;;;###autoload
(defun peteches-go-activate ()
  "Activate the peteches Go kit in go-mode/go-ts-mode and optimize Projectile."
  (interactive)
  ;; Prefer treesit mode when available
  (when (boundp 'major-mode-remap-alist)
    (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode)))
  ;; Ensure files actually enter a Go mode automatically
  (peteches-go--associate-modes)
  ;; Optional: richer highlighting when using treesit
  (when (boundp 'treesit-font-lock-level) (setq treesit-font-lock-level 4))
  ;; Hooks
  (add-hook 'go-mode-hook #'peteches-go--buffer-setup)
  (when (fboundp 'go-ts-mode)
    (add-hook 'go-ts-mode-hook #'peteches-go--buffer-setup))
  ;; Projectile speed-ups
  (peteches-go--projectile-optimize)
  (message "peteches-golang: activated"))

;;;###autoload
(defun peteches-go-deactivate ()
  "Remove our hooks from go-mode and go-ts-mode."
  (interactive)
  (remove-hook 'go-mode-hook #'peteches-go--buffer-setup)
  (when (fboundp 'go-ts-mode)
    (remove-hook 'go-ts-mode-hook #'peteches-go--buffer-setup))
  (message "peteches-golang: deactivated"))

(provide 'peteches-golang)
;;; peteches-golang.el ends here
