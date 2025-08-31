(provide 'peteches-lsp)
(message "Loading LSP Config")

;; Set prefix BEFORE loading lsp-mode
(setq lsp-keymap-prefix "C-c l")

;; Load lsp-mode (use normal quote)
(require 'lsp-mode)

;; Bigger pipes + longer init timeout (helps gopls)
(setq read-process-output-max (* 1024 1024 16))  ;; 16 MB
(setq lsp-response-timeout 180)
(setq lsp-idle-delay 0.3)
(setq gc-cons-threshold (* 100 1024 1024))

;; File watching
(setq lsp-file-watch-threshold 500)
(add-hook 'lsp-before-initialize-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local lsp-enable-file-watchers nil))))

;; ---- Don’t use company; use CAPF (built-in completion-at-point) ----
(setq lsp-completion-provider :capf)  ;; no company needed
(with-eval-after-load 'lsp-mode
  ;; Some Guix builds try to autoconfigure company even when absent.
  ;; Make that step a no-op if company isn't installed.
  (unless (featurep 'company)
    (defun lsp--auto-configure-company (&rest _args) nil)))

;; ---- Ensure UI bits are present but don’t error if missing ----
(dolist (feat '(lsp-modeline lsp-lens lsp-headerline))
  (require feat nil t))

(with-eval-after-load 'lsp-mode
  ;; Make sure your prefix actually works
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

  ;; Only enable these if the functions exist in your build
  (when (fboundp 'lsp-modeline-workspace-status-mode)
    (add-hook 'lsp-mode-hook #'lsp-modeline-workspace-status-mode))
  (when (fboundp 'lsp-modeline-code-actions-mode)
    (add-hook 'lsp-mode-hook #'lsp-modeline-code-actions-mode))
  (when (fboundp 'lsp-modeline-diagnostics-mode)
    (add-hook 'lsp-mode-hook #'lsp-modeline-diagnostics-mode)))

;; ---- Format on save when supported (replace the old var) ----
(defun peteches--lsp-format-on-save ()
  (when (and (bound-and-true-p lsp-mode)
             (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer)))
(add-hook 'lsp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'peteches--lsp-format-on-save nil t)))
