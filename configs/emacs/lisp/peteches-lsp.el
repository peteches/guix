;;; peteches-lsp --- My LSP configs
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(message "Loading LSP Config")

(use-package lsp-mode
;  :straight '(lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode")

  ;; setup hooks
  :hook '((before-save . peteches--lsp-format-on-save)
	  (lsp-before-initialize .  (lambda ()
				      (when (file-remote-p default-directory)
					(setq-local lsp-enable-file-watchers nil))))
	  (lsp-mode . (lsp-modeline-workspace-status-mode
		       lsp-modeline-code-actions-mode
		       lsp-modeline-diagnostics-mode)))

  ;; autoload these commands
  :commands lsp

  ;; evaluate before lsp-mode loaded
  :init
  (setq lsp-keymap-prefix "C-c l")

  ;; evaluate after lsp-mode loaded
  :config
  ;; ---- Format on save when supported (replace the old var) ----
  (defun peteches--lsp-format-on-save ()
    "Ensure that buffers are formatted before saving."
    (when (and (bound-and-true-p lsp-mode)
               (lsp-feature? "textDocument/formatting"))
      (lsp-format-buffer)))

  ;; Bigger pipes + longer init timeout (helps gopls)
  (setq read-process-output-max (* 1024 1024 16)  ;; 16 MB
	lsp-response-timeout 180
	lsp-idle-delay 0.3
	gc-cons-threshold (* 100 1024 1024)

	;; File watching
	lsp-file-watch-threshold 3000)


  ;; ---- Ensure UI bits are present but don’t error if missing ----
  (dolist (feat '(lsp-modeline lsp-lens lsp-headerline))
    (require feat nil t)))

(provide 'peteches-lsp)
;;; peteches-lsp.el ends here
