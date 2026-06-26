;;; peteches-python.el --- Python configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Python behaviour lives here.  Tree-sitter grammar discovery and manual
;;; grammar install commands live in `peteches-treesit'.  Python LSP starts only
;;; when a configured server executable is already available on `exec-path'.

;;; Code:

(require 'peteches-lsp)

(defun peteches/python-enable-tsmark ()
  "Enable `tsmark-mode' in Python tree-sitter buffers when available."
  (when (require 'tsmark-backend-python nil t)
    (tsmark-mode 1)))

(defun peteches/python-maybe-start-lsp ()
  "Start Python LSP when a configured Python server is available."
  (peteches/lsp-maybe-start 'python))

(add-hook 'python-mode-hook #'peteches/python-maybe-start-lsp)
(add-hook 'python-ts-mode-hook #'peteches/python-enable-tsmark)
(add-hook 'python-ts-mode-hook #'peteches/python-maybe-start-lsp)

(provide 'peteches-python)
;;; peteches-python.el ends here
