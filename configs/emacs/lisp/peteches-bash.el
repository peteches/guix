;;; peteches-bash.el --- Shell configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Shell behaviour lives here.  Grammar discovery and manual grammar install
;;; commands live in `peteches-treesit'.  Language server binaries are expected
;;; to come from Guix and are started only when already available.

;;; Code:

(require 'peteches-treesit)
(require 'peteches-lsp)

(defun peteches/bash-buffer-p ()
  "Return non-nil when the current `sh-mode' buffer is Bash."
  (and (derived-mode-p 'sh-mode)
       (boundp 'sh-shell)
       (eq sh-shell 'bash)))

(defun peteches/bash-use-ts-mode-when-ready ()
  "Use `bash-ts-mode' in Bash buffers when the Bash grammar is available."
  (when (and (peteches/bash-buffer-p)
             (peteches/treesit-mode-ready-p 'bash-ts-mode 'bash))
    (bash-ts-mode)))

(defun peteches/bash-maybe-start-lsp ()
  "Start Bash LSP when `bash-language-server' is available."
  (when (or (peteches/bash-buffer-p)
            (derived-mode-p 'bash-ts-mode))
    (peteches/lsp-maybe-start 'bash)))

(add-hook 'sh-mode-hook #'peteches/bash-use-ts-mode-when-ready)
(add-hook 'sh-mode-hook #'peteches/bash-maybe-start-lsp)
(add-hook 'bash-ts-mode-hook #'peteches/bash-maybe-start-lsp)

(provide 'peteches-bash)
;;; peteches-bash.el ends here
