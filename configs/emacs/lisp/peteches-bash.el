;;; peteches-bash.el --- Shell configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Shell behaviour lives here.  Grammar discovery and manual grammar install
;;; commands live in `peteches-treesit'.

;;; Code:

(require 'peteches-treesit)

(defun peteches/bash-use-ts-mode-when-ready ()
  "Use `bash-ts-mode' in Bash buffers when the Bash grammar is available."
  (when (and (derived-mode-p 'sh-mode)
             (boundp 'sh-shell)
             (eq sh-shell 'bash)
             (peteches/treesit-mode-ready-p 'bash-ts-mode 'bash))
    (bash-ts-mode)))

(add-hook 'sh-mode-hook #'peteches/bash-use-ts-mode-when-ready)

(provide 'peteches-bash)
;;; peteches-bash.el ends here
