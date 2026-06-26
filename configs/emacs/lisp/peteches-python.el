;;; peteches-python.el --- Python configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Python behaviour lives here.  Tree-sitter grammar discovery and manual
;;; grammar install commands live in `peteches-treesit'.

;;; Code:

(defun peteches/python-enable-tsmark ()
  "Enable `tsmark-mode' in Python tree-sitter buffers when available."
  (when (require 'tsmark-backend-python nil t)
    (tsmark-mode 1)))

(add-hook 'python-ts-mode-hook #'peteches/python-enable-tsmark)

(provide 'peteches-python)
;;; peteches-python.el ends here
