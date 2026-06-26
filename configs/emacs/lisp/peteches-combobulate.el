;;; peteches-combobulate.el --- Combobulate configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Enable Combobulate only in buffers that actually have a tree-sitter parser.

;;; Code:

(require 'peteches-treesit)

(use-package combobulate
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect.
  (combobulate-key-prefix "C-c <tab>")
  :preface
  (defun peteches/combobulate-enable-when-ready ()
    "Enable `combobulate-mode' when the current buffer has a tree-sitter parser."
    (when (and (fboundp 'treesit-parser-list)
               (treesit-parser-list)
               (require 'combobulate nil t))
      (condition-case err
          (combobulate-mode 1)
        (error
         (message "combobulate unavailable in %s: %s"
                  major-mode
                  (error-message-string err))))))
  :hook ((prog-mode . peteches/combobulate-enable-when-ready)))

(provide 'peteches-combobulate)
;;; peteches-combobulate.el ends here
