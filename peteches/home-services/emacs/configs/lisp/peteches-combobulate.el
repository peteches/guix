(require 'combobulate)
(setq combobulate-key-prefix "C-c o")

(defun peteches/enable-combobulate-if-tree-sitter ()
  "Enable `combobulate-mode' if the major mode is tree-sitter based."
  (when (string-suffix-p "-ts-mode" (symbol-name major-mode))
    (combobulate-mode 1)))

(add-hook 'after-change-major-mode-hook #'peteches/enable-combobulate-if-tree-sitter)


(provide 'peteches-combobulate)
