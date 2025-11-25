;;; peteches-combobulate --- my combobulate config
;;;
;;; Commentary:
;;;
;;;
;;; Code:
(use-package combobulate
   :custom
   ;; You can customize Combobulate's key prefix here.
   ;; Note that you may have to restart Emacs for this to take effect!
   (combobulate-key-prefix "C-c <tab>")
   :hook ((prog-mode . combobulate-mode)))

(provide 'peteches-combobulate)
;;; peteches-combobulate.el ends here
