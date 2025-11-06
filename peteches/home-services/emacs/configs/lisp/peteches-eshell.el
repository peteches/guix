;;; peteches-eshell --- my eshell configs
;;;
;;; Commentary:
;;; 
;;;
;;; Code:

(require 'eshell)
(message "Loading eshell configs")
(setq
 eshell-destroy-buffer-when-process-dies t
 eshell-prefer-lisp-functions t)

(add-to-list 'eshell-visual-commands "btop")
(provide 'peteches-eshell)
;;; peteches-eshell.el ends here
