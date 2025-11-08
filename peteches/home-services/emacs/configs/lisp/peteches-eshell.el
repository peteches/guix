;;; peteches-eshell --- my eshell configs
;;;
;;; Commentary:
;;; 
;;;
;;; Code:

(require 'eshell)
(require 'em-term)
(message "Loading eshell configs")
(setq
 eshell-destroy-buffer-when-process-dies t
 eshell-prefer-lisp-functions t)

(defun eshell/grhome ()
    "Run's guix home reconfigure"
  (peteches/guix-home-reconfigure))

(add-to-list 'eshell-visual-commands "btop")
(provide 'peteches-eshell)
;;; peteches-eshell.el ends here
