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
    "Run's guix home reconfigure."
  (peteches/guix-home-reconfigure))

(defun eshell/ecd ()
    "Make a better more interactive cd for eshell."
  (let ((subdirs (split-string
		  (shell-command-to-string (format "find %s -type d " default-directory)) "\n")))
    (eshell/cd (completing-read "Choose directory to cd: " subdirs 'stringp))))

(use-package aweshell
  :straight (aweshell :host github :repo "manateelazycat/aweshell")
  :config (require 'eshell-up))

(add-to-list 'eshell-visual-commands "btop")

(provide 'peteches-eshell)
;;; peteches-eshell.el ends here
