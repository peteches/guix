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
    (setq default-directory (completing-read "Choose directory to cd: " subdirs 'stringp))))


(defun eshell/ucd ()
  "Make traversing to parent directories easier."
  (let* ((dir-components (split-string default-directory "/"))
	 (dirs (let ((dir '()) (path "/"))
		 (dolist (d dir-components)
		   (setq path (f-join path d))
		   (append dir path)))))
    (setq default-directory (completing-read "Choose parent to cd: " dirs))))

(add-to-list 'eshell-visual-commands "btop")
(provide 'peteches-eshell)
;;; peteches-eshell.el ends here
