(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (require 'tsmark-backend-elisp)
	    (tsmark-mode 1)))

(provide 'peteches-elisp)
