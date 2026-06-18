;; Python
(add-hook 'python-ts-mode-hook
	  (lambda ()
	    (require 'tsmark-backend-python)
	    (tsmark-mode 1)))

(defun peteches-python-install-treesit-grammers ()
  "Install/update the Python Tree-sitter grammar."
  (interactive)
  (unless (fboundp 'treesit-available-p)
    (user-error "This Emacs was not built with tree-sitter support"))
  (unless (treesit-available-p)
    (user-error "Tree-sitter isn’t available in this Emacs build"))
  (unless (assoc 'python treesit-language-source-alist)
    (add-to-list 'treesit-language-source-alist
                 '(python "https://github.com/tree-sitter/tree-sitter-python")))
  (condition-case err
      (progn
        (treesit-install-language-grammar 'python)
        (message "Python grammar installed. Use `python-ts-mode`.")) 
    (error (user-error "Failed to install Python grammar: %s"
                       (error-message-string err)))))

(defun peteches--ts-python-missing-p () (not (treesit-language-available-p 'python)))

(when (and (fboundp 'treesit-available-p)
           (treesit-available-p)
           (treesit-language-available-p 'python))
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

(add-hook 'after-init-hook
          (lambda ()
            (when (and (fboundp 'treesit-available-p)
                       (treesit-available-p)
                       (peteches--ts-python-missing-p))
              (run-with-idle-timer 2 nil #'peteches-python-install-treesit-grammers))))


(provide 'peteches-python)
