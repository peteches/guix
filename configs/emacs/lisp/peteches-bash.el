;; Shell (bash)
;;; Install Bash Tree-sitter grammar
;;; Requires Emacs 29+

(defun peteches-bash-install-treesit-grammers ()
  "Install (or update) the Bash Tree-sitter grammar for Emacs.

Adds the Bash source to `treesit-language-source-alist` if missing,
then compiles and installs the grammar."
  (interactive)
  (unless (fboundp 'treesit-available-p)
    (user-error "This Emacs was not built with tree-sitter support"))
  (unless (treesit-available-p)
    (user-error "Tree-sitter isn’t available in this Emacs build"))

  ;; Ensure the Bash grammar source is known.
  (let ((src '(bash "https://github.com/tree-sitter/tree-sitter-bash")))
    (unless (assoc 'bash treesit-language-source-alist)
      (add-to-list 'treesit-language-source-alist src)))

  ;; Install or update the grammar.
  (condition-case err
      (progn
        (treesit-install-language-grammar 'bash)
        (message "Bash Tree-sitter grammar installed. Try `bash-ts-mode`."))
    (error
     (user-error "Failed to install Bash grammar: %s" (error-message-string err)))))

(defun peteches--bash-grammar-missing-p ()
  (not (treesit-language-available-p 'bash)))

(defun peteches--maybe-bash-ts-mode ()
  "Use `bash-ts-mode` in Bash buffers when grammar is available."
  (when (and (derived-mode-p 'sh-mode)
             (boundp 'sh-shell)
             (eq sh-shell 'bash)
             (fboundp 'treesit-available-p)
             (treesit-available-p)
             (treesit-language-available-p 'bash))
    (bash-ts-mode)))

(add-hook 'sh-mode-hook #'peteches--maybe-bash-ts-mode)

(add-hook 'after-init-hook
          (lambda ()
            (when (and (fboundp 'treesit-available-p)
                       (treesit-available-p)
                       (peteches--bash-grammar-missing-p))
              ;; defer so it won’t slow startup
              (run-with-idle-timer
               2 nil #'peteches-bash-install-treesit-grammers))))


(provide 'peteches-bash)
