(require 'peteches-lsp)

(message "loading scheme config")

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/area_51/guix"))

;; setup lsp stuff

(add-hook 'scheme-mode-hook #'lsp)

(add-to-list 'lsp-language-id-configuration '(".*\\.scm$" . "scheme"))

(lsp-register-client (make-lsp-client
                      :new-connection (lsp-stdio-connection "guile-lsp-server")
                      :activation-fn (lsp-activate-on "scheme")
                      :server-id 'guile-lsp-server))


(defun peteches-guile-install-treesit-grammers ()
  "Install/update the Scheme (Guile) Tree-sitter grammar."
  (interactive)
  (unless (fboundp 'treesit-available-p)
    (user-error "This Emacs was not built with tree-sitter support"))
  (unless (treesit-available-p)
    (user-error "Tree-sitter isn’t available in this Emacs build"))
  ;; Guile uses Scheme grammar
  (unless (assoc 'scheme treesit-language-source-alist)
    (add-to-list 'treesit-language-source-alist
                 '(scheme "https://github.com/6cdh/tree-sitter-scheme")))
  (condition-case err
      (progn
        (treesit-install-language-grammar 'scheme)
        (message "Scheme grammar installed. Use `scheme-ts-mode`.")) 
    (error (user-error "Failed to install Scheme grammar: %s"
                       (error-message-string err)))))

(defun peteches--ts-scheme-missing-p () (not (treesit-language-available-p 'scheme)))


(add-hook 'after-init-hook
          (lambda ()
            (when (and (fboundp 'treesit-available-p)
                       (treesit-available-p)
                       (peteches--ts-scheme-missing-p))
              (run-with-idle-timer 2 nil #'peteches-guile-install-treesit-grammers))))


(provide 'peteches-scheme)
