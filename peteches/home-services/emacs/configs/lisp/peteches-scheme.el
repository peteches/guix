(provide 'peteches-scheme)
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
