(provide 'peteches-golang)
(require 'peteches-lsp)

(message "loading golang config")

(add-hook 'go-mode-hook #'lsp)
