(provide 'peteches-lsp)
(message "Loading LSP Config")
(setq lsp-keymap-prefix "C-c l")
(require `lsp-mode)

;; Always format
(setq lsp-format-buffer-on-save t)


(add-hook 'lsp-before-initialize-hook
	  (lambda ()
	    (if (file-remote-p default-directory)
		(set (make-local-variable 'lsp-enable-file-watchers) nil))))

