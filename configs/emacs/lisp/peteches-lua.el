;;; peteches-lua.el --- Lua configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Lua LSP and mode selection.  Grammar discovery and manual grammar install
;;; commands live in `peteches-treesit'.
;;;
;;; Code:

(require 'peteches-treesit)
(require 'peteches-lsp nil t)

(defun peteches/lua-mode ()
  "Open the current buffer with the best available Lua major mode."
  (interactive)
  (cond
   ((peteches/treesit-mode-ready-p 'lua-ts-mode 'lua)
    (lua-ts-mode))
   ((fboundp 'lua-mode)
    (lua-mode))
   (t
    (prog-mode)
    (message "No Lua major mode is available"))))

(defun peteches/lua-start-lsp ()
  "Start LSP in Lua buffers when `lsp-mode' is available."
  (when (fboundp 'lsp)
    (lsp)))

(add-to-list 'auto-mode-alist '("\\.lua\\'" . peteches/lua-mode))
(dolist (hook '(lua-mode-hook lua-ts-mode-hook))
  (add-hook hook #'peteches/lua-start-lsp))

(with-eval-after-load 'lsp-mode
  ;; Map Lua files to the LSP language id "lua".  lsp-mode may already do this,
  ;; but keeping it here makes the custom client self-contained.
  (add-to-list 'lsp-language-id-configuration
               '("\\.lua\\'" . "lua"))

  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection
     '("/home/peteches/.guix-home/profile/bin/lua-language-server"))
    :activation-fn
    (lsp-activate-on "lua")
    ;; Higher than the built-in lua-language-server priority you saw: -2.
    :priority 1
    ;; Custom server id, avoiding collision with lsp-mode's built-in one.
    :server-id 'peteches/lua-language-server)))

(provide 'peteches-lua)
;;; peteches-lua.el ends here
