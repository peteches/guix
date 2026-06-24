;;; peteches-lua --- my lua setup -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Author: Pete "Peteches" McCabe
;;; Maintainer: Pete "Peteches" McCabe
;;; Version: 0.1.0
;;; Package-Requires: ((Emacs "30.2"))
;;; Keywords: convenience
;;; URL: https://github.com/peteches/guix
;; Description: lots of configs are based in lua

;;; Code:

;; setup lsp stuff

(with-eval-after-load 'lsp-mode
  ;; Map Lua files to the LSP language id "lua".
  ;; lsp-mode may already do this, but keeping it here makes the custom
  ;; client self-contained.
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

