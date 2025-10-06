;;; peteches-mcp.el --- Graceful MCP hub + per-project filesystem  -*- lexical-binding: t; -*-

;;; ------------------------------------------------------------
;;; Install packages ONLY (config is below)
(use-package mcp
  :straight (mcp :host github :repo "lizqwerscott/mcp.el"))
(use-package gptel-mcp
  :straight (gptel-mcp :host github :repo "lizqwerscott/gptel-mcp.el"))

(message "Loading peteches-mcp")

;;; Config begins
(require 'mcp)
(require 'mcp-hub)
(require 'projectile nil t)   ;; optional
(require 'peteches-first-frame)
(require 'gptel-integrations)
;;;; ------------------------------------------------------------------
;;;; Utilities

(setq mcp-hub-servers
      '(("filesystem-primary-work" . (:command "mcp-filesystem-server" :args ("/home/peteches/area_51/github.com/ScorePlay-Inc/backend-monorepo.git/checkouts/primaryWork" )))
	("shell" . (:command "mcp-shell"))))

;; Start hub
(add-hook 'after-init-hook
	  #'mcp-hub-start-all-servers)

(provide 'peteches-mcp)
;;; peteches-mcp.el ends here
