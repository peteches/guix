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
      `(("filesystem-main-work" . (:command "mcp-filesystem-server" :args ("/home/peteches/area_51/github.com/ScorePlay-Inc/backend-monorepo.git/checkouts/main" )))
	("filesystem-primary-work" . (:command "mcp-filesystem-server" :args ("/home/peteches/area_51/github.com/ScorePlay-Inc/backend-monorepo.git/checkouts/primaryWork" )))
	("filesystem-secondary-work" . (:command "mcp-filesystem-server" :args ("/home/peteches/area_51/github.com/ScorePlay-Inc/backend-monorepo.git/checkouts/secondaryWork" )))
	("shell" . (:command "mcp-shell"))
;; 	("linear" . (:command "docker" :args (
;; 					      "run"
;; ;					      "--rm"
;; 					      "--interactive"
;; 					      "--net=host"
;; 					      "--user" ,(number-to-string (user-uid))
;; 					      "--volume" ,(concat (expand-file-name "~/.mcp-remote-go-auth") ":/.mcp-remote-go-auth")
;; 					      "--publish" "3334:3334"
;; 					      "ghcr.io/naotama2002/mcp-remote-go:latest"
;; 					      "https://mcp.linear.app/sse")))
	))

;; Start hub
(add-hook 'after-init-hook
	  #'mcp-hub-start-all-server)

(provide 'peteches-mcp)
;;; peteches-mcp.el ends here
