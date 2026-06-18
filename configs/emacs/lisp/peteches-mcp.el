;;; peteches-mcp.el --- Graceful MCP hub + per-project filesystem  -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;;
;;; Code:

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

;; Start hub

;; (add-hook 'after-init-hook
;; 	  #'mcp-hub-start-all-server)

(provide 'peteches-mcp)
;;; peteches-mcp.el ends here
