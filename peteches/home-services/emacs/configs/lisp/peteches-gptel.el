;;; gptel-min.el --- Minimal GPtel: Copilot Enterprise first -*- lexical-binding: t; -*-

;; Requirements (Guix packages): emacs, emacs-gptel
(require 'gptel)
(require 'gptel-gh)     ;; Copilot factory
(require 'gptel-curl)   ;; robust streaming

;; Use curl + streaming (works well with Copilot’s SSE responses)
(setq gptel-use-curl t
      gptel-stream   t
      gptel-default-mode #'org-mode)

;; --- Copilot Enterprise backend --------------------------------------------
(defvar my/gptel-copilot-enterprise
  (gptel-make-gh-copilot "Copilot (Enterprise)"
    :host "api.githubcopilot.com"
    :endpoint "/chat/completions"))

;; Make it the default backend.
(setq gptel-backend my/gptel-copilot-enterprise)

;; Handy command to pop a chat buffer (pick model per-buffer via M-x gptel-menu).
(defun peteches/copilot-chat ()
  "Open a GPtel chat using Copilot Enterprise."
  (interactive)
  (let ((gptel-backend my/gptel-copilot-enterprise))
    ;; Don’t pin a model globally; choose it per buffer with `gptel-menu`.
    (kill-local-variable 'gptel-model)
    (call-interactively #'gptel)))


(provide 'peteches-gptel)
;;; gptel-min.el ends here
