;;; gptel-min.el --- Minimal GPtel: Copilot Enterprise first -*- lexical-binding: t; -*-

;; Requirements (Guix packages): emacs, emacs-gptel
(require 'gptel)
(require 'gptel-gh)     ;; Copilot factory
(require 'gptel-curl)   ;; robust streaming


(push '(gptel) warning-suppress-types)

;; Use curl + streaming (works well with Copilot’s SSE responses)
(setq gptel-use-curl t
      gptel-stream   t
      gptel-default-mode #'org-mode)

;; --- Copilot Enterprise backend --------------------------------------------
(defvar peteches/gptel-copilot-enterprise
  (gptel-make-gh-copilot "Copilot (Enterprise)"
    :host "api.githubcopilot.com"
    :endpoint "/chat/completions"))

(defvar peteches/gptel-koboldcpp
  (gptel-make-openai "KoboldCPP"
    :protocol "http"	      ; change to "https" if you terminate TLS
    :host "nug.peteches.co.uk:5001"
    :endpoint "/v1/chat/completions"
    :stream t))

;; Make it the default backend.
(setq gptel-backend peteches/gptel-koboldcpp)

;; Define a keymap for gptel-mode
(define-prefix-command 'gptel-mode-prefix-map)

;; Bind gptel commands to specific keys in gptel-mode-prefix-map
(define-key gptel-mode-prefix-map (kbd "m") 'gptel-menu)
(define-key gptel-mode-prefix-map (kbd "s") 'gptel-send)
(define-key gptel-mode-prefix-map (kbd "g") 'gptel)
(define-key gptel-mode-prefix-map (kbd "r") 'gptel-rewrite)
(define-key gptel-mode-prefix-map (kbd "a") 'gptel-abort)

;; Bind C-c a g to the prefix map
(global-set-key (kbd "C-c a") 'gptel-mode-prefix-map)

(provide 'peteches-gptel)
;;; gptel-min.el ends here
