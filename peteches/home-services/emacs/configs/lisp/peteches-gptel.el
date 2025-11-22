;;; peteches-gptel.el --- Minimal GPtel: Copilot Enterprise first -*- lexical-binding: t; -*-

;;; Commentary:
;; This Emacs Lisp file sets up a configuration for using GPtel, a package for interacting with various AI language models, primarily focusing on Copilot Enterprise and KoboldCPP.  The file configures the GPtel backend, defines a keymap for easy access to GPtel commands, and provides functions to switch between different backends.  The setup is designed to work with Emacs and is intended to be used with the Guix package manager for Emacs and its dependencies.


;;; Code:

(straight-use-package 'gptel)

;; Requirements (Guix packages): emacs, emacs-gptel
(require 'gptel)
(require 'gptel-gh)     ;; Copilot factory

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
    :host "nug.local:5001"
    :endpoint "/v1/chat/completions"
    :stream t))

;; Make it the default backend.
(setq gptel-backend peteches/gptel-koboldcpp
      peteches/gptel-backends '(peteches/gptel-koboldcpp
				peteches/gptel-copilot-enterprise))
(defun peteches/switch-gptel-backend (backend-symbol prefix)
  "Switch gptel BACKEND-SYMBOL.  With PREFIX, set it buffer-locally.

Select one of \"peteches/gptel-backends\" (a list of symbols whose values are
gptel backend objects)."
  (interactive
   (let* ((syms    peteches/gptel-backends)
          (names   (mapcar #'symbol-name syms))
          ;; Try to find which symbol currently corresponds to `gptel-backend`
          (current (car (seq-filter
                         (lambda (s) (and (boundp s) (eq (symbol-value s) gptel-backend)))
                         syms)))
          (picked  (completing-read "Select a backend: " names nil t
                                    nil nil (and current (symbol-name current)))))
     (list (intern picked) current-prefix-arg)))
  (unless (and (symbolp backend-symbol) (memq backend-symbol peteches/gptel-backends))
    (user-error "Not a configured backend: %S" backend-symbol))
  (unless (boundp backend-symbol)
    (user-error "Backend variable %S is not bound" backend-symbol))
  (let ((backend (symbol-value backend-symbol)))
    (if prefix
        (setq-local gptel-backend backend)
      (setq gptel-backend backend))
    (message "gptel backend → %s%s"
             (symbol-name backend-symbol)
	     (if (local-variable-p 'gptel-backend) " (buffer-local)" ""))))

(require 'peteches-gptel-commit)
(require 'peteches-gptel-presets)

;; Define a keymap for gptel-mode
(define-prefix-command 'gptel-mode-prefix-map)

;; Bind gptel commands to specific keys in gptel-mode-prefix-map
(define-key gptel-mode-prefix-map (kbd "m") 'gptel-menu)
(define-key gptel-mode-prefix-map (kbd "s") 'gptel-send)
(define-key gptel-mode-prefix-map (kbd "g") 'gptel)
(define-key gptel-mode-prefix-map (kbd "r") 'gptel-rewrite)
(define-key gptel-mode-prefix-map (kbd "a") 'gptel-add)
(define-key gptel-mode-prefix-map (kbd "A") 'gptel-abort)
(define-key gptel-mode-prefix-map (kbd "S") 'peteches/switch-gptel-backend)
(define-key gptel-mode-prefix-map (kbd "c") 'gptel-commit)

;; Bind C-c a g to the prefix map
(global-set-key (kbd "C-c a") 'gptel-mode-prefix-map)

(provide 'peteches-gptel)
;;; peteches-gptel.el ends here
