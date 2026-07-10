;;; peteches-ghostel --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Author: Pete "Peteches" McCabe
;;; Maintainer: Pete "Peteches" McCabe
;;; Version: 0.1.0
;;; Package-Requires: ((Emacs "30.2"))
;;; Keywords: convenience
;;; URL: https://github.com/peteches/guix
;; Description: Description.

;;; Code:

(straight-use-package
 '(ghostel :type git :host github :repo "dakra/ghostel"))

(require 'ghostel)

;; Bindings and configuration for ghostel
(defun my/ghostel-send-C-k-and-kill ()
  "Send `C-k' to ghostel.
Like normal Emacs `C-k'. Kill to end of line and put content in kill-ring."
  (interactive)
  (kill-ring-save (point) (line-end-position))
  (ghostel-send-key "k" "ctrl"))

(define-prefix-command 'peteches-ghostel)
;; Global bindings
(global-set-key (kbd "C-x m") 'peteches-ghostel)

(define-key peteches-ghostel (kbd "m") 'ghostel)
(define-key peteches-ghostel (kbd "r") 'ghostel-force-redraw)
(define-key peteches-ghostel (kbd "M") 'ghostel-project)
(define-key peteches-ghostel (kbd "o") 'ghostel-other)
(define-key peteches-ghostel (kbd "n") 'ghostel-next)
(define-key peteches-ghostel (kbd "p") 'ghostel-previous)
(define-key peteches-ghostel (kbd "N") 'ghostel-project-next)
(define-key peteches-ghostel (kbd "P") 'ghostel-project-previous)
(define-key peteches-ghostel (kbd "c") 'ghostel-clear)
(define-key peteches-ghostel (kbd "C") 'ghostel-compile)


;; ghostel-semi-char-mode-map bindings
(define-key ghostel-semi-char-mode-map (kbd "C-s") 'consult-line)
(define-key ghostel-semi-char-mode-map (kbd "C-k") 'my/ghostel-send-C-k-and-kill)
(define-key ghostel-semi-char-mode-map (kbd "M-p") (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
(define-key ghostel-semi-char-mode-map (kbd "M-n") (lambda () (interactive) (ghostel-send-key "n" "ctrl")))

;; project-prefix-map bindings
(define-key project-prefix-map (kbd "m") 'ghostel-project)
(define-key project-prefix-map (kbd "M") 'ghostel-project-list-buffers)

;; Add to project switch commands
(add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
(add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)

;; Add to ghostel-eval-cmds
(add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))
  
(require 'ghostel-compile)
(ghostel-compile-global-mode 1)
(provide 'peteches-ghostel)
;;; peteches-ghostel.el ends here
