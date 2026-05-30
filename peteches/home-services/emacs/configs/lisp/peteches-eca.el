;;; peteches-eca.el --- ECA (Editor Code Assistant) -*- lexical-binding: t; -*-
;;; Code:

(straight-use-package '(eca :host github :repo "editor-code-assistant/eca-emacs"))

(require 'eca)

(setq eca-chat-window-side 'right
      eca-chat-focus-on-open t)

(define-prefix-command 'eca-prefix-map)
(define-key eca-prefix-map (kbd "e") 'eca)
(define-key eca-prefix-map (kbd "t") 'eca-chat-toggle-window)
(define-key eca-prefix-map (kbd "n") 'eca-chat-new)
(define-key eca-prefix-map (kbd "c") 'eca-complete)
(define-key eca-prefix-map (kbd "r") 'eca-rewrite)
(global-set-key (kbd "C-c e") 'eca-prefix-map)

(provide 'peteches-eca)
;;; peteches-eca.el ends here
