;;; peteches-flymake.el --- Configure Flymake -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Lightweight diagnostics UI.  `peteches-lsp' configures lsp-mode to publish
;;; diagnostics through Flymake.

;;; Code:

(require 'flymake)

(setq flymake-show-diagnostics-at-end-of-line t)

(add-hook 'prog-mode-hook #'flymake-mode)

(define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)

(provide 'peteches-flymake)
;;; peteches-flymake.el ends here
