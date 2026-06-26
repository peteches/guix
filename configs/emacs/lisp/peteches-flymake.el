;;; peteches-flymake --- configuring flymake -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Flymake diagnostics should be visible, but not inserted visually at the
;;; end of the current line while typing.
;;;
;;; Code:

(require 'flymake)
(require 'eldoc)

(setq flymake-show-diagnostics-at-end-of-line nil)

(defun peteches/flymake-setup-eldoc ()
  "Prefer Flymake diagnostics in Eldoc's echo-area output."
  (eldoc-mode 1)
  (when (boundp 'eldoc-documentation-functions)
    (setq-local eldoc-documentation-functions
                (cons #'flymake-eldoc-function
                      (remove #'flymake-eldoc-function
                              eldoc-documentation-functions)))))

(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'flymake-mode-hook #'peteches/flymake-setup-eldoc)

(define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c ! b") #'flymake-show-buffer-diagnostics)
(define-key flymake-mode-map (kbd "C-c ! p") #'flymake-show-project-diagnostics)

(provide 'peteches-flymake)
;;; peteches-flymake.el ends here
