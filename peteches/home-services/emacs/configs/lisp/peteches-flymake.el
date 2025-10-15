;; Generate a simple keymap for navigating errors with flymake


(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)


(provide 'peteches-flymake)
