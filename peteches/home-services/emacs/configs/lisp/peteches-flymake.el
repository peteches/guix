;;; peteches-flymake --- configuring flymake
;;; Commentary:

;;; Code:
(setq
 flymake-show-diagnostics-at-end-of-line t)
(flymake-mode)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)


(provide 'peteches-flymake)
;;; peteches-flymake.el ends here
