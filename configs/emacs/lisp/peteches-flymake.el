;;; peteches-flymake --- configuring flymake
;;; Commentary:


;;; Code:
(require 'flymake)
(setq
 flymake-show-diagnostics-at-end-of-line t)

(add-hook 'prog-mode-hook 'flymake-mode)


(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)


(provide 'peteches-flymake)
;;; peteches-flymake.el ends here
