;;; peteches-sly --- My sly configs
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(use-package sly
  :straight (sly :type git :host github :repo "joaotavora/sly")

  :config
  (setq inferior-lisp-program "sbcl"))


(provide 'peteches-sly)
;;; peteches-sly.el ends here
