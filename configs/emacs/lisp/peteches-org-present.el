;;; peteches-org-present --- for presentations
;;;
;;; Commentary:
;;;
;;;
;;; Code:


;; Hide only mermaid src blocks during org-present
(defvar my/org-present--mermaid-invis-symbol 'my-org-present-mermaid)

(defun my/org-present--hide-mermaid-blocks ()
  (add-to-invisibility-spec my/org-present--mermaid-invis-symbol)
  (org-with-wide-buffer
    (org-element-map (org-element-parse-buffer) 'src-block
      (lambda (src)
        (when (string= (org-element-property :language src) "mermaid")
          (let ((beg (org-element-property :begin src))
                (end (org-element-property :end src)))
            (org-flag-region beg end t my/org-present--mermaid-invis-symbol)))))))

(defun my/org-present--show-mermaid-blocks ()
  (org-with-wide-buffer
    (org-flag-region (point-min) (point-max) nil my/org-present--mermaid-invis-symbol))
  (remove-from-invisibility-spec my/org-present--mermaid-invis-symbol))

(defun my/org-present--start ()
  ;; show/refresh images, then hide mermaid code
  (org-display-inline-images)
  (org-redisplay-inline-images)
  (my/org-present--hide-mermaid-blocks))

(defun my/org-present--end ()
  ;; restore code visibility
  (my/org-present--show-mermaid-blocks)
  (org-redisplay-inline-images))

(with-eval-after-load 'org-present
  (add-hook 'org-present-mode-hook #'my/org-present--start)
  (add-hook 'org-present-mode-quit-hook #'my/org-present--end)
  ;; refresh inline images when moving between slides
  (add-hook 'org-present-after-navigate-functions
            (lambda (&rest _) (org-redisplay-inline-images))))



(provide 'peteches-org-present)
;;; peteches-org-present.el ends here
