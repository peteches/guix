;;; peteches-yasnippet --- my yasnipptet setup

;;; Commentary:

;;; Code:

(use-package f)

(use-package yasnippet
  :straight (yasnippet :type git :host github :repo "joaotavora/yasnippet")

  :after (f)

  ;; evaluate after yasnippet loaded
  :config
  (yas-global-mode 1)

  (setq peteches/snippet-dir (expand-file-name "~/.config/emacs/snippets"))

  (if (not (f-directory? (file-name-concat peteches/snippet-dir ".git")))
      (magit-git "clone" "git@github.com:peteches/yasnippets.git" peteches/snippet-dir))

  (defun peteches/snippit-git ()
    "Open magit status for the snippet dir."
    (interactive)
    (magit-status "~/.config/emacs/snippets")))

(use-package yasnippet-snippets
  :straight (yasnippet-snippets :type git :host github :repo "AndreaCrotti/yasnippet-snippets"))

(provide `peteches-yasnippet)
;;; peteches-yasnippet.el ends here
