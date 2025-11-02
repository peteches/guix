;;; peteches-yasnippet --- my yasnipptet setup

;;; Commentary:

;;; Code:
(require 'yasnippet)
(yas-global-mode 1)

(setq peteches/snippet-dir (expand-file-name "~/.config/emacs/snippets"))

(if (not (f-directory? peteches/snippet-dir))
    (magit-git "clone" "git@github.com:peteches/yasnippets.git" peteches/snippet-dir))

(defun peteches/snippit-git ()
    "Open magit status for the snippet dir."
    (interactive)
    (magit-status "~/.config/emacs/snippets"))

(provide `peteches-yasnippet)
;;; peteches-yasnippet.el ends here
