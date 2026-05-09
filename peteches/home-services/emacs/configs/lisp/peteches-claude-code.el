;;; peteches-claude-code.el --- claude-code.el integration -*- lexical-binding: t; -*-

;;; Code:

(straight-use-package '(claude-code :host github :repo "stevemolitor/claude-code.el"))

(require 'claude-code)

(setq claude-code-program "claude-emacs-wrapper")

(defun peteches/claude-code ()
  "Open Claude Code in a vterm, rooted at the Projectile project root."
  (interactive)
  (let ((default-directory
         (or (ignore-errors
               (and (fboundp 'projectile-project-root)
                    (projectile-project-root)))
             default-directory)))
    (claude-code)))

(global-set-key (kbd "C-c a C") #'peteches/claude-code)

(provide 'peteches-claude-code)
;;; peteches-claude-code.el ends here
