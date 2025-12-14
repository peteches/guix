;;; peteches-magit --- My Magit config

;;; Commentary:
;;;
;;; This setups up magit and Forge
;;;
;;; Code:

(require 'magit)
(require 'forge)
(message "loading Magit config")
;; Install code-review from GitHub (maintained fork)
(straight-use-package
 '(code-review
   :type git
   :host github
   :repo "phelrine/code-review"
   :files ("*.el" "graphql" "LICENSE" "package.json" "npm-shrinkwrap.json")
   :build (:not compile)))

;; Optional: basic configuration without use-package
(with-eval-after-load 'forge
  ;; Bind a key inside forge-topic-mode (i.e. inside a PR buffer)
  (define-key forge-topic-mode-map (kbd "C-c r") #'code-review-forge-pr-at-point)

  ;; Configure storage/log paths (both optional)
  (setq code-review-log-file
        (expand-file-name "code-review.log" user-emacs-directory))
  (setq code-review-db-database-file
        (expand-file-name "code-review-db.sqlite" user-emacs-directory)))

(require 'cl-lib)
(require 'magit)

(require 'cl-lib)
(require 'magit)
(require 'projectile)

(defun peteches/magit-init-bare-with-main-worktree (path)
  "Create a bare repo at PATH, suffixing .git if needed.
Add a main worktree at PATH/checkouts/main, make an initial empty commit,
then open Magit in that worktree."
  (interactive (list (read-directory-name "Bare repo location: "
					  (expand-file-name "~/area_51/"))))

  (let* ((repo-path (directory-file-name (expand-file-name path)))
         (bare-path (if (string-suffix-p ".git" repo-path)
                        repo-path
                      (concat repo-path ".git")))
         (bare-dir  (file-name-as-directory bare-path))
         (worktree  (expand-file-name "checkouts/main" bare-dir))
         (parent    (file-name-directory (directory-file-name bare-dir))))

    (when (file-exists-p bare-dir)
      (user-error "Destination already exists: %s" bare-dir))

    (let ((default-directory parent))
      (magit-call-git "init" "--bare" bare-dir))

    (let ((default-directory bare-path))
      (magit-call-git "worktree" "add" "-b" "main" worktree))

    (let ((default-directory worktree))
      (magit-call-git "commit" "--allow-empty" "--no-gpg-sign" "--no-verify" "-m" "Initial Commit"))

    (projectile-add-known-project worktree)

    (magit-status worktree)))

(provide 'peteches-magit)
;;; peteches-magit.el ends here
