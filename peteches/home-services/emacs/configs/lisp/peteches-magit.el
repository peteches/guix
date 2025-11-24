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




(provide 'peteches-magit)
;;; peteches-magit.el ends here
