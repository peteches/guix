;;; peteches/org-babel.el --- Org Babel languages & safety -*- lexical-binding: t; -*-

;;; Code:

(require 'org)

;; Load languages conditionally (no hard deps; only activate if available).
;; Add/remove to taste. All guarded with (require ... nil t).
(dolist (pair '((emacs-lisp . ob-emacs-lisp)
                (shell      . ob-shell)
                (python     . ob-python)
                (awk        . ob-awk)
                (sed        . ob-sed)
                (sql        . ob-sql)
                (sqlite     . ob-sqlite)
                (js         . ob-js)
                (go         . ob-go)     ;; requires ob-go installed
                (dot        . ob-dot)))
  (let* ((lang (car pair))
         (feat (cdr pair)))
    (when (require feat nil t)
      (add-to-list 'org-babel-load-languages (cons lang t)))))

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; Only skip confirmation for a whitelist of "safe" langs
(defcustom peteches/org-babel-no-confirm '(emacs-lisp shell)
  "Languages that can execute without confirmation."
  :type '(repeat symbol))

(defun peteches/org-babel-confirm-evaluate (lang body)
  "Return nil to skip confirmation for LANG if whitelisted; t otherwise."
  (not (memq (intern lang) peteches/org-babel-no-confirm)))

(setq org-confirm-babel-evaluate #'peteches/org-babel-confirm-evaluate)

;; Sensible defaults for results
(setq org-babel-default-header-args
      (let ((args (copy-alist org-babel-default-header-args)))
        (setf (alist-get :results args) "output replace")
        (setf (alist-get :noweb   args) "no")
        args))

;; Keep code blocks tidy
(setq org-src-preserve-indentation t
      org-edit-src-content-indentation 0
      org-src-window-setup 'current-window)

(provide 'peteches-org-babel)
;;; peteches/org-babel.el ends here
