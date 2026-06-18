;;; peteches-projectile --- projectile configs
;;;
;;; Commentary:
;;;
;;;
;;; Code:

(require 'projectile)

(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
(global-set-key (kbd "C-c p") 'projectile-command-map)

(setq projectile-per-project-compilation-buffer t)

(setq projectile-indexing-method 'alien)   ;; use external tool
(setq projectile-enable-caching t)

(dolist (dir '("vendor" "node_modules" "dist" "bin" "bazel-bin" "bazel-out" "bazel-testlogs"))
  (add-to-list 'projectile-globally-ignored-directories dir))

(when (executable-find "fd")
  ;; Make Projectile’s fd backend behave like `find`:
  ;;  -t f : files only
  ;;  -L   : follow symlinks
  ;;  -uu  : ignore ALL ignore files (VCS + parent/global)
  ;;  -0   : NUL-separated (what Projectile expects)
  ;;  -E .git : skip .git noise
  ;;  --strip-cwd-prefix : paths relative to project root
  ;;  -c never : no colors
  (setq projectile-git-fd-args "-t f -L -uu -0 -E .git --strip-cwd-prefix -c never"))

(unless (executable-find "fd")
  (setq projectile-generic-command "find . -type f -print0"))

(defun peteches/projectile-switch-project-menu ()
  "Offer a small menu of actions after switching to a Projectile project."
  (let* ((actions
          '(("VC status"          . projectile-vc)
            ("Find file"          . projectile-find-file)
            ("Switch to buffer"   . projectile-switch-to-buffer)
            ("Recent file"        . projectile-recentf)
            ("Dired"              . projectile-dired)
            ("Eshell"             . projectile-run-eshell)
            ("Compile project"    . projectile-compile-project)
            ("Test project"       . projectile-test-project)))
         (choice (completing-read
                  (format "In %s → " (projectile-project-name))
                  (mapcar #'car actions) nil t)))
    (funcall (cdr (assoc choice actions)))))

(setq projectile-switch-project-action #'peteches/projectile-switch-project-menu)


(defun peteches/projectile-copy-relative-file-name (&optional with-line)
  "Copy this buffer's file name relative to the Projectile project root.

With prefix argument WITH-LINE (\\[universal-argument]), also append \":LINE\".

If the current buffer isn't visiting a file, fall back to `default-directory`."
  (interactive "P")
  (let* ((root (projectile-project-root))
         (path (or buffer-file-name default-directory))
         (rel  (file-relative-name path root))
         (out  (if with-line
                   (format "%s:%d" rel (line-number-at-pos))
                 rel)))
    (kill-new out)
    (message "Copied: %s" out)))


(with-eval-after-load 'projectile
  (define-key projectile-command-map (kbd "w")
    #'peteches/projectile-copy-relative-file-name))


(provide 'peteches-projectile)
;;; peteches-projectile.el ends here
