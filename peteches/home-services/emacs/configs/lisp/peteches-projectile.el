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

(provide 'peteches-projectile)
