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

(provide 'peteches-projectile)
