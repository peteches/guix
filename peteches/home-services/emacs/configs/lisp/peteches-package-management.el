;; peteches-package-management.el -- straight-only bootstrap

(defvar bootstrap-version)
(let* ((user-dir (or user-emacs-directory "~/.config/emacs/"))
       (bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-dir))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-always-ensure nil)

(provide 'peteches-package-management)
