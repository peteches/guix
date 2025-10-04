;;; Guix: refresh load-path from active profiles, on demand.
(require 'seq)

(defun guix--add-site-lisp-from (profile)
  "Add PROFILE's Emacs site-lisp (and its subdirs) to `load-path`."
  (when (and profile (not (string-empty-p profile)))
    (let* ((p (substitute-in-file-name profile)) ; expand ~ and $GUIX_PROFILE
           (dir (expand-file-name "share/emacs/site-lisp" p)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)
        ;; Also add package subdirectories (common in Guix builds).
        (let ((default-directory dir))
          (when (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path)))))))

(defun guix-refresh-emacs-load-path (&optional profiles)
  "Refresh `load-path` from Guix profiles.
If PROFILES is nil, use a sensible default set."
  (interactive)
  (let* ((defaults (list (getenv "GUIX_PROFILE")      ; current shell’s profile, if any
                         "~/.guix-profile"
                         "~/.guix-home/profile"
                         "/run/current-system/profile"))
         (candidates (or profiles defaults)))
    (dolist (p (delete-dups (seq-filter #'identity candidates)))
      (guix--add-site-lisp-from p))
    (message "Guix load-path refreshed. Use M-x load-library or (require '…) to load a package.")))

(defun guix-load-package (library)
  "Convenience wrapper to `require` a LIBRARY by name (string)."
  (interactive "sLibrary to load (e.g. vertico, magit): ")
  (condition-case err
      (progn (require (intern library)) (message "Loaded %s" library))
    (error (user-error "Could not load %s: %s" library (error-message-string err)))))

(provide 'peteches-guix)
