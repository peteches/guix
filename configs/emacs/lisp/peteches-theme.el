;;; peteches-theme.el --- Matugen theme loading -*- lexical-binding: t; -*-

;;; Commentary:
;; Load the generated matugen theme when it exists, with a Modus fallback.
;; The generated theme lives under ~/.cache/matugen/emacs so Guix Home can
;; manage the static template while matugen/DMS can overwrite the output.

;;; Code:

(defgroup peteches-theme nil
  "Theme loading for the Peteches Emacs configuration."
  :group 'faces)

(defcustom peteches/matugen-theme-directory
  (expand-file-name "~/.cache/matugen/emacs")
  "Directory containing the generated matugen Emacs theme."
  :type 'directory
  :group 'peteches-theme)

(defcustom peteches/matugen-theme-file
  (expand-file-name "matugen-theme.el" peteches/matugen-theme-directory)
  "Generated matugen Emacs theme file."
  :type 'file
  :group 'peteches-theme)

(defcustom peteches/theme-fallback 'modus-vivendi-tinted
  "Theme to load when the generated matugen theme is unavailable."
  :type 'symbol
  :group 'peteches-theme)

(defvar peteches/matugen-theme-watch nil
  "File-notify watch descriptor for the matugen theme directory.")

(defvar peteches/matugen-theme-reload-timer nil
  "Debounce timer used when the generated matugen theme changes.")

(defun peteches/disable-enabled-themes ()
  "Disable all currently enabled custom themes."
  (mapc #'disable-theme custom-enabled-themes))

(defun peteches/load-matugen-theme (&optional quiet)
  "Load the generated matugen theme, falling back when it is absent.

When QUIET is non-nil, suppress status messages."
  (interactive)
  (add-to-list 'custom-theme-load-path peteches/matugen-theme-directory)
  (put 'matugen 'theme-settings nil)
  (put 'matugen 'theme-feature nil)
  (peteches/disable-enabled-themes)
  (condition-case err
      (if (file-readable-p peteches/matugen-theme-file)
          (progn
            ;; `load-file' re-reads the generated file every time, which makes
            ;; wallpaper/theme changes visible in a long-running daemon.
            (load-file peteches/matugen-theme-file)
            (enable-theme 'matugen)
            (unless quiet
              (message "Loaded generated matugen theme")))
        (load-theme peteches/theme-fallback t)
        (unless quiet
          (message "Matugen theme missing; loaded %s"
                   peteches/theme-fallback)))
    (error
     (load-theme peteches/theme-fallback t)
     (unless quiet
       (message "Could not load matugen theme (%s); loaded %s"
                (error-message-string err)
                peteches/theme-fallback)))))

(defun peteches/reload-matugen-theme ()
  "Reload the generated matugen theme."
  (interactive)
  (peteches/load-matugen-theme))

(defun peteches--schedule-matugen-theme-reload ()
  "Reload the matugen theme shortly after a file-notify event."
  (when (timerp peteches/matugen-theme-reload-timer)
    (cancel-timer peteches/matugen-theme-reload-timer))
  (setq peteches/matugen-theme-reload-timer
        (run-at-time
         1 nil
         (lambda ()
           (setq peteches/matugen-theme-reload-timer nil)
           (when (file-readable-p peteches/matugen-theme-file)
             (peteches/load-matugen-theme t))))))

(defun peteches--matugen-theme-file-event-p (event)
  "Return non-nil when file-notify EVENT is for `peteches/matugen-theme-file'."
  (let ((file (nth 2 event)))
    (and file
         (string= (expand-file-name file)
                  (expand-file-name peteches/matugen-theme-file)))))

(defun peteches--matugen-theme-watch-callback (event)
  "Handle file-notify EVENT for generated matugen themes."
  (when (peteches--matugen-theme-file-event-p event)
    (peteches--schedule-matugen-theme-reload)))

(defun peteches/watch-matugen-theme ()
  "Watch the generated matugen theme and reload Emacs after changes."
  (interactive)
  (when (and (fboundp 'file-notify-add-watch)
             (file-directory-p peteches/matugen-theme-directory)
             (not peteches/matugen-theme-watch))
    (condition-case err
        (setq peteches/matugen-theme-watch
              (file-notify-add-watch
               peteches/matugen-theme-directory
               '(change)
               #'peteches--matugen-theme-watch-callback))
      (error
       (message "Could not watch matugen theme directory: %s"
                (error-message-string err))))))

(make-directory peteches/matugen-theme-directory t)
(peteches/load-matugen-theme t)
(peteches/watch-matugen-theme)

(provide 'peteches-theme)
;;; peteches-theme.el ends here
