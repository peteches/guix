;;; peteches-linear --- my linear integration
;;;
;;; Commentary:
;;; 
;;;
;;; Code:

(require 'linear-emacs)
(require 'peteches-first-frame)
(add-hook 'first-frame-ready-hook (lambda (_)
				   (setq
				    linear-emacs-api-key (auth-source-pass-get "api_key" "linear.app/peter.mccabe@scoreplay.io")
				    linear-emacs-default-team-id "TECHVP"
				    ;; Or use a different subdirectory in your org-directory
				    linear-emacs-org-file-path (expand-file-name "agenda/linear.org" org-directory))))

;; Automatically enable two-way sync when linear.org is opened
(defun peteches/enable-linear-org-sync ()
  "Enable Linear-org synchronization when linear.org is opened."
  (when (and buffer-file-name
             (string-match-p "linear\\.org$" buffer-file-name))
    (when (fboundp 'linear-emacs-enable-org-sync)
      (linear-emacs-enable-org-sync)
      (message "Linear-org synchronization enabled for this buffer"))))

;; Add hook to auto-enable sync when linear.org is opened
(add-hook 'find-file-hook #'peteches/enable-linear-org-sync)

;; Enable sync for org-after-todo-state-change-hook
(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match-p "linear\\.org$" buffer-file-name)
                       (fboundp 'linear-emacs-sync-org-to-linear))
              (linear-emacs-sync-org-to-linear))))

(provide 'peteches-linear)
;;; peteches-linear.el ends here
