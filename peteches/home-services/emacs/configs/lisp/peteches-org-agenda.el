;;; peteches/org-agenda.el --- Agenda setup -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)

(defgroup peteches/org-agenda nil
  "Org Agenda configuration."
  :group 'peteches/org)

(setq org-agenda-files
      (directory-files-recursively (expand-file-name "agenda" org-directory) "\\.org$"))

;; Subtle UI tweaks
(setq org-agenda-span 'week
      org-agenda-start-on-weekday 1  ;; Monday
      org-agenda-start-with-log-mode t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-use-time-grid t
      org-agenda-time-grid '((daily today require-timed)
                             (800 1000 1200 1400 1600 1800 2000)
                             "......" "----------------"))

;; Log state changes into a LOGBOOK drawer, keep agenda cleaner
(setq org-log-into-drawer t)

;; A focused dashboard and a couple of handy custom commands
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((todo "IN-PROGRESS"
                ((org-agenda-overriding-header "✅ Current tasks")
                 (org-agenda-sorting-strategy '(priority-down todo-state-down))))
          (todo "BLOCKED"
                ((org-agenda-overriding-header "⛔ Waiting on")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'scheduled))))
	  (todo "BACKLOG"
		((org-agenda-overriding-header "👀 Backlog")))))
        ("n" "Next only" todo "NEXT")
        ("w" "Work focus"
         ((tags-todo "+work+TODO={TODO\\|NEXT}")))
        ("i" "Inbox" tags "LEVEL=1+TODO={TODO}"
         ((org-agenda-files (list (expand-file-name "inbox.org" org-directory))))))
      )

;; Quick helper: refiling between common files
(setq org-refile-targets
      `(((,(expand-file-name "Tasks.org" org-directory)) :maxlevel . 3)
        ((,(expand-file-name "Scoreplay.org"    org-directory)) :maxlevel . 2)
        ((,(expand-file-name "calendar.org" org-directory)) :maxlevel . 2)))

(setq org-outline-path-complete-in-steps nil   ;; more natural minibuffer completion
      org-refile-use-outline-path 'file)

(require 'peteches-linear)
(provide 'peteches-org-agenda)
;;; peteches-org-agenda.el ends here
