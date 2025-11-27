;;; peteches/org.el --- Core Org defaults, loads agenda/roam/babel -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; --- Install org-roam and org-modern via straight.el ---
(straight-use-package
 '(org-modern :type git :host github :repo "minad/org-modern"))
(straight-use-package
 '(org-roam :type git :host github :repo "org-roam/org-roam"))

(straight-use-package
 '(verb :type git :host github :repo "federicotdn/verb"))

;; --- Core Org preferences (no external deps) -------------------------------
(require 'org)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(defgroup peteches/org nil
  "Opinionated, minimal Org setup."
  :group 'org)

(defcustom peteches/org-directory (expand-file-name "~/area_51/org")
  "Base directory for Org files."
  :type 'directory :group 'peteches/org)


;; Reasonable visual defaults
(setq
 olivetti-body-width 0.8
 org-M-RET-may-split-line '((default . nil))
 org-agenda-files (list (concat org-directory "/agenda/"))
 org-attach-store-link-p 'file
 org-directory peteches/org-directory
 org-ellipsis "…"
 org-hide-emphasis-markers t
 org-image-actual-width '(500)
 org-insert-heading-respect-content t
 org-log-done 'time
 org-log-into-draw t
 org-pretty-entities t
 org-startup-folded 'content
 org-startup-indented t
 org-startup-with-inline-images t
 org-table-allow-automatic-line-recalculation t
 org-tags-column 0
 org-use-sub-superscripts "{}"
 )

;; Streamlined TODO flow
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))


(defvar peteches-org-map nil "Top-level Org prefix map.")
(unless (keymapp peteches-org-map)
  (setq peteches-org-map (make-sparse-keymap)))
(define-key global-map (kbd "C-c O") peteches-org-map)

;; Main Org actions under C-c o
(define-key peteches-org-map (kbd "a") #'org-agenda)
(define-key peteches-org-map (kbd "c") #'org-capture)
(define-key peteches-org-map (kbd "l") #'org-store-link)
(define-key peteches-org-map (kbd "t") #'org-todo)         ;; context-aware when on a heading
(define-key peteches-org-map (kbd "s") #'org-schedule)     ;; handy helper
(define-key peteches-org-map (kbd "d") #'org-deadline)

;; (Optional) which-key labels, only if available
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements
    peteches-org-map
    "a" "agenda"
    "c" "capture"
    "l" "store link"
    "t" "todo"
    "s" "schedule"
    "d" "deadline"
    "r" "org-roam …"))  ;; "r" will be the embedded roam map


;; Fast capture targets (lightweight, tweak to taste)
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
(setq org-capture-templates
      `(("e" "Emacs Task" entry
	 (file+headline ,(expand-file-name "agenda/Tasks.org" org-directory) "Emacs Stuff")
	 "* TODO %?  %^g\n")
	("g" "Guix Task" entry
	 (file+headline ,(expand-file-name "agenda/Tasks.org" org-directory) "Guix Stuff")
	 "* TODO %?  %^g\n")
	("t" "Tasks" entry
	 (file+headline ,(expand-file-name "agenda/Tasks.org" org-directory) "Tasks")
	 "* TODO %? %^g\n")))

;; Make file: links relative inside org-directory

(setq org-link-file-path-type 'relative)

      ;; --- Load the rest of the stack -------------------------------------------

;; --- Fonts ---
;; Set these once to whatever fonts you use:
(let ((variable-font  "Noto Sans")
      (fixed-font "Noto Sans Mono"))
  (set-face-attribute 'variable-pitch nil :family variable-font)
  (set-face-attribute 'fixed-pitch nil :family fixed-font)
  (set-face-attribute 'org-table nil :family fixed-font)
  (set-face-attribute 'org-code nil :family fixed-font)
  (set-face-attribute 'org-block nil :family fixed-font))

;; --- Enable org-modern, olivetti and variable pitch for Org buffers ---
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'olivetti-mode)

;; Optional: width of the writing column



(require 'peteches-org-agenda) ;; agenda and custom commands
(require 'peteches-org-babel)  ;; babel languages & safety
(require 'peteches-org-roam)   ;; roam (kept last; it hooks into capture, etc.)

(require 'peteches-scoreplay)
(provide 'peteches-org)
;;; peteches-org.el ends here
