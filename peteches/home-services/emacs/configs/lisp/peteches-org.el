
;;; peteches/org.el --- Core Org defaults, loads agenda/roam/babel -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; --- Install org-roam and org-modern via straight.el ---
(straight-use-package
 '(org-modern :type git :host github :repo "minad/org-modern"))
(straight-use-package
 '(org-roam :type git :host github :repo "org-roam/org-roam"))

;; --- Core Org preferences (no external deps) -------------------------------
(require 'org)

(defgroup peteches/org nil
  "Opinionated, minimal Org setup."
  :group 'org)

(defcustom peteches/org-directory (expand-file-name "~/area_51/org")
  "Base directory for Org files."
  :type 'directory :group 'peteches/org)


;; Reasonable visual defaults
(setq
 org-attach-store-link-p 'file
 org-startup-indented t
 org-pretty-entities t
 org-use-sub-superscripts "{}"
 org-hide-emphasis-markers t
 org-startup-with-inline-images t
 org-image-actual-width '(300)
 org-log-done 'time
 org-log-into-draw t
 org-directory peteches/org-directory
 org-agenda-files (list (concat org-directory "/agenda/"))
 org-startup-folded 'content
 org-ellipsis "…"
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-image-actual-width '(500)
 org-M-RET-may-split-line '((default . nil))
 org-insert-heading-respect-content t
 org-tags-column 0
 org-table-allow-automatic-line-recalculation t
 olivetti-body-width 0.8
 )

;; Streamlined TODO flow
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))


(defvar peteches-org-map nil "Top-level Org prefix map.")
(unless (keymapp peteches-org-map)
  (setq peteches-org-map (make-sparse-keymap)))
(define-key global-map (kbd "C-c o") peteches-org-map)

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
	 "* TODO %?  %^g\n")))

;; Make file: links relative inside org-directory

(setq org-link-file-path-type 'relative)

      ;; --- Load the rest of the stack -------------------------------------------

;; --- Fonts ---
;; Set these once to whatever fonts you use:
(set-face-attribute 'variable-pitch nil :family "Noto Sans")
(set-face-attribute 'fixed-pitch nil :family "Noto Sans Mono-14")
(set-face-attribute 'org-table nil :family "Noto Sans Mono-14")

;; --- Enable org-modern, olivetti and variable pitch for Org buffers ---
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook #'olivetti-mode)
(add-hook 'org-mode-hook #'variable-pitch-mode)

;; Optional: width of the writing column



(require 'peteches-org-agenda) ;; agenda and custom commands
(require 'peteches-org-babel)  ;; babel languages & safety
(require 'peteches-org-roam)   ;; roam (kept last; it hooks into capture, etc.)

(require 'peteches-scoreplay)
(provide 'peteches-org)
;;; peteches-org.el ends here
