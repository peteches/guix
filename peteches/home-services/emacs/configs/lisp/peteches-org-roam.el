;;; peteches/org-roam.el --- Org-roam basics -*- lexical-binding: t; -*-

;;; Code:

(defvar peteches-org-map)

(defvar peteches-org-roam-map nil
  "Prefix map for Org-roam")
(unless (keymapp peteches-org-roam-map)
  (setq peteches-org-roam-map (make-sparse-keymap)))

(with-eval-after-load 'peteches-org
  (define-key peteches-org-map (kbd "r") peteches-org-roam-map))

(autoload 'org-roam-node-find         "org-roam"          nil t)
(autoload 'org-roam-node-insert       "org-roam"          nil t)
(autoload 'org-roam-tag-add           "org-roam"          nil t)

(require 'org-roam)
(require 'org-roam-dailies)

(message "loading org-roam config")

(defgroup peteches/org-roam nil
  "Org-roam configuration."
  :group 'peteches/org)

(defcustom peteches/org-roam-directory
  (expand-file-name "roam" org-directory)
  "Directory for Org-roam notes."
  :type 'directory :group 'peteches/org-roam)

(setq org-roam-directory peteches/org-roam-directory)

(unless (file-directory-p peteches/org-roam-directory)
  (make-directory peteches/org-roam-directory t))

; "Ensure `org-roam-directory' and its subfolders exist."
(unless (file-directory-p peteches/org-roam-directory)
  (make-directory peteches/org-roam-directory t))
(let ((daily-dir (expand-file-name "daily" peteches/org-roam-directory)))
  (unless (file-directory-p daily-dir)
    (make-directory daily-dir t)))

;; Lightweight DB sync; no extra deps
(org-roam-db-autosync-enable)

;; Simple capture templates that play nicely with the rest of the setup
(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n#+date: %U\n\n")
         :immediate-finish nil
         :unnarrowed t)
        ("l" "literature" plain
         "* %^{Title}\n:PROPERTIES:\n:Source: %^{Source}\n:END:\n\n%?"
         :if-new (file+head "lit/${slug}.org"
                            "#+title: ${title}\n#+filetags: :literature:\n#+date: %U\n\n")
         :immediate-finish nil
         :unnarrowed t)))

(define-prefix-command 'peteches-org-roam-map)

(define-key peteches-org-roam-map (kbd "f") #'org-roam-node-find)
(define-key peteches-org-roam-map (kbd "i") #'org-roam-node-insert)
(define-key peteches-org-roam-map (kbd "t") #'org-roam-tag-add)

;; When the full dailies map is present, expose it at "d"
(with-eval-after-load "org-roam-dailies"
  (when (boundp 'org-roam-dailies-map)
    (define-key peteches-org-roam-map (kbd "d") org-roam-dailies-map)))

;; (optional) which-key labels
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements
    peteches-org-roam-map
    "f" "find node" "i" "insert node" "t" "tag add"
    "d" "dailies …"))  

;; Dailies (optional but zero-cost to wire up)
(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%H:%M> %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n\n"))))

(provide 'peteches-org-roam)
;;; peteches/org-roam.el ends here
