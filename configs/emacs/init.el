;;; init --- Peteches emacs configs

;;; Commentary:
;;; top level initialisations

;;; Code:
(message "Loading base emacs config")
(add-to-list 'load-path "~/.config/emacs/lisp")

;;; backups-and-auto-save.el --- Keep Emacs backup files out of project dirs -*- lexical-binding: t; -*-

(defvar peteches/emacs-state-directory
  (expand-file-name "emacs/"
                    (or (getenv "XDG_STATE_HOME")
                        (expand-file-name "~/.local/state/")))
  "Directory for persistent Emacs state files.")

(defvar peteches/backup-directory
  (expand-file-name "backups/" peteches/emacs-state-directory)
  "Directory for Emacs backup files.")

(defvar peteches/auto-save-directory
  (expand-file-name "auto-save/" peteches/emacs-state-directory)
  "Directory for Emacs auto-save files.")

;; Ensure the directories exist before Emacs tries to write into them.
(dolist (dir (list peteches/backup-directory
                   peteches/auto-save-directory))
  (make-directory dir t))

;; Move `file~' backups into `peteches/backup-directory'.
;;
;; Because the destination is an absolute directory, Emacs uniquifies backup
;; names using the original absolute file name, replacing directory separators
;; with `!'. This avoids clashes between files with the same basename.
;;
;; Example:
;;
;;   /home/peteches/foo/config.scm
;;   /home/peteches/bar/config.scm
;;
;; become distinct backup names in the central backup directory.
(setq backup-directory-alist
      `(("." . ,peteches/backup-directory)))

;; Move `#file#' auto-save files into `peteches/auto-save-directory'.
;;
;; The final `t' enables Emacs' built-in uniquification, so auto-save files are
;; also based on the original absolute path rather than only the basename.
(setq auto-save-file-name-transforms
      `((".*" ,peteches/auto-save-directory t)))

;; Keep Emacs' auto-save session list files with the auto-save files too.
;;
;; These are not the `#file#' files themselves, but they are used by recovery
;; commands such as `recover-session'.
(setq auto-save-list-file-prefix
      (expand-file-name ".saves-" peteches/auto-save-directory))

(let ((guix-elisp-dir "~/.guix-home/profile/share/emacs/site-lisp/"))
  (when (file-directory-p guix-elisp-dir)
    (dolist (dir (directory-files guix-elisp-dir t "\\`[^.]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

(require 'peteches-pinentry)
(require 'peteches-package-management)
(require 'peteches-first-frame)

(dolist (dir '("~/.guix-home/profile/bin"
               "~/.guix-home/profile/sbin"
	       "~/.local/bin"
               "~/.cargo/bin" ;; for Rust users, for example
               "/usr/local/bin"))
  (let ((expanded (expand-file-name dir)))
    (when (file-directory-p expanded)
      (setenv "PATH" (concat expanded ":" (getenv "PATH")))
      (add-to-list 'exec-path expanded))))


(require 'peteches-treesit)


;; this gets around several issues with tramp setting it to the path
;; inside the store that causes issues with remote connections over tramp
;; when `remote-path` isn't repected... I'm looking at you lsp-mode:
;; https://github.com/emacs-lsp/lsp-mode/issues/4371
(setq shell-file-name "/bin/sh")

(require 'peteches-lsp)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; package management, though prefer guix packages where possible
					;(load (locate-user-emacs-file "package-management.el"))

(require 'peteches-theme)

(add-to-list 'default-frame-alist '(font . "Noto Sans Mono-14"))
(set-face-attribute 'default t :font "Noto Sans Mono-14")

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq vc-follow-symlinks t)
(global-visual-line-mode)
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)

(global-subword-mode)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'peteches-abbrev)
(require 'peteches-alert)
(require 'peteches-anvil)
(require 'peteches-auth-sources)
(require 'peteches-bash)
(require 'peteches-combobulate)
(require 'peteches-company)
(require 'peteches-dired)
(require 'peteches-eca)
(require 'peteches-elisp)
(require 'peteches-emojify)
(require 'peteches-eshell)
(require 'peteches-flymake)
(require 'peteches-ghostel)
(require 'peteches-golang)
(require 'peteches-gptel)
(require 'peteches-guix)
(require 'peteches-isearch)
(require 'peteches-lua)
(require 'peteches-magit)
(require 'peteches-marginalia)
(require 'peteches-mcp)
(require 'peteches-orderless)
(require 'peteches-org)
(require 'peteches-proced)
(require 'peteches-projectile)
(require 'peteches-python)
(require 'peteches-scheme)
(require 'peteches-sops)
(require 'peteches-sql)
(require 'peteches-tramp)
(require 'peteches-treesit-fold)
(require 'peteches-ui-toys)
(require 'peteches-vertico)
(require 'peteches-which-key)
(require 'peteches-yasnippet)
; (require 'peteches-slack)

(require 'peteches-gnus)
(peteches-go-activate)

(require 'peteches-scoreplay)

(setopt register-alist-save-flag t)
(setopt register-separator-storage-file
        (expand-file-name "registers.el" peteches/org-directory))

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
