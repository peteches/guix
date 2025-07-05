(message "Loading base emacs config")
(add-to-list 'load-path "~/.config/emacs/lisp")

;; this gets around several issues with tramp setting it to the path
;; inside the store that causes issues with remote connections over tramp
;; when `remote-path` isn't repected... I'm looking at you lsp-mode:
;; https://github.com/emacs-lsp/lsp-mode/issues/4371
(setq shell-file-name "/bin/sh")

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; package management, though prefer guix packages where possible
					;(load (locate-user-emacs-file "package-management.el"))

(load-theme 'atom-one-dark t)

(add-to-list 'default-frame-alist '(font . "Noto Sans Mono-14"))
(set-face-attribute 'default t :font "Noto Sans Mono-14")

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq vc-follow-symlinks t)

;; ;; DIRED
;; (setq insert-directory-program (executable-find "eza")
;;       dired-dwim-target t
;;       dired-hide-details-hide-symlink-targets t
;;       directory-listing-before-filename-regexp ".*[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] \\([01][0-9]\\|2[0-3]\\):[0-5][0-9] +"
;;       dired-listing-switches "--classify --long --all --sort=Name --group-directories-first --time-style=long-iso --no-user")


(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)

(global-subword-mode)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; aidermacs
;; (load-file (locate-user-emacs-file "aidermacs.el"))

(require 'peteches-abbrev)
(require 'peteches-alert)
(require 'peteches-auth-sources)
(require 'peteches-eshell)
(require 'peteches-golang)
(require 'peteches-magit)
(require 'peteches-marginalia)
(require 'peteches-orderless)
(require 'peteches-pinentry)
(require 'peteches-projectile)
(require 'peteches-scheme)
(require 'peteches-slack)
(require 'peteches-tramp)
(require 'peteches-vertico)
(require 'peteches-which-key)
