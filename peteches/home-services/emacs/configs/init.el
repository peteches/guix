;;; init --- Peteches emacs configs

;;; Commentary:
;;; top level initialisations

;;; Code:
(message "Loading base emacs config")
(add-to-list 'load-path "~/.config/emacs/lisp")

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


(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; this gets around several issues with tramp setting it to the path
;; inside the store that causes issues with remote connections over tramp
;; when `remote-path` isn't repected... I'm looking at you lsp-mode:
;; https://github.com/emacs-lsp/lsp-mode/issues/4371
(setq shell-file-name "/bin/sh")

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; package management, though prefer guix packages where possible
					;(load (locate-user-emacs-file "package-management.el"))

(load-theme 'modus-vivendi-tinted t)

(add-to-list 'default-frame-alist '(font . "Noto Sans Mono-14"))
(set-face-attribute 'default t :font "Noto Sans Mono-14")

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq vc-follow-symlinks t)
(global-visual-line-mode)
(require 'paredit)
(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)

(global-subword-mode)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'peteches-abbrev)
(require 'peteches-alert)
(require 'peteches-auth-sources)
(require 'peteches-bash)
(require 'peteches-combobulate)
(require 'peteches-company)
(require 'peteches-elisp)
(require 'peteches-eshell)
(require 'peteches-flymake)
(require 'peteches-emojify)
(require 'peteches-golang)
(require 'peteches-gptel)
(require 'peteches-guix)
(require 'peteches-isearch)
(require 'peteches-magit)
(require 'peteches-marginalia)
(require 'peteches-mcp)
(require 'peteches-orderless)
(require 'peteches-org)
(require 'peteches-proced)
(require 'peteches-projectile)
(require 'peteches-python)
(require 'peteches-scheme)
; (require 'peteches-slack)
(require 'peteches-tramp)
(require 'peteches-vertico)
(require 'peteches-which-key)
(require 'peteches-yasnippet)

(require 'peteches-gnus)
(peteches-go-activate)

(require 'peteches-scoreplay)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
