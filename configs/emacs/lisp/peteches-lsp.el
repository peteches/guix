;;; peteches-lsp.el --- Shared LSP configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; Central LSP policy for this Emacs configuration.
;;;
;;; Guix should provide language server binaries.  This module configures
;;; `lsp-mode', finds servers on `exec-path', and gives language modules a
;;; small helper for starting LSP only when the relevant server is already
;;; available.  It deliberately avoids `lsp-install-server' and other
;;; auto-install paths.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(message "Loading LSP Config")

(defgroup peteches-lsp nil
  "Shared LSP configuration."
  :group 'tools
  :prefix "peteches-lsp-")

(defcustom peteches-lsp-auto-start t
  "If non-nil, language modules may start LSP automatically."
  :type 'boolean
  :group 'peteches-lsp)

(defcustom peteches-lsp-enable-remote nil
  "If non-nil, allow automatic LSP startup in remote buffers."
  :type 'boolean
  :group 'peteches-lsp)

(defcustom peteches-lsp-format-on-save nil
  "If non-nil, `peteches/lsp-enable-format-on-save' formats before saving.

This is intentionally disabled by default.  Language modules should opt in when
LSP formatting is the preferred formatter for that language."
  :type 'boolean
  :group 'peteches-lsp)

(defcustom peteches-lsp-server-programs
  '((bash . "bash-language-server")
    (go . "gopls")
    (lua . "lua-language-server")
    (python . ("basedpyright-langserver" "pyright-langserver" "pylsp"))
    (scheme . "guile-lsp-server")
    (sql . "sqls")
    (yaml . "yaml-language-server"))
  "Language server executables to look for on `exec-path'.

Entries are intentionally executable names, not install instructions.  Install
or remove the corresponding Guix packages to control availability."
  :type '(alist :key-type symbol :value-type (choice string (repeat string)))
  :group 'peteches-lsp)

(defun peteches/lsp--program-candidates (language)
  "Return executable candidates configured for LANGUAGE."
  (let ((programs (alist-get language peteches-lsp-server-programs)))
    (cond
     ((stringp programs) (list programs))
     ((listp programs) programs)
     (t nil))))

(defun peteches/lsp-server-executable (language)
  "Return the first executable found for LANGUAGE, or nil."
  (cl-loop for program in (peteches/lsp--program-candidates language)
           for executable = (executable-find program)
           when executable return executable))

(defun peteches/lsp-server-available-p (language)
  "Return non-nil when a language server for LANGUAGE is available."
  (and (peteches/lsp-server-executable language) t))

(defun peteches/lsp-server-command (language)
  "Return an argv list for LANGUAGE's language server.

Signal a user error when LANGUAGE has no available server.  This is useful as a
lazy command provider for `lsp-stdio-connection'."
  (let ((executable (peteches/lsp-server-executable language)))
    (unless executable
      (user-error "No %s language server found on exec-path; install it with Guix" language))
    (list executable)))

(defun peteches/lsp--remote-buffer-p ()
  "Return non-nil when the current buffer is visiting a remote location."
  (or (file-remote-p default-directory)
      (when-let ((file (buffer-file-name)))
        (file-remote-p file))))

(defun peteches/lsp-disable-file-watchers-for-remote ()
  "Disable LSP file watchers for remote buffers."
  (when (peteches/lsp--remote-buffer-p)
    (setq-local lsp-enable-file-watchers nil)))

(defun peteches/lsp-enable-ui-modes ()
  "Enable optional LSP UI minor modes when they are available."
  (dolist (mode '(lsp-modeline-workspace-status-mode
                  lsp-modeline-code-actions-mode
                  lsp-modeline-diagnostics-mode))
    (when (fboundp mode)
      (funcall mode 1))))

(defun peteches/lsp-format-buffer-on-save ()
  "Format the current buffer through LSP before saving, when supported."
  (when (and peteches-lsp-format-on-save
             (bound-and-true-p lsp-mode)
             (fboundp 'lsp-feature?)
             (lsp-feature? "textDocument/formatting"))
    (lsp-format-buffer)))

(defun peteches/lsp-enable-format-on-save ()
  "Enable the shared LSP format-on-save helper in the current buffer."
  (setq-local peteches-lsp-format-on-save t)
  (add-hook 'before-save-hook #'peteches/lsp-format-buffer-on-save nil t))

(defun peteches/lsp-maybe-start (language)
  "Start LSP for LANGUAGE in the current buffer when its server is available.

Return non-nil when startup was requested.  Missing servers are silent during
hook-based startup and reported when called interactively."
  (interactive
   (list
    (intern
     (completing-read
      "Start LSP for language: "
      (mapcar (lambda (entry) (symbol-name (car entry)))
              peteches-lsp-server-programs)
      nil t))))
  (cond
   ((not peteches-lsp-auto-start)
    (when (called-interactively-p 'interactive)
      (message "LSP auto-start is disabled"))
    nil)
   ((and (peteches/lsp--remote-buffer-p)
         (not peteches-lsp-enable-remote))
    (when (called-interactively-p 'interactive)
      (message "LSP auto-start is disabled for remote buffers"))
    nil)
   ((not (peteches/lsp-server-available-p language))
    (when (called-interactively-p 'interactive)
      (message "No %s language server found on exec-path" language))
    nil)
   ((not (require 'lsp-mode nil t))
    (when (called-interactively-p 'interactive)
      (message "lsp-mode is not available"))
    nil)
   ((fboundp 'lsp-deferred)
    (lsp-deferred)
    t)
   ((fboundp 'lsp)
    (lsp)
    t)))

(defun peteches/lsp-status ()
  "Show LSP status for the current buffer and configured server executables."
  (interactive)
  (let ((source-buffer (current-buffer))
        (buf (get-buffer-create "*peteches-lsp-status*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Buffer: %s\n" (buffer-name source-buffer)))
      (insert (format "Major mode: %s\n"
                      (buffer-local-value 'major-mode source-buffer)))
      (insert (format "Default directory: %s\n"
                      (buffer-local-value 'default-directory source-buffer)))
      (insert (format "Remote: %s\n"
                      (if (with-current-buffer source-buffer
                            (peteches/lsp--remote-buffer-p))
                          "yes"
                        "no")))
      (insert (format "lsp-mode active: %s\n"
                      (if (and (boundp 'lsp-mode)
                               (buffer-local-value 'lsp-mode source-buffer))
                          "yes"
                        "no")))
      (insert (format "Diagnostics provider: %s\n"
                      (if (boundp 'lsp-diagnostics-provider)
                          lsp-diagnostics-provider
                        "unknown")))
      (insert (format "Completion provider: %s\n\n"
                      (if (boundp 'lsp-completion-provider)
                          lsp-completion-provider
                        "unknown")))
      (insert (format "%-12s %-9s %s\n" "Language" "Status" "Executable"))
      (insert (make-string 72 ?-) "\n")
      (dolist (entry peteches-lsp-server-programs)
        (let* ((language (car entry))
               (exe (peteches/lsp-server-executable language)))
          (insert (format "%-12s %-9s %s\n"
                          language
                          (if exe "found" "missing")
                          (or exe (mapconcat #'identity
                                             (peteches/lsp--program-candidates language)
                                             ", "))))))
      (goto-char (point-min))
      (view-mode 1))
    (display-buffer buf)))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook ((lsp-before-initialize . peteches/lsp-disable-file-watchers-for-remote)
         (lsp-mode . peteches/lsp-enable-ui-modes))
  :init
  (setq lsp-keymap-prefix "C-c l"
        ;; Performance.
        read-process-output-max (* 1024 1024 16)
        gc-cons-threshold (* 100 1024 1024)
        ;; Completion and diagnostics integrate with the existing Company/CAPF
        ;; and Flymake setup.
        lsp-completion-provider :capf
        lsp-diagnostics-provider :flymake
        ;; Language servers are installed by Guix, not by lsp-mode.
        lsp-enable-suggest-server-download nil)
  :config
  (setq lsp-response-timeout 180
        lsp-idle-delay 0.3
        lsp-file-watch-threshold 3000)

  ;; Ensure optional UI bits are loaded when installed, but do not require them.
  (dolist (feat '(lsp-modeline lsp-lens lsp-headerline))
    (require feat nil t)))

(provide 'peteches-lsp)
;;; peteches-lsp.el ends here
