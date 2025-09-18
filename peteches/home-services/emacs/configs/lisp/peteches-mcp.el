;;; peteches-mcp.el --- Graceful MCP hub + per-project filesystem  -*- lexical-binding: t; -*-

;;; ------------------------------------------------------------
;;; Install packages ONLY (config is below)
(use-package mcp
  :straight (mcp :host github :repo "lizqwerscott/mcp.el"))
(use-package gptel-mcp
  :straight (gptel-mcp :host github :repo "lizqwerscott/gptel-mcp.el"))

;;; Config begins
(require 'mcp)
(require 'mcp-hub)
(require 'subr-x)
(require 'projectile nil t)   ;; optional
(require 'peteches-first-frame)

;;;; ------------------------------------------------------------------
;;;; Utilities

(defgroup peteches-mcp nil
  "Personal MCP setup."
  :group 'tools)

(defcustom peteches-mcp-default-root (expand-file-name "~")
  "Fallback filesystem root when no project root can be determined."
  :type 'directory)

(defun peteches-mcp--project-root ()
  "Return Projectile root if available, else current buffer dir, else fallback."
  (or (when (featurep 'projectile)
        (ignore-errors (projectile-project-root)))
      (when-let ((d (or (and (buffer-file-name) (file-name-directory (buffer-file-name)))
                        default-directory)))
        (expand-file-name d))
      peteches-mcp-default-root))

(defun peteches-mcp--file-uri (path)
  (concat "file://" (directory-file-name (expand-file-name path))))

(defun peteches-mcp--find-exe (&rest candidates)
  "Return first found executable among CANDIDATES, or nil."
  (seq-some #'executable-find candidates))

(defun peteches-mcp--github-pat ()
  "GitHub PAT from pass (entry \"github.com/peteches\", field \"emacs_pat_token\").
Falls back to env; returns nil if none."
  (or (and (featurep 'auth-source-pass)
           (ignore-errors (auth-source-pass-get "emacs_pat_token" "github.com/peteches")))
      (getenv "GITHUB_PERSONAL_ACCESS_TOKEN")))

;;;; ------------------------------------------------------------------
;;;; Global request dispatcher: answer roots/list (prevents timeouts)

(defun mcp-request-dispatcher (name method params)
  "Reply to important MCP requests. Returns a result or nil.
NAME is the connection name; METHOD a string/symbol; PARAMS a plist."
  (ignore params)
  (let ((m (if (symbolp method) (symbol-name method) method)))
    (pcase m
      ("roots/list"
       (let* ((root (peteches-mcp--project-root))
              (label (format "%s:%s"
                             (or name "filesystem")
                             (file-name-nondirectory (directory-file-name root)))))
         ;; Return a proper list of roots; vconcat OK (server expects array)
         (list :roots (vconcat (list (list :uri (peteches-mcp--file-uri root)
                                           :name label))))))
      (_
       ;; Be quiet: just return nil (jsonrpc -> \"result\": null)
       nil))))

(defun peteches-mcp--notify-roots-changed ()
  "Best-effort notify filesystem servers that roots may have changed."
  (when (boundp 'mcp-server-connections)
    (maphash
     (lambda (nm conn)
       (when (and (stringp nm) (string-prefix-p "filesystem:" nm))
         (ignore-errors
           (mcp-notify conn :notifications/roots/list_changed))))
     mcp-server-connections)))

;;;; ------------------------------------------------------------------
;;;; Global (non-filesystem) servers — discovered opportunistically

(defun peteches-mcp--github-server ()
  "Alist for GitHub MCP (stdio binary) if available; prefers PAT from pass/env."
  (let ((cmd (peteches-mcp--find-exe "github-mcp-server"))
        (pat (peteches-mcp--github-pat)))
    (when cmd
      `(:command ,cmd :args ("stdio")
        :env ,(when pat `(:GITHUB_PERSONAL_ACCESS_TOKEN ,pat))))))

(defun peteches-mcp--linear-server ()
  "Alist for Linear via mcp-remote (OAuth in browser) if available."
  (when (peteches-mcp--find-exe "mcp-remote")
    '(:command "mcp-remote" :args ("https://mcp.linear.app/mcp" "--transport" "http-only"))))

(defun peteches-mcp--notion-server ()
  "Alist for Notion via mcp-remote (OAuth in browser) if available."
  (when (peteches-mcp--find-exe "mcp-remote")
    '(:command "mcp-remote" :args ("https://mcp.notion.com/mcp" "--transport" "http-only"))))

(defun peteches-mcp--emacs-server ()
  "Alist for a local Emacs MCP server binary if present."
  (when (peteches-mcp--find-exe "emacs-mcp-server")
    '(:command "emacs-mcp-server" :args ())))

(defun peteches-mcp--global-servers ()
  "Build a filtered (NAME . CONFIG) list for hub; skip any missing."
  (delq nil
        (list (when-let ((x (peteches-mcp--github-server))) (cons "github" x))
              (when-let ((x (peteches-mcp--linear-server))) (cons "linear" x))
              (when-let ((x (peteches-mcp--notion-server))) (cons "notion" x))
              (when-let ((x (peteches-mcp--emacs-server)))  (cons "emacs"  x)))))

(defun peteches-mcp-start-global ()
  "Start all discoverable global servers; skip silently if none.
Never throws: errors are caught and logged."
  (let* ((computed (peteches-mcp--global-servers))
         (merged   (append computed mcp-hub-servers)))
    (if (null merged)
        (message "[mcp] No global MCP servers available; skipping hub start.")
      (setq mcp-hub-servers merged)
      (condition-case err
          (mcp-hub-start-all-server)
        (error (message "[mcp] Hub start failed (non-fatal): %s"
                        (error-message-string err)))))))

;;;; ------------------------------------------------------------------
;;;; Per-project filesystem server (Projectile only, graceful if missing)

(defun peteches-mcp--fs-connection-name (root)
  (let* ((base (file-name-nondirectory (directory-file-name root)))
         (hash (substring (md5 (expand-file-name root)) 0 8)))
    (format "filesystem:%s:%s" (if (string-empty-p base) "~" base) hash)))

(defun peteches-mcp--ensure-fs-for (root)
  "Ensure a filesystem server is running for ROOT; no error if binary missing."
  (let ((cmd (peteches-mcp--find-exe "mcp-server-filesystem")))
    (if (not cmd)
        (message "[mcp] mcp-server-filesystem not found in PATH; skipping filesystem server for %s" root)
      (let* ((name (peteches-mcp--fs-connection-name root))
             (already (gethash name mcp-server-connections)))
        (unless already
          (let ((process-connection-type nil)) ;; ensure PIPE, no PTY banners
            (condition-case err
                (mcp-connect-server
                 name :command cmd :args (list (expand-file-name root))
                 :initial-callback (lambda (_c)
                                     (message "[mcp] %s connected." name)))
              (error
               (message "[mcp] Failed to start filesystem server (%s): %s"
                        name (error-message-string err))))))))))

(defun peteches-mcp-start-filesystem-for-current-project ()
  "Attempt to start a per-project filesystem server (never errors)."
  (let ((root (peteches-mcp--project-root)))
    (when root
      (peteches-mcp--ensure-fs-for root)
      (peteches-mcp--notify-roots-changed))))

;; Hooks: safe, no-op if Projectile missing or binaries absent
(when (featurep 'projectile)
  (add-hook 'projectile-after-switch-project-hook
            #'peteches-mcp-start-filesystem-for-current-project))
(add-hook 'find-file-hook #'peteches-mcp-start-filesystem-for-current-project)

;; Optionally start global servers after init; harmless if none found
(add-hook 'first-frame-ready-hook (lambda (_) (peteches-mcp-start-global)))

(provide 'peteches-mcp)
;;; peteches-mcp.el ends here
