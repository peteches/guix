;;; Install tools to ~/.local/bin using Yarn v1 if available, else npm.
(require 'cl-lib)

(defconst peteches/npm-local-prefix (expand-file-name "~/.local"))
(defconst peteches/npm-local-bin    (expand-file-name "bin" peteches/npm-local-prefix))
(defconst peteches/npm-local-cache  (expand-file-name "npm-cache" user-emacs-directory))
(defconst peteches/yarn-global-folder (expand-file-name "~/.local/share/yarn-global"))

;; Ensure Emacs finds ~/.local/bin
(make-directory peteches/npm-local-bin t)
(unless (member peteches/npm-local-bin exec-path)
  (add-to-list 'exec-path peteches/npm-local-bin))
(setenv "PATH" (concat peteches/npm-local-bin path-separator (getenv "PATH")))

(defun peteches/yarn-classic-p ()
  "Return non-nil if `yarn` exists and is classic v1."
  (let ((yver (and (executable-find "yarn")
                   (string-trim (with-temp-buffer
                                  (ignore-errors
                                    (call-process "yarn" nil t nil "--version"))
                                  (buffer-string))))))
    (and yver (string-match-p "^1\\." yver))))

(defun peteches/npm-install-to-local (pkg)
  "Install PKG so its executables end up in ~/.local/bin.
Uses Yarn classic (v1) if present; otherwise uses npm with a per-run prefix."
  (interactive "sPackage (npm/yarn name): ")
  (if (peteches/yarn-classic-p)
      ;; Yarn v1 path: install, then link all yarn global bins into ~/.local/bin
      (let* ((pkgq (shell-quote-argument pkg))
             (yglob (shell-quote-argument peteches/yarn-global-folder))
             (bin   (shell-quote-argument peteches/npm-local-bin))
             (cmd (mapconcat #'identity
                             (list
                              (format "export YARN_GLOBAL_FOLDER=%s" yglob)
                              (format "yarn global add %s" pkgq)
                              "YBIN=$(yarn global bin)"
                              (format "mkdir -p %s" bin)
                              ;; link/update all yarn-global binaries into ~/.local/bin
                              "for f in \"$YBIN\"/*; do [ -f \"$f\" ] && ln -sf \"$f\" ~/.local/bin/; done"
                              "echo '[mcp] yarn install complete; binaries linked into ~/.local/bin'")
                             " && ")))
        (async-shell-command cmd))
    ;; npm path: per-run prefix so bins land in ~/.local/bin
    (let ((process-environment (cl-copy-list process-environment)))
      (setenv "npm_config_prefix" peteches/npm-local-prefix)
      (setenv "npm_config_cache"  peteches/npm-local-cache)
      (async-shell-command
       (format "npm install -g --prefix %s %s"
               (shell-quote-argument peteches/npm-local-prefix)
               (shell-quote-argument pkg))))))


(provide 'peteches-npm)
