;; early-init.el
(setq package-enable-at-startup nil
      package-quickstart nil)     ;; stop package.el auto-loading/caching
;; (then bootstrap straight)

;; --- Guix Home Git + CA in Emacs (no cl-lib needed) ------------------------
(let* ((sep path-separator)
       (profile (cond
                 ((file-directory-p (expand-file-name "~/.guix-home/profile"))
                  (expand-file-name "~/.guix-home/profile"))
                 ((file-directory-p (expand-file-name "~/.guix-profile"))
                  (expand-file-name "~/.guix-profile"))
                 (t nil)))
       (bin  (and profile (expand-file-name "bin" profile)))
       (sbin (and profile (expand-file-name "sbin" profile)))
       (certs-dir  (and profile (expand-file-name "etc/ssl/certs" profile)))
       (certs-file (and certs-dir (expand-file-name "ca-certificates.crt" certs-dir))))

  (let* ((env-path-contains-p
          (lambda (dir)
            (let ((cur (or (getenv "PATH") "")))
              (and (not (string= cur ""))
                   (member dir (split-string cur (regexp-quote sep) t))))))
         (append-to-env-path
          (lambda (dir)
            (when (and dir (file-directory-p dir)
                       (not (funcall env-path-contains-p dir)))
              (setenv "PATH" (if (getenv "PATH")
                                 (concat (getenv "PATH") sep dir)
                               dir)))))
         (append-to-exec-path
          (lambda (dir)
            (when (and dir (file-directory-p dir) (not (member dir exec-path)))
              (add-to-list 'exec-path dir t)))))

    ;; Prefer Guix Home bin/sbin for subprocesses
    (funcall append-to-env-path bin)
    (funcall append-to-env-path sbin)
    (funcall append-to-exec-path bin)
    (funcall append-to-exec-path sbin)

    ;; Ensure Git helpers (git-remote-https) are discoverable
    (let* ((gx (ignore-errors (car (process-lines "git" "--exec-path"))))
           (gx (cond
                ((and gx (file-directory-p gx)) gx)
                ((and profile (file-directory-p (expand-file-name "libexec/git-core" profile)))
                 (expand-file-name "libexec/git-core" profile))
                (t nil))))
      (when gx
        (unless (equal (getenv "GIT_EXEC_PATH") gx)
          (setenv "GIT_EXEC_PATH" gx))
        (funcall append-to-env-path gx)
        (funcall append-to-exec-path gx)))

    ;; Point Git/libcurl to the CA bundle if unset/invalid (don’t clobber)
    (let ((cafile-ok  (and (getenv "GIT_SSL_CAINFO")
                           (file-readable-p (getenv "GIT_SSL_CAINFO"))))
          (capath-ok  (and (getenv "GIT_SSL_CAPATH")
                           (file-directory-p (getenv "GIT_SSL_CAPATH")))))
      (unless cafile-ok
        (when (and certs-file (file-readable-p certs-file))
          (setenv "GIT_SSL_CAINFO" certs-file)
          (unless (getenv "CURL_CA_BUNDLE")
            (setenv "CURL_CA_BUNDLE" certs-file))
          (unless (getenv "SSL_CERT_FILE")
            (setenv "SSL_CERT_FILE" certs-file))))
      (unless capath-ok
        (when (and certs-dir (file-directory-p certs-dir))
          (setenv "GIT_SSL_CAPATH" certs-dir)
          (unless (getenv "SSL_CERT_DIR")
            (setenv "SSL_CERT_DIR" certs-dir)))))))
;; ---------------------------------------------------------------------------

