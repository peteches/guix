(require 'cl-lib)
(require 'pinentry)

(message "Loading pinentry config")
(setenv "INSIDE_EMACS" (format "t,%s" emacs-version))
(setenv "PINENTRY_USER_DATA" "USE_EMACS=1")
(setq epg-pinentry-mode 'nil)
(pinentry-start)


;; Remote Magit/Git commit signing runs GPG on the remote host.  Do not
;; ask the remote gpg-agent to use pinentry-emacs: the remote process cannot
;; reach this Emacs' pinentry socket over TRAMP.  Instead mark those processes
;; so the Guix Home Git GPG wrapper uses GnuPG loopback pinentry.
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-process-environment
               "PINENTRY_USER_DATA=MAGIT_TRAMP=1"))

(getenv "INSIDE_EMACS")

(defun peteches--normalized-inside-emacs (&optional keep-suffix)
  "Return 'INSIDE_EMACS' that starts with 't,<version>'.
If KEEP-SUFFIX is non-nil, preserve any ,term:… or ,vterm:… suffix
already present in the current environment."
  (let* ((ver (format "t,%s" emacs-version))
         (cur (getenv "INSIDE_EMACS"))
         (suffix (and keep-suffix cur
                      (when (string-match "\\(,\\(term\\|vterm\\):[^,]+\\)" cur)
                        (match-string 1 cur)))))
    (concat ver (or suffix ""))))

;; Make it global for all future subprocesses spawned by Emacs.
(let ((val (peteches--normalized-inside-emacs t)))
  (setenv "INSIDE_EMACS" val)
  ;; Replace any existing entry in process-environment
  (setq process-environment
        (cons (concat "INSIDE_EMACS=" val)
              (cl-remove-if (lambda (s) (string-prefix-p "INSIDE_EMACS=" s))
                            process-environment))))
(with-eval-after-load 'term
  (defun peteches--term-inside-emacs-advice (orig &rest args)
    (let* ((val (peteches--normalized-inside-emacs t))
           (process-environment
            (cons (concat "INSIDE_EMACS=" val)
                  (cl-remove-if (lambda (s) (string-prefix-p "INSIDE_EMACS=" s))
                                process-environment))))
      (apply orig args)))
  (advice-add 'term-exec-1 :around #'peteches--term-inside-emacs-advice))

(provide 'peteches-pinentry)
