;; peteches/home-services/git.scm
(define-module (peteches home-services git)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services configuration)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (home-git-configuration
            git-section git-section-name git-section-config))

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(define (serialize-string field-name value) value)

(define (key-value? x)
  (and (string? (car x)) (string? (cdr x))))

(define (list-of-key-values? lst)
  ((list-of key-value?) lst))

(define (serialize-list-of-key-values field-name values)
  (string-join
   (map (lambda (x) (string-append (car x) " = " (cdr x) "\n")) values) ""))

(define-configuration git-section
  (name   (string "")              "Name of the git config section")
  (config (list-of-key-values '()) "list of key value pairs to populate the config"))

(define (list-of-git-sections? sections)
  ((list-of git-section?) sections))

(define (serialize-git-section section)
  (string-append "[" (git-section-name section) "]\n"
                 (serialize-list-of-key-values "c" (git-section-config section))
                 "\n"))

;; Ensure a single [core] with hooksPath and excludesFile, without mutating input.
(define (serialize-list-of-git-sections fieldname sections)
  (let* ((core?   (lambda (s) (string=? "core" (git-section-name s))))
         (core-in (find core? sections))
         (rest    (remove core? sections))
         (cfg0    (if core-in (git-section-config core-in) '()))
         (has     (lambda (k cfg) (any (lambda (p) (string-ci=? k (car p))) cfg)))
         (ensure  (lambda (k v cfg) (if (has k cfg) cfg (append cfg (list (cons k v))))))
         (cfg1    (ensure "hooksPath" "~/.config/git/hooks" cfg0))
         (cfg2    (ensure "excludesFile" "~/.config/git/ignore" cfg1))
         (core*   (git-section (name "core") (config cfg2)))
         (final   (append rest (list core*))))
    (string-join (map serialize-git-section final) "")))

;; If the list is empty -> emit an empty, non-exec file (Git ignores it).
;; If the list is non-empty -> emit an executable script that runs the payload with sh.
;; This avoids POSIX in the builder and avoids needing Guile POSIX at runtime.
(use-modules (gnu packages guile)) ; for 'guile-3.0' below (top of file once)

(define (hook-file* label files)
  (if (null? files)
      ;; Empty hook is just an executable script that exits 0
      (program-file label
        #~(exit 0)
        #:guile guile-3.0)
      (let ((payload
             (mixed-text-file (string-append label ".payload")
               (serialize-text-config label files))))
        ;; program-file produces an executable wrapper
        (program-file label
          #~(begin
              (use-modules (ice-9 popen)) ; provides system*
              ;; run payload via sh
              (apply system* "sh" #$payload (cdr (command-line))))
          #:guile guile-3.0))))

;; -----------------------------------------------------------------------------
;; Configuration
;; -----------------------------------------------------------------------------

(define-configuration home-git-configuration
  (config
   (list-of-git-sections '())
   "List of git sections to add to global config")
  (global-ignore
   (text-config '())
   "List of file-objects to write to global ignore-file")
  (applypatch-msg-hook
   (text-config '())
   "List of file-objects to write to global applypatch-msg-hook script")
  (commit-msg-hook
   (text-config '())
   "List of file-objects to write to global commit-msg-hook script")
  (fsmonitor-watchman-hook
   (text-config '())
   "List of file-objects to write to global fsmonitor-watchman-hook script")
  (post-merge-hook
   (text-config '())
   "List of file-objects to write to global post-merge-hook script")
  (post-update-hook
   (text-config '())
   "List of file-objects to write to global post-update-hook script")
  (pre-merge-commit-hook
   (text-config '())
   "List of file-objects to write to global pre-merge-commit-hook script")
  (pre-rebase-hook
   (text-config '())
   "List of file-objects to write to global pre-rebase-hook script")
  (pre-receive-hook
   (text-config '())
   "List of file-objects to write to global pre-receive-hook script")
  (pre-applypatch-hook
   (text-config '())
   "List of file-objects to write to global pre-applypatch-hook script")
  (pre-commit-hook
   (text-config '())
   "List of file-objects to write to global pre-commit-hook script")
  (pre-push-hook
   (text-config '())
   "List of file-objects to write to global pre-push-hook script")
  (prepare-commit-msg-hook
   (text-config '())
   "List of file-objects to write to global prepare-commit-msg-hook script")
  (push-to-checkout-hook
   (text-config '())
   "List of file-objects to write to global push-to-checkout-hook script")
  (sendemail-validate-hook
   (text-config '())
   "List of file-objects to write to global sendemail-validate-hook script")
  (update-hook
   (text-config '())
   "List of file-objects to write to global update-hook script"))

;; -----------------------------------------------------------------------------
;; Services
;; -----------------------------------------------------------------------------

(define (home-git-profile-service-type config)
  ;; Keep original package set for back-compat.
  (list git nss-certs pre-commit))

(define (home-git-files-service-type config)
  (list
   ;; Hooks
   `("git/hooks/applypatch-msg"
     ,(hook-file* "applypatch-msg"
                  (home-git-configuration-applypatch-msg-hook config)))
   `("git/hooks/commit-msg"
     ,(hook-file* "commit-msg"
                  (home-git-configuration-commit-msg-hook config)))
   `("git/hooks/fsmonitor-watchman"
     ,(hook-file* "fsmonitor-watchman"
                  (home-git-configuration-fsmonitor-watchman-hook config)))
   `("git/hooks/post-merge"
     ,(hook-file* "post-merge"
                  (home-git-configuration-post-merge-hook config)))
   `("git/hooks/post-update"
     ,(hook-file* "post-update"
                  (home-git-configuration-post-update-hook config)))
   `("git/hooks/pre-merge-commit"
     ,(hook-file* "pre-merge-commit"
                  (home-git-configuration-pre-merge-commit-hook config)))
   `("git/hooks/pre-rebase"
     ,(hook-file* "pre-rebase"
                  (home-git-configuration-pre-rebase-hook config)))
   `("git/hooks/pre-receive"
     ,(hook-file* "pre-receive"
                  (home-git-configuration-pre-receive-hook config)))
   `("git/hooks/pre-applypatch"
     ,(hook-file* "pre-applypatch"
                  (home-git-configuration-pre-applypatch-hook config)))
   `("git/hooks/pre-commit"
     ,(hook-file* "pre-commit"
                  (home-git-configuration-pre-commit-hook config)))
   `("git/hooks/pre-push"
     ,(hook-file* "pre-push"
                  (home-git-configuration-pre-push-hook config)))
   `("git/hooks/prepare-commit-msg"
     ,(hook-file* "prepare-commit-msg"
                  (home-git-configuration-prepare-commit-msg-hook config)))
   `("git/hooks/push-to-checkout"
     ,(hook-file* "push-to-checkout"
                  (home-git-configuration-push-to-checkout-hook config)))
   `("git/hooks/sendemail-validate"
     ,(hook-file* "sendemail-validate"
                  (home-git-configuration-sendemail-validate-hook config)))
   `("git/hooks/update"
     ,(hook-file* "update"
                  (home-git-configuration-update-hook config)))

   ;; Global ignore file
   `("git/ignore"
     ,(mixed-text-file "global-ignore"
        (serialize-text-config "ignore"
          (home-git-configuration-global-ignore config))))

   ;; Global config (ensures hooksPath & excludesFile)
   `("git/config"
     ,(mixed-text-file "config"
        (serialize-list-of-git-sections "config"
          (home-git-configuration-config config))))))

(define-public home-git-service-type
  (service-type
   (name 'home-git)
   (description "Manage git config and related setup / packages")
   (default-value '())
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      home-git-files-service-type)
     (service-extension
      home-profile-service-type
      home-git-profile-service-type)))))
