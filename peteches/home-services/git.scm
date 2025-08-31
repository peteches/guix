(define-module (peteches home-services git)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services configuration)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (home-git-configuration git-section git-section-name git-section-config))


(define  (serialize-string field-name value)
  value)

(define (list-of-key-values? lst)
  ((list-of key-value?) lst))

(define (serialize-list-of-key-values field-name values)
  (string-join (map (lambda (x)
		      (string-append (car x) " = " (cdr x) "\n"))
		    values)
	       ""))

(define-configuration git-section
  (name
   (string "")
   "Name of the git config section")
  (config
   (list-of-key-values '())
   "list of key value pairs to populate the config"))

(define (list-of-git-sections? sections)
  ((list-of git-section?) sections))

(define (serialize-list-of-git-sections fieldname sections)
  (let ((core-section (filter (lambda (x)
				(string=? "core" (git-section-name x)))
			      sections)))
    (if (= 0 (length core-section))
	(begin
	  (set! core-section (list (git-section (name "core")
						(config '(("hooksPath" . "~/.config/git/hooks")))))))
	(begin
	  (let ((conf (filter (lambda (x) (string=? "hooksPath" (car x))) (git-section-config (car core-section)))))
	    (if (= 0 (length conf))
		(begin
		  (append! (git-section-config (car core-section)) '(("hooksPath" . "~/.config/git/hooks"))))))))
    (string-join (map serialize-git-section (append sections core-section)) "")))

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
   "List of file-objects to write to global update-hook script")
  )


(define (key-value? x)
  (and
   (string? (car x))
   (string? (cdr x))))

(define (serialize-git-section section)
  (string-append "[" (git-section-name section) "]\n"
		 (serialize-list-of-key-values "c" (git-section-config section))
		 "\n"))

(define (home-git-profile-service-type config)
  (list git nss-certs))

(define (home-git-files-service-type config)
  (list
   `("git/hooks/applypatch-msg"
     ,(mixed-text-file "applypatch-msg"
		       (serialize-text-config "applypatch-msg" (home-git-configuration-applypatch-msg-hook config))))
   `("git/config" ,(mixed-text-file "config"
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



