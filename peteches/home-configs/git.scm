(define-module (peteches home-configs git)
  #:use-module (guix gexp)
  #:use-module (gnu packages gnupg)
  #:use-module (peteches home services git))

(define-public peteches-gpg-for-git
  (program-file
   "gpg-for-git-peteches"
   #~(begin
       (use-modules (srfi srfi-13))

       (define user-data
         (or (getenv "PINENTRY_USER_DATA") ""))

       (if (string-contains user-data "MAGIT_TRAMP=1")
           (apply execl
                  #$(file-append gnupg "/bin/gpg")
                  "gpg"
                  "--pinentry-mode"
                  "loopback"
                  (cdr (command-line)))
           (apply execl
                  #$(file-append gnupg "/bin/gpg")
                  "gpg"
                  (cdr (command-line)))))))

(define-public git-config
  (list (git-section
	 (name "user")
	 (config '(("name" . "Pete 'Peteches' McCabe")
		   ("email" . "pete@peteches.co.uk")
		   ("signingkey" . "A6E8150FED0029D7"))))
	(git-section
	 (name "core")
	 (config '(("compression" . "6")
		   ("editor" . "emacsclient --create-frame")
		   ("hooksPath" . "~/.config/git/hooks"))))
	(git-section
	 (name "url \"git@github.com:peteches\"")
	 (config '(("insteadOf" . "https://github.com/peteches"))))
	(git-section
	 (name "pull")
	 (config '(("ff" . "true")
		   ("rebase" . "true"))))
	(git-section
	 (name "rerere")
	 (config '(("autoUpdate" . "true")
		   ("enabled" . "true"))))
	(git-section
	 (name "init")
	 (config '(("defaultBranch" . "main"))))
	(git-section
	 (name "commit")
	 (config '(("gpgSign" . "true"))))
	(git-section
	 (name "gpg")
	 (config `(("program" . ,peteches-gpg-for-git))))
	(git-section
	 (name "github")
	 (config '(("user" . "peteches"))))))
