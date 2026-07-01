(define-module (peteches home-configs git)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (peteches home services git))

(define-public peteches-gpg-for-git
  (package
    (name "peteches-gpg-for-git")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      #~(let* ((bin (string-append #$output "/bin"))
               (program (string-append bin "/gpg-for-git-peteches")))
          ;; Keep the builder self-contained so it also works in minimal
          ;; offloaded build environments where Guix build modules are not in
          ;; Guile's load path.
          (mkdir bin)
          (call-with-output-file program
            (lambda (port)
              (display "#!" port)
              (display #$(file-append bash-minimal "/bin/sh") port)
              (display "
case \"${PINENTRY_USER_DATA:-}\" in
  *MAGIT_TRAMP=1*) exec " port)
              (display #$(file-append gnupg "/bin/gpg") port)
              (display " --pinentry-mode loopback \"$@\" ;;
  *) exec " port)
              (display #$(file-append gnupg "/bin/gpg") port)
              (display " \"$@\" ;;
esac
" port)))
          (chmod program #o555))))
    (home-page "https://peteches.co.uk")
    (synopsis "GPG wrapper for Git signing from Magit over TRAMP")
    (description
     "Wrap GnuPG for Git signing.  When Magit marks a remote TRAMP process
with PINENTRY_USER_DATA=MAGIT_TRAMP=1, run gpg with --pinentry-mode loopback;
otherwise execute gpg normally.")
    (license license:gpl3+)))

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
	 (config '(("program" . "gpg-for-git-peteches"))))
	(git-section
	 (name "github")
	 (config '(("user" . "peteches"))))))
