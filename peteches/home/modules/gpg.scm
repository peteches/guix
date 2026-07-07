(define-module (peteches home modules gpg)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (base-gpg-service))

(define-public base-gpg-service
  (service home-gpg-agent-service-type
	   (home-gpg-agent-configuration
	    ;; Pick a pinentry implementation using PINENTRY_USER_DATA.  Local
	    ;; Emacs sets USE_EMACS=1.  Magit/TRAMP signing uses loopback mode
	    ;; through git's gpg.program wrapper, so this pinentry wrapper remains
	    ;; for direct/local GPG operations.
	    (pinentry-program
	     (program-file
	      "pinentry-peteches"
	      #~(begin
		  (use-modules (srfi srfi-13))

		  (define user-data
		    (or (getenv "PINENTRY_USER_DATA") ""))

		  (define have-gui?
		    (or (getenv "WAYLAND_DISPLAY")
			(getenv "DISPLAY")))

		  (define-values (program argv0)
		    (cond
		     ((or (string-contains user-data "MAGIT_TRAMP=1")
			  (string-contains user-data "USE_TTY=1"))
		      (values #$(file-append pinentry-tty "/bin/pinentry-tty")
			      "pinentry-tty"))

		     ((string-contains user-data "USE_EMACS=1")
		      (values #$(file-append pinentry-emacs "/bin/pinentry-emacs")
			      "pinentry-emacs"))

		     (have-gui?
		      (values #$(file-append pinentry-qt "/bin/pinentry-qt")
			      "pinentry-qt"))

		     (else
		      (values #$(file-append pinentry-tty "/bin/pinentry-tty")
			      "pinentry-tty"))))

		  (apply execl program argv0 (cdr (command-line))))))
	    (extra-content (string-append
			    "log-file ${HOME}/.local/var/log/gpg-agent.log\n"
			    "verbose\n"
			    "disable-scdaemon\n"
			    "allow-emacs-pinentry\n"
			    "allow-loopback-pinentry\n"))
	    (ssh-support? #t))))
