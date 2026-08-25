;;; peteches/home/modules/gpg.scm — gpg-agent with a dispatching pinentry.
;;;
;;; `pinentry-peteches' is a generated program that picks a real pinentry at
;;; runtime by inspecting $PINENTRY_USER_DATA, because one static choice
;;; cannot serve both a Wayland session and a headless TRAMP connection:
;;;
;;;   MAGIT_TRAMP=1 / USE_TTY=1  → pinentry-tty
;;;   USE_EMACS=1                → pinentry-emacs
;;;   $WAYLAND_DISPLAY or $DISPLAY set → pinentry-qt
;;;   otherwise                  → pinentry-tty
;;;
;;; Related but separate: git signing goes through the `gpg-for-git-peteches'
;;; wrapper package in (peteches home modules git), which switches gpg to
;;; --pinentry-mode loopback under MAGIT_TRAMP=1.  Both halves are needed —
;;; this one picks the pinentry, that one bypasses it for remote signing.
;;; `allow-loopback-pinentry' in extra-content is what permits the latter.
;;;
;;; ssh-support? #t means gpg-agent also serves as the SSH agent.

(define-module (peteches home modules gpg)
  #:use-module (gnu home services)
  #:use-module (gnu home services gnupg)
  #:use-module ((gnu packages gnupg) #:hide (gnupg))
  ;; Upstream `gnupg' is pinned to an explicit development/test snapshot
  ;; (2.5.20) at the current channel commit -- its `gpg-agent --supervised'
  ;; socket-activation mode does not correctly serve connections through
  ;; the socket Shepherd binds for it, breaking passphrase caching
  ;; entirely.  See peteches/packages/gnupg.scm for the full story; this
  ;; pins back to the known-good stable 2.4.8.
  #:use-module (peteches packages gnupg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (base-gpg-service
            base-log-dir-service))

;;; gpg-agent's log-file below lands in ~/.local/var/log, which is not
;;; guaranteed to exist on a fresh home — koboldcpp's shepherd service
;;; creates the same directory, but only on the hosts that run it.  Create
;;; it at activation time on every host instead, same pattern as
;;; base-ssh-control-dir-service in (peteches home modules ssh).
(define-public base-log-dir-service
  (simple-service 'log-dir
                  home-activation-service-type
                  #~(let ((home (getenv "HOME")))
                      (for-each (lambda (dir)
                                  (unless (file-exists? dir)
                                    (mkdir dir #o700)))
                                (list (string-append home "/.local")
                                      (string-append home "/.local/var")
                                      (string-append home "/.local/var/log"))))))

(define-public base-gpg-service
  (service home-gpg-agent-service-type
	   (home-gpg-agent-configuration
	    ;; home-gpg-agent-configuration's `gnupg' field defaults to
	    ;; whatever (gnu packages gnupg) provides in *its own* module
	    ;; scope (an #:autoload), not whatever this file imports under
	    ;; that name -- it must be set explicitly here for the pinned
	    ;; 2.4.8 override above to actually take effect.
	    (gnupg gnupg)
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
