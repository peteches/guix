;;; peteches/packages/gpg.scm --- package for a smart pinentry switcher -*- scheme -*-

(define-module (peteches packages gpg)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (gnu packages bash)
  #:use-module (guix build-system trivial)  
  #:export (peteches-pinentry-switch))

(define pinentry-switch-script
  #~(string-append
     "#!" #$(file-append bash-minimal "/bin/sh") "\n"
     "# Minimal, reliable pinentry chooser for Emacs/Guix workflows.\n"
     "find_bin() { command -v \"$1\" 2>/dev/null; }\n"
     "PIN_TTY=\"$(find_bin pinentry-curses || find_bin pinentry-tty || true)\"\n"
     "PIN_GUI=\"$(find_bin pinentry-qt || find_bin pinentry-gtk-2 || find_bin pinentry-gnome3 || true)\"\n"
     "PIN_ANY=\"$(find_bin pinentry || true)\"\n"
     "\n"
     "case ${PINENTRY_USER_DATA} in\n"
     "*gui*)\n"
     "    [ -n \"${PIN_GUI}\" ] && exec \"${PIN_GUI}\" \"${@}\"\n"
     " ;;\n"
     "*tty*|*INSIDE_EMACS*|*USE_EMACS=1*)\n"
     "    [ -n \"${PIN_TTY}\" ] && exec \"${PIN_TTY}\" \"${@}\"\n"
     " ;;\n"
     "*)\n"
     "    [ -n \"$PIN_ANY\" ] && exec \"$PIN_ANY\" \"$@\"\n"
     "    [ -n \"$PIN_TTY\" ] && exec \"$PIN_TTY\" \"$@\"\n"
     "esac\n"
     "echo \"ERR 83886179 No usable pinentry (pinentry-switch)\" >&2\n"
     "exit 1\n"))


(define peteches-pinentry-switch
  (package
   (name "peteches-pinentry-switch")
   (version "0.1")
   (source #f)
   (build-system trivial-build-system)
   (inputs (list bash-minimal))
   (arguments
    (list
     #:modules '((guix build utils))
     #:builder
     #~(begin
         (use-modules (guix build utils))
         (let* ((out  #$output)
                (bin  (string-append out "/bin"))
                (prog (string-append bin "/pinentry-switch")))
           (mkdir-p bin)
           (call-with-output-file prog
             (lambda (p) (display #$pinentry-switch-script p)))
           (chmod prog #o755)))))
   (synopsis "Context-aware pinentry selector for Emacs/Guix workflows")
   (description "Chooses a pinentry backend based on context:
 - INSIDE_EMACS → TTY pinentry (so gpg-agent can divert to Emacs minibuffer)
 - Any terminal/tty → TTY pinentry
 - Otherwise, with DISPLAY/WAYLAND_DISPLAY → GUI pinentry
Falls back sensibly and propagates the environment to the chosen pinentry.")
   (home-page "https://example.invalid/peteches")
   (license expat)))
