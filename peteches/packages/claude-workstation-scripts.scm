(define-module (peteches packages claude-workstation-scripts)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (peteches repository)
  #:export (claude-workstation-peteches-scripts))

;; claude-workstation's peteches account: a `rec` wrapper that shadows the
;; profile's rec so pi-dictate (voice dictation in pi) captures a
;; desktop's PulseAudio session instead of this VM's nonexistent local
;; audio input.  The script (co-located with the account's home config)
;; picks per invocation whichever desktop has a live SSH connection to
;; the VM -- see its own header for the selection logic and tie-breaks,
;; and configs/hypr/peteches/autostart.lua for the desktop side of the
;; arrangement.
;;
;; Installed as bin/rec-mic (NOT bin/rec -- sox's own rec occupies that
;; name in the profile, which the wrapper execs) and exposed as
;; ~/.local/bin/rec via a home-files symlink (see
;; claude-workstation-peteches.scm).  The explicit chmod phase is
;; required because the daemon's add-to-store strips the exec bit from
;; local files -- the same make-executable phase peteches-desktop-scripts
;; uses for the same reason.

(define-public claude-workstation-peteches-scripts
  (package
    (name "claude-workstation-peteches-scripts")
    (version "0.1")
    (source
     (local-file (source-path "peteches/home/configs/claude-workstation-peteches-rec")))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("claude-workstation-peteches-rec" "bin/rec-mic"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'make-executable
            (lambda _
              (chmod (string-append (assoc-ref %outputs "out") "/bin/rec-mic")
                     #o755)
              #t)))))
    (home-page "https://github.com/peteches/guix")
    (license license:gpl3+)
    (synopsis "rec wrapper capturing a desktop's PulseAudio mic for pi-dictate")
    (description
     "The @command{rec-mic} wrapper shadows the profile's @command{rec}
for pi-dictate (voice dictation in pi) on claude-workstation, a VM with
no local audio input: it inspects the VM's established SSH connections
and points @env{PULSE_SERVER} at the desktop driving the VM right now,
so @command{rec -d pulse} captures that desktop's microphone.")))