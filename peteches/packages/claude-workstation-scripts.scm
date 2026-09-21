(define-module (peteches packages claude-workstation-scripts)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((gnu packages audio) #:select (sox))
  #:use-module ((guix licenses) #:prefix license:)
  #:export (claude-workstation-peteches-scripts))

;; claude-workstation's peteches account: a `rec` wrapper that IS the
;; profile's rec (this package provides bin/rec; sox is NOT in the
;; profile).  pi-dictate (voice dictation in pi) spawns `rec`; this VM
;; has no local audio input, so the wrapper points PULSE_SERVER at a
;; desktop's PulseAudio session -- the desktops expose their session
;; servers over TCP at Hyprland session start (see
;; configs/hypr/peteches/autostart.lua, restricted to this VM's
;; Tailscale IP).
;;
;; WHICH desktop is an explicit user choice, not auto-detected (both
;; can be connected at once): the /dictate pi slash command (a prompt
;; template, installed at ~/.pi/agent/prompts/dictate.md -- see
;; claude-workstation-peteches.scm) writes the choice to
;; ~/.config/dictate/host, which the wrapper reads per invocation.
;;
;; The wrapper is a MIXED-TEXT-FILE: the selection logic below, plus a
;; final exec line whose sox rec path is a file-like element that the
;; gexp inlines as the sox STORE PATH -- so the wrapper (the profile's
;; rec) execs the real sox rec and not itself.  (file-append is not
;; available in the build environment, which is why the path comes from
;; the gexp inlining rather than a build phase.)  The exec line uses
;; sox's `-d` (--default-device, a BOOLEAN -- it does not take a driver
;; name); with PULSE_SERVER set by the selection logic above, sox's
;; default device is the desktop's PulseAudio session, so it captures
;; that mic.  (An earlier `-d pulse` made sox treat `pulse` as a
;; filename -- "can't open input file `pulse'".)  The chmod phase is
;; required because copy-build-system does not preserve +x -- the same
;; make-executable pattern peteches-desktop-scripts uses.

(define %rec-selection-logic "#!/bin/sh
# pi-dictate (voice dictation in pi) rec wrapper for claude-workstation.
#
# This VM has no local audio input; audio comes from a DESKTOP's
# PulseAudio session, which the desktops expose over TCP at Hyprland
# session start (see configs/hypr/peteches/autostart.lua, restricted to
# this VM's Tailscale IP).
#
# WHICH desktop is an EXPLICIT user choice, not auto-detected -- both
# desktops can be connected at once and guessing is confusing.  The
# /dictate pi slash command (a prompt template installed at
# ~/.pi/agent/prompts/dictate.md, see claude-workstation-peteches.scm)
# writes the choice to ~/.config/dictate/host.  This wrapper reads it
# per invocation and points PULSE_SERVER at that desktop.
#
# If no choice has been made yet (file absent or unknown value), the
# ambient PULSE_SERVER from setup-environment (nyarlothotep) is left in
# place -- a static default, not a switch.
#
# The final exec line (the real sox rec, store path) is spliced in by
# the package -- see peteches/packages/claude-workstation-scripts.scm.

h=\"\"
if [ -n \"${HOME:-}\" ] && [ -r \"$HOME/.config/dictate/host\" ]; then
  read -r h < \"$HOME/.config/dictate/host\" 2>/dev/null || h=\"\"
fi
case $h in
  dagon)
    PULSE_SERVER=dagon.spaniel-cordylus.ts.net
    export PULSE_SERVER
    ;;
  nyarlothotep)
    PULSE_SERVER=nyarlothotep.spaniel-cordylus.ts.net
    export PULSE_SERVER
    ;;
esac
")

(define-public claude-workstation-peteches-scripts
  (package
    (name "claude-workstation-peteches-scripts")
    (version "0.1")
    (source
     (mixed-text-file "rec"
       %rec-selection-logic
       "exec " (file-append sox "/bin/rec") " -d \"$@\"\n"))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("rec" "bin/rec"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'make-executable
            (lambda _
              (chmod (string-append (assoc-ref %outputs "out") "/bin/rec")
                     #o555)
              #t)))))
    (home-page "https://github.com/peteches/guix")
    (license license:gpl3+)
    (synopsis "rec wrapper capturing a desktop's PulseAudio mic for pi-dictate")
    (description
     "The @command{rec} wrapper is the profile's rec on claude-workstation,
a VM with no local audio input: for pi-dictate (voice dictation in pi)
it reads the user's explicit desktop choice from
@file{~/.config/dictate/host} (set with the @command{/dictate} pi slash
command) and points @env{PULSE_SERVER} at that desktop's PulseAudio
session, so @command{rec -d pulse} captures that desktop's
microphone.")))