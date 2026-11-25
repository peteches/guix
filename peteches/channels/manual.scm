;;; peteches/channels/manual.scm — plain channel list for `guix pull -C'.
;;;
;;; Re-exports %dagon-channels from (peteches channels), i.e. the full routine
;;; channel set with guix-hpc-non-free added on top.  This file's sole purpose
;;; is to serve as a plain `(list …)' expression that `guix pull -C' can read;
;;; all pins are maintained in channels.scm and referenced here via module
;;; import, so there is exactly one copy of every commit SHA.
;;;
;;; For symlinking to ~/.config/guix/channels.scm when you want standard
;;; `guix pull' behavior (channels are installed into your profile), use
;;; this file instead of running `guix pull -C':
;;;
;;;   ln -sf $(pwd)/peteches/channels/manual.scm ~/.config/guix/channels.scm
;;;   guix pull
;;;
;;; Note: standard `guix pull' installs channel modules into your profile's
;;; guile site directory, making them available through Guile's load path
;;; automatically — no `-L' flags needed for channel imports.  The old setup
;;; using `guix pull -C' required manually adding `-L' paths for every
;;; channel checkout, which caused ABI mismatch crashes when the pulled guix
;;; version differed from the system-installed one.
;;;
;;; See channels.scm for the complete pin reference.

(use-modules (peteches channels))
%dagon-channels
