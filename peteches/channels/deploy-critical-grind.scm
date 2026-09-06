;;; peteches/channels/deploy-critical-grind.scm — channel set for deploying
;;; the critical-grind VM.
;;;
;;; %base-channels plus the critical-grind app channel (see critical-grind.scm
;;; for why that channel isn't in base.scm itself). This is what the CI
;;; deploy task pulls before running `guix deploy' against
;;; critical-grind-campaign-machine -- and what any operator machine doing
;;; interactive work on that VM's config should pull too, with the read-only
;;; deploy key loaded into an agent first.
;;;
;;; A `define-module' header makes this load cleanly under `-L .'; the
;;; trailing bare `(append ...)' lets it double as a plain channels list for
;;; `guix pull -C peteches/channels/deploy-critical-grind.scm'.

(define-module (peteches channels deploy-critical-grind)
  #:use-module (guix channels)
  #:use-module (peteches channels base)
  #:use-module (peteches channels critical-grind)
  #:export (%deploy-critical-grind-channels))

(define %deploy-critical-grind-channels
  (append %base-channels %critical-grind-channel))

%deploy-critical-grind-channels
