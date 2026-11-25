;;; peteches/channels/critical-grind.scm — the Critical Grind app channel.
;;;
;;; Provides (critical-grind packages campaign) and (critical-grind services
;;; campaign) for peteches/systems/critical-grind-campaign.scm.
;;;
;;; Deliberately kept separate from channels.scm: the repo is private on GitHub
;;; and fetched over SSH, and guix authenticates git fetches with
;;; (%make-auth-ssh-agent) alone — no ~/.ssh/config, no key file directly.
;;; Only machines that load the read-only deploy key into an agent (the CI
;;; deploy task; optionally claude-workstation) should pull this channel.
;;; Every other machine's routine `guix pull' never sees it.
;;;
;;; No channel introduction: commits are unauthenticated and `guix pull` will
;;; warn on every pull.  Shipping a new version of the app is a commit bump
;;; here, nothing more — see scripts/bump-channel.sh.
;;;
;;; Also exported as a bare list so any `(guix pull -C …)' invocation can use
;;; this file directly without needing a module import layer.

(define-module (peteches channels critical-grind)
  #:use-module (guix channels)
  #:export (critical-grind-channel))

(define critical-grind-channel
  (channel
   (name 'critical-grind)
   (url "git@github.com:peteches/critical-grind-battlefronts.git")
   (branch "main")
   (commit "88d3d4c85801c7cc0ec2694b5a9d858e9e128c18")))

;; Bare list for standalone guix pull -C usage.
(list critical-grind-channel)
