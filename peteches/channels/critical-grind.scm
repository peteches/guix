;;; peteches/channels/critical-grind.scm — the Critical Grind app channel.
;;;
;;; Provides (critical-grind packages campaign) and (critical-grind services
;;; campaign) for peteches/systems/critical-grind-campaign.scm.
;;;
;;; Deliberately NOT in base.scm/manual.scm: the repo is private on GitHub
;;; and fetched over SSH, and guix authenticates git fetches with
;;; (%make-auth-ssh-agent) alone -- no ~/.ssh/config, no key file directly.
;;; Only machines that load the read-only deploy key into an agent (the CI
;;; deploy task; optionally claude-workstation) should pull this file.
;;; Every other machine's routine `guix pull' never sees it.
;;;
;;; No channel introduction: commits are unauthenticated and guix pull will
;;; say so on every pull. Shipping a new version of the app is a commit
;;; bump here, nothing more -- see scripts/bump-channel.sh in this repo.
;;;
;;; A bare `(list ...)', not a `define-module' -- `guix pull -C' on this
;;; fleet's guix version cannot load a define-module'd file at all (confirmed
;;; by testing; base.scm's own header comment claiming otherwise is stale).
;;; deploy-critical-grind.scm duplicates this channel's entry rather than
;;; importing it as a module, for the same reason.

(list
 (channel
  (name 'critical-grind)
  (url "git@github.com:peteches/critical-grind-battlefronts.git")
  (branch "main")
  (commit "dbfca0aab84f260627ec4e89f0d987841c3696a2")))
