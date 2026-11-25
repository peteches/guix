;;; peteches/channels/channels.scm — %base-channels, the canonical channel set.
;;;
;;; This is the single source of truth for all routine channel pins.
;;; Every other file in this directory either reuses these definitions via
;;; #:use-module or, like manual.scm, provides a bare list form for guix pull -C
;;; by calling out to this module.
;;;
;;; CHANNEL SETS
;;;
;;;   %base-channels       Verified by channel introduction (OpenPGP fingerprint).
;;;                        Used by nyarlothotep, all VMs, and everything except
;;;                        dagon's desktop.
;;;
;;;   %dagon-channels      = %base-channels + guix-hpc-non-free.
;;;                        Used by dagon's home config.  guix-hpc-non-free has
;;;                        no channel introduction — its commits are not
;;;                        signature-verified (see .guix-channel on that repo).
;;;
;;; PINS AND UPDATES
;;;
;;; Channels are pinned by commit + verified by channel introduction.
;;; The introduction is a property of the channel, not of the pin — change
;;; the commit on an update, never the introduction.
;;;
;;; `guix' is pinned here too, so `guix pull' with these channels replaces
;;; the default guix channel rather than adding to it.
;;;
;;; There used to be a `peteches' channel here (codeberg.org/peteches/guix-
;;; channel) supplying (peteches packages …), (peteches home services …) and
;;; (peteches services …). Codeberg's ToS changed to prohibit AI-generated
;;; code, and since this repo is now developed with Claude Code, that channel
;;; was folded back into this repo — those modules now live under
;;; peteches/packages/, peteches/home/services/ and peteches/services/ here,
;;; loaded via `-L .' like everything else. No channel pin, no re-pinning
;;; step: editing a service type is just editing the file.
;;;
;;; The `critical-grind' channel (the Critical Grind application repo) is
;;; deliberately NOT here.  It lives in its own dedicated file,
;;; peteches/channels/critical-grind.scm, because the repo is private on
;;; GitHub and fetched over SSH -- guix authenticates git fetches with
;;; (%make-auth-ssh-agent) alone, so only machines that load that deploy
;;; key into an agent (the CI deploy task; optionally claude-workstation)
;;; should pull it, rather than exposing every machine's routine `guix
;;; pull' to a channel it can't authenticate.
;;;
;;; DEPENDENCIES
;;;
;;; `guix-science-nonfree' declares `guix-science' and `guix-past' as
;;; dependencies in its `.guix-channel' file.  Guix resolves these
;;; transitively during `guix pull', so they need not appear in your local
;;; channels list — guix will fetch them automatically.  This is one reason
;;; switching from `guix pull -C' to standard `guix pull' helps: transitive
;;; deps resolve automatically instead of needing manual tracking.

(define-module (peteches channels)
  #:use-module (guix channels)
  #:export (%base-channels
            %dagon-channels))

;;; Base channel set — all channels verified by channel introduction.
(define %base-channels
  (list
   (channel
    (name 'sops-guix)
    (url "https://github.com/fishinthecalculator/sops-guix.git")
    (branch "main")
    (commit "c3045852cec9e1d4ffa125661789ed22a673cffb")
    (introduction
     (make-channel-introduction
      "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
      (openpgp-fingerprint
       "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
   ;; Guix-Science (free)
   (channel
    (name 'guix-science)
    (url "https://codeberg.org/guix-science/guix-science.git")
    (branch "master")
    (commit "f8b5232a16643ac3fdedc3c2c071b674da823b35")
    (introduction
     (make-channel-introduction
      "b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
      (openpgp-fingerprint
       "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
   ;; Guix-Science (non-free)
   (channel
    (name 'guix-science-nonfree)
    (url "https://codeberg.org/guix-science/guix-science-nonfree.git")
    (branch "master")
    (commit "766a8138ad450832807cde1330ac66b88c40c06d")
    (introduction
     (make-channel-introduction
      "58661b110325fd5d9b40e6f0177cc486a615817e"
      (openpgp-fingerprint
       "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
   (channel
    (name 'nonguix)
    (url "https://gitlab.com/nonguix/nonguix.git")
    (branch "master")
    (commit "ee118ce90d55c9d80af11295f28d6f971369161c")
    (introduction
     (make-channel-introduction
      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
      (openpgp-fingerprint
       "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
   (channel
    (name 'guix)
    (url "https://codeberg.org/guix/guix.git")
    (branch "master")
    (commit "c947335d508e5cde9464b1102bc7fbcfa835d883")
    (introduction
     (make-channel-introduction
      "199fd26ab26cebcb39e844fe4ff9bea9bc"
      (openpgp-fingerprint
       "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

;; Full channel set — inherits %base-channels and adds guix-hpc-non-free.
;; guix-hpc-non-free lacks a channel introduction, so its commits are not
;; signature-verified (unlike every channel above).  This is a known
;; limitation inherited from upstream.
(define %dagon-channels
  (append
   %base-channels
   (list
    (channel
     (name 'guix-hpc-non-free)
     (url "https://gitlab.inria.fr/guix-hpc/guix-hpc.git")
     (branch "master")
     (commit "e23f019cbd3322e12dcb3f2f3691b02028ac297d")))))
