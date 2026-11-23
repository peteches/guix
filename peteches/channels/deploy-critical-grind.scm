;;; peteches/channels/deploy-critical-grind.scm — %base-channels plus
;;; critical-grind, for `guix pull -C'.
;;;
;;; What the CI deploy task's manual `guix deploy' actually needs is handled
;;; separately (it clones each channel and passes -L flags directly, to
;;; avoid `guix pull' in CI entirely -- see ci/tasks/critical-grind-deploy.yml
;;; in this repo and its own comments on why). This file is for an operator
;;; machine doing interactive work on the critical-grind-campaign VM's
;;; config: load the read-only deploy key into an ssh-agent, then
;;;
;;;   guix pull -C peteches/channels/deploy-critical-grind.scm
;;;
;;; Duplicates every entry from base.scm plus critical-grind.scm rather than
;;; importing them as modules -- matching the fleet's own established
;;; "three channel files, duplicated, nothing enforces agreement" convention
;;; (see base.scm's header) -- because `guix pull -C' on this fleet's guix
;;; version cannot load a define-module'd file at all (confirmed by testing).
;;; Keep in sync with base.scm and critical-grind.scm by hand.

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
    "199fd26ab268d4f26cebcb39e844fe4ff9bea9bc"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
 (channel
  (name 'critical-grind)
  (url "git@github.com:peteches/critical-grind-battlefronts.git")
  (branch "main")
  (commit "88d3d4c85801c7cc0ec2694b5a9d858e9e128c18")))
