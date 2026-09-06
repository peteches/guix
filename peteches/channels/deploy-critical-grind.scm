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
  (commit "c53e27e533836ea8595626ba6796dee5362f8c4a")
  (introduction
   (make-channel-introduction
    "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
    (openpgp-fingerprint
     "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
 (channel
  (name 'guix-science)
  (url "https://codeberg.org/guix-science/guix-science.git")
  (branch "master")
  (commit "52d7ab7851a5ced97c0c8f1a602a103b5ca0b046")
  (introduction
   (make-channel-introduction
    "b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
    (openpgp-fingerprint
     "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
 (channel
  (name 'guix-science-nonfree)
  (url "https://codeberg.org/guix-science/guix-science-nonfree.git")
  (branch "master")
  (commit "54a483df56f24d8fd1c91ed3de547f5f1cf85964")
  (introduction
   (make-channel-introduction
    "58661b110325fd5d9b40e6f0177cc486a615817e"
    (openpgp-fingerprint
     "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix.git")
  (branch "master")
  (commit "caa8c0b4646b993537be13c9bc819b3df68ab9b2")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
  (name 'guix)
  (url "https://codeberg.org/guix/guix.git")
  (branch "master")
  (commit "b13c7c02b5f6d635e123f863227aa32ac64e3498")
  (introduction
   (make-channel-introduction
    "199fd26ab268d4f26cebcb39e844fe4ff9bea9bc"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
 (channel
  (name 'critical-grind)
  (url "git@github.com:peteches/critical-grind-battlefronts.git")
  (branch "main")
  (commit "15e0724edea3ff84eb8b086989a5d9d70f50f517")))
