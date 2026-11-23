;;; peteches/channels/manual.scm — plain channel list for `guix pull -C'.
;;;
;;; Mirrors %dagon-channels from (peteches channels dagon) — i.e. %base-channels
;;; plus guix-hpc-non-free.  It has a `define-module' header but ends in a
;;; bare `(list ...)', so it still doubles as a plain channels list:
;;;
;;;   guix pull -C peteches/channels/manual.scm
;;;
;;; or symlinked to ~/.config/guix/channels.scm.  This is the full list.
;;;
;;; Kept in sync BY HAND with base.scm / dagon.scm — see the header in
;;; base.scm for the full picture, and prefer the `/update-channels' skill
;;; over editing pins here directly.

;;;
;;; A `define-module' header (matching this file's path) makes guix load it
;;; cleanly when it scans the `-L .' load path — `guix home'/`guix system'
;;; load every module under `-L .', and a plain list without a module header
;;; fails that load ("no code for module ...").  The trailing bare
;;; `(list ...)' still lets the file double as a plain channels list for
;;; `guix pull -C peteches/channels/manual.scm', exactly like base.scm.

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
  (name 'guix-hpc-non-free)
  (url "https://gitlab.inria.fr/guix-hpc/guix-hpc.git")
  (branch "master")
  (commit "e23f019cbd3322e12dcb3f2f3691b02028ac297d")))
