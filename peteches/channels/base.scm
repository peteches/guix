;;; peteches/channels/base.scm — %base-channels, the pinned channel set.
;;;
;;; THE FOUR CHANNEL FILES.  Pinned commits are duplicated across four files
;;; in this directory, and nothing enforces agreement between them.  Update
;;; all four together — the `/update-channels' skill (.claude/skills/) exists
;;; to do exactly this, and is the preferred route.
;;;
;;;   base.scm         (this file)  %base-channels.  Module; used by
;;;                    nyarlothotep's home config and by nug.scm below.
;;;                    THE REFERENCE — update here first.
;;;   nug.scm          %nug-channels = %base-channels + guix-hpc-non-free.
;;;                    Module; used by nug's home config.
;;;   manual.scm       Plain list mirroring %nug-channels (all 7 channels).
;;;                    No define-module, so it works with `guix pull -C'.
;;;   channels-nug.scm Plain list, but only the `peteches' channel — NOT a
;;;                    mirror of the above despite the name.  Pulling with
;;;                    it leaves guix itself unpinned.
;;;
;;; The `channels-nug.scm' / `manual.scm' naming is misleading: manual.scm is
;;; the full plain list you probably want for `guix pull -C' or for
;;; symlinking to ~/.config/guix/channels.scm.  (CLAUDE.md's instruction to
;;; use channels-nug.scm for this predates the split.)
;;;
;;; Channels are pinned by commit + verified by channel introduction
;;; (an OpenPGP fingerprint + the first signed commit).  The introduction is
;;; a property of the channel, not of the pin — change the commit on an
;;; update, never the introduction.
;;;
;;; `guix' is pinned here too, so `guix pull' with these channels replaces
;;; the default guix channel rather than adding to it.
;;;
;;; The `peteches' channel carries the custom packages, home services and
;;; system services this repo consumes as (peteches packages …),
;;; (peteches home services …) and (peteches services …).  Those live in
;;; codeberg.org/peteches/guix-channel, NOT in this repo — editing a service
;;; type means editing that channel and re-pinning its commit here.
;;;
;;; The trailing bare `%base-channels' lets the file double as a plain
;;; channels list for `guix pull -C peteches/channels/base.scm'.

(define-module (peteches channels base)
  #:use-module (gnu)
  #:use-module (guix channels)
  #:export (%base-channels))

(define %base-channels
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
   ;; Guix-Science (free)
    (channel
     (name 'guix-science)
     (url "https://codeberg.org/guix-science/guix-science.git")
     (branch "master")
     (commit "4d6fe4eab7cecc4d2d96a9476cd645bbbde727be")
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
     (commit "f30ef0318dd1c26bcb9d952a50cfa777429ab4e9")
     (introduction
      (make-channel-introduction
       "58661b110325fd5d9b40e6f0177cc486a615817e"
       (openpgp-fingerprint
        "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
    (channel
     (name 'nonguix)
     (url "https://gitlab.com/nonguix/nonguix.git")
     (branch "master")
     (commit "bf39542ca537fde8839b209ac21d6f3254469b15")
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
	"2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
   (channel
    (name 'guix)
    (url "https://codeberg.org/guix/guix.git")
    (branch "master")
    (commit "dd3e59ad40f480ae330e0814a7ab5bf32fa4ef9e")
    (introduction
     (make-channel-introduction
      "199fd26ab268d4f26cebcb39e844fe4ff9bea9bc"
      (openpgp-fingerprint
       "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
   (channel
    (name 'peteches)
    (url "https://codeberg.org/peteches/guix-channel")
    (branch "main")
    (commit "3cd8b41934f8384ef278c2895d4128b013144815")
    (introduction
     (make-channel-introduction
      "ff4b8a08276932c10cf6ca8cf726d78a86c17588"
      (openpgp-fingerprint
       "73C1 C132 9190 37C0 6D6A  6729 A6E8 150F ED00 29D7"))))))
%base-channels
