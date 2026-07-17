;; Plain channels file — no define-module; evaluates to a channel list, so
;; it works with `guix pull -C' and can be symlinked to
;; ~/.config/guix/channels.scm.
;;
;; SCOPE: despite the name, this holds ONLY the `peteches' channel — it is
;; not the nug channel set and not a mirror of (peteches channels nug).
;; Pulling with it leaves guix itself, nonguix, sops-guix and the rest
;; unpinned (guix falls back to its built-in default channel).  For the full
;; pinned set use manual.scm in this directory instead.
;;
;; Kept in sync by hand with base.scm / nug.scm / manual.scm — see the header
;; in base.scm, and prefer the `/update-channels' skill over editing the pin
;; here directly.
;;
;; A `define-module' header (matching this file's path) makes guix load it
;; cleanly when it scans the `-L .' load path — `guix home'/`guix system'
;; load every module under `-L .', and a plain list without a module header
;; fails that load ("no code for module ...").  The trailing bare `(list ...)'
;; still lets the file double as a plain channels list for `guix pull -C'.
(define-module (peteches channels channels-nug)
  #:use-module (guix channels))

(list
 (channel
  (name 'peteches)
  (url "https://codeberg.org/peteches/guix-channel")
  (commit "3cd8b41934f8384ef278c2895d4128b013144815")
  (introduction
   (make-channel-introduction
    "ff4b8a08276932c10cf6ca8cf726d78a86c17588"
    (openpgp-fingerprint
     "73C1 C132 9190 37C0 6D6A  6729 A6E8 150F ED00 29D7")))))
