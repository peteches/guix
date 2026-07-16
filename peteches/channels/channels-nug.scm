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
(list
 (channel
  (name 'peteches)
  (url "https://codeberg.org/peteches/guix-channel")
  (commit "0f072b6a6733d0a249d543424daac5a758562f51")
  (introduction
   (make-channel-introduction
    "ff4b8a08276932c10cf6ca8cf726d78a86c17588"
    (openpgp-fingerprint
     "73C1 C132 9190 37C0 6D6A  6729 A6E8 150F ED00 29D7")))))
