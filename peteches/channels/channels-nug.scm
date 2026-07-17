;; Plain channels file for nug — suitable for `guix pull -C` and symlinking
;; to ~/.config/guix/channels.scm.  No define-module; evaluates to a channel list.
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
