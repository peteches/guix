(define-module (peteches channels base)
  #:use-module (gnu)
  #:use-module (guix channels)
  #:export (%base-channels))

(define %base-channels
  (list
   (channel
    (name 'simendsjo)
    (url "https://git.sr.ht/~simendsjo/dotfiles")
    (commit "883c5b28c280afa6ea86aa95b2c33f57567a023c"))
   (channel
    (name 'nonguix)
    (url "https://gitlab.com/nonguix/nonguix")
    (commit "ba6ce23e785d1ccd9194202d8f27f6db4b649170")
    (introduction
     (make-channel-introduction
      "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
      (openpgp-fingerprint
       "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
   (channel
    (name 'guix)
    (url "https://codeberg.org/guix/guix.git")
    (commit "130421225ed87fa3a2feaa4056c090000f7ffcb8")
    (introduction
     (make-channel-introduction
      "199fd26ab268d4f26cebcb39e844fe4ff9bea9bc"
      (openpgp-fingerprint
       "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))
%base-channels
