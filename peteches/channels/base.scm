(define-module (peteches channels base)
  #:use-module (gnu)
  #:use-module (guix channels)
  #:export (%base-channels))

(define %base-channels
  (list
   (channel
    (name 'simendsjo)
    (url "https://git.sr.ht/~simendsjo/dotfiles")
    (commit "99d950cb2b3b4a7d91b40883fd942e34b9c76b42"))
   ;; Guix-Science (free)
    (channel
     (name 'guix-science)
     (url "https://codeberg.org/guix-science/guix-science.git")
     (branch "master")
     (commit "36c3777ffe1fc3eb12b5f659b26d9754c8e0655c")
     (introduction
      (make-channel-introduction
       "b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
       (openpgp-fingerprint
        "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
    (channel
     (name 'nonguix)
     (url "https://gitlab.com/nonguix/nonguix")
     (commit "da4e72efef62d48dbc2eb089c36972ff55fe6acd")
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
	"2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
   (channel
    (name 'guix)
    (url "https://codeberg.org/guix/guix.git")
    (commit "75001089b5b43342c3e784d7ca2c42eade953211")
    (introduction
     (make-channel-introduction
      "199fd26ab268d4f26cebcb39e844fe4ff9bea9bc"
      (openpgp-fingerprint
       "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))
%base-channels
