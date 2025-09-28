(define-module (peteches channels base)
  #:use-module (gnu)
  #:use-module (guix channels)
  #:export (%base-channels))

(define %base-channels
  (list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (commit "57c186c44fdec1461af63286cb9442424ca8a0b2")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
      (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix.git")
        (commit "325058efde")
        (introduction
          (make-channel-introduction
	    "199fd26ab268d4f26cebcb39e844fe4ff9bea9bc"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))
