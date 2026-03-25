(define-module (peteches channels base)
  #:use-module (gnu)
  #:use-module (guix channels)
  #:export (%base-channels))

(define %base-channels
  (list
   (channel
    (name 'simendsjo)
    (url "https://git.sr.ht/~simendsjo/dotfiles")
    (commit "ddbbb26091c884d68c6870819e7a3f811cbe35a7"))
   ;; Guix-Science (free)
    (channel
     (name 'guix-science)
     (url "https://codeberg.org/guix-science/guix-science.git")
     (branch "master")
     (commit "28e4880093f98b85489cb9f2d7602282eda84fbb")
     (introduction
      (make-channel-introduction
       "b1fe5aaff3ab48e798a4cce02f0212bc91f423dc"
       (openpgp-fingerprint
        "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))
    (channel
     (name 'nonguix)
     (url "https://gitlab.com/nonguix/nonguix")
     (commit "b8648bf188df213455983e7eaab9f6c6101ca0c2")
     (introduction
      (make-channel-introduction
       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
       (openpgp-fingerprint
	"2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
   (channel
    (name 'guix)
    (url "https://codeberg.org/guix/guix.git")
    (commit "2a2fe7705e9f881a8a76b4b74df96784f793af54")
    (introduction
     (make-channel-introduction
      "199fd26ab268d4f26cebcb39e844fe4ff9bea9bc"
      (openpgp-fingerprint
       "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))
%base-channels
