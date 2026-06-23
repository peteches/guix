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
     (commit "471a32834b63ec0989feb4624dfa2603859ba8b4")
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
    (commit "5523e2d7ebda813b2a3f33b95e328f92b9037ff8")
    (introduction
     (make-channel-introduction
      "ff4b8a08276932c10cf6ca8cf726d78a86c17588"
      (openpgp-fingerprint
       "73C1 C132 9190 37C0 6D6A  6729 A6E8 150F ED00 29D7"))))))
%base-channels
