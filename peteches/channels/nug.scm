(define-module (peteches channels nug)
  #:use-module (guix channels)
  #:use-module (peteches channels base)
  #:export (%nug-channels))

(define %nug-channels
  (append
   %base-channels
   (list
    
    ;; Guix-Science (free)
    (channel
     (name 'guix-science)
     (url "https://codeberg.org/guix-science/guix-science.git")
     (branch "master")
     (commit "1573f7f57b4d1122844b2397fda5ad7a213068cc")
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
     (commit "c0aa6541b5c813d5520783b7e5b3956a753138cd")
     (introduction
      (make-channel-introduction
       "58661b110325fd5d9b40e6f0177cc486a615817e"
       (openpgp-fingerprint
        "CA4F 8CF4 37D7 478F DA05  5FD4 4213 7701 1A37 8446"))))

    (channel
     (name 'peteches)
     (url "file:///home/peteches/area_51/peteches-channel")
     (branch "main"))  
    
    ;; Guix-HPC Non-Free
    (channel
     (name 'guix-hpc-non-free)
     (url "https://gitlab.inria.fr/guix-hpc/guix-hpc.git")
     (branch "master")
     (commit "652f5b762b751699175a98f665b646a886679974")
     ))))
