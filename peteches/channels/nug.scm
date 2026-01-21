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
     (commit "6cc008a008606a3048ee558bbd91fed4e205b657")
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
     (commit "b94a466109e546b01eada60804661bdcd18e6199")
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
     (commit "9ce7aba36c32c343dc32609f36e7d98672c93ca7")
     ))))
