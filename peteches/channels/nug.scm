(define-module (peteches channels nug)
  #:use-module (guix channels)
  #:use-module (peteches channels base)
  #:export (%nug-channels))

(define %nug-channels
  (append
   %base-channels
   (list
    ;; (channel
    ;;  (name 'peteches)
    ;;  (url "file:///home/peteches/area_51/peteches-channel")
    ;;  (branch "main"))

    ;; Guix-HPC Non-Free
    (channel
     (name 'guix-hpc-non-free)
     (url "https://gitlab.inria.fr/guix-hpc/guix-hpc.git")
     (branch "master")
     (commit "f0997dfc0b74272cba970c97773c63f7e126d46d")
     ))))
