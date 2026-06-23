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
     (commit "01019984193f32234cb16275c1ebc335b60d9652")
     ))))
%nug-channels
