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
     (commit "e225c0f0ee0203ad8469e964e176fbe0b11462da")
     ))))
%nug-channels
