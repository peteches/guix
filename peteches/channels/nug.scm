;;; peteches/channels/nug.scm — %nug-channels: %base-channels + HPC non-free.
;;;
;;; nug's home config uses this; nyarlothotep uses %base-channels directly.
;;; The only addition is guix-hpc-non-free, for nug's CUDA/GPU work.  Note
;;; it has no channel introduction, so its commits are not signature-verified
;;; the way every channel in base.scm is.
;;;
;;; Kept in sync by hand with base.scm / manual.scm / channels-nug.scm — see
;;; the header in base.scm, and prefer the `/update-channels' skill.

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
