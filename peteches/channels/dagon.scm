;;; peteches/channels/dagon.scm — %dagon-channels: %base-channels + HPC non-free.
;;;
;;; dagon's home config uses this (inherited from nug, the desktop this
;;; module was renamed from after nug's decommission); nyarlothotep uses
;;; %base-channels directly. The only addition is guix-hpc-non-free, for
;;; CUDA/GPU work. Note it has no channel introduction, so its commits are
;;; not signature-verified the way every channel in base.scm is.
;;;
;;; Kept in sync by hand with base.scm / manual.scm — see the header in
;;; base.scm, and prefer the `/update-channels' skill.

(define-module (peteches channels dagon)
  #:use-module (guix channels)
  #:use-module (peteches channels base)
  #:export (%dagon-channels))

(define %dagon-channels
  (append
   %base-channels
   (list
    ;; Guix-HPC Non-Free
    (channel
     (name 'guix-hpc-non-free)
     (url "https://gitlab.inria.fr/guix-hpc/guix-hpc.git")
     (branch "master")
     (commit "e23f019cbd3322e12dcb3f2f3691b02028ac297d")
     ))))
%dagon-channels
