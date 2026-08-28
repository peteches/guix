;; peteches/packages/zoom.scm
;;
;; Upstream nonguix's `zoom' package (nongnu/packages/messaging.scm, current
;; as of nonguix master and the commit pinned in peteches/channels/base.scm)
;; wraps the "zoom" binary's LD_LIBRARY_PATH with every *dependency*
;; package's lib directory, but never adds the package's own output
;; directories. Zoom bundles several of its own shared libraries --
;; including libcef.so (Chromium Embedded Framework) -- under
;; lib/zoom/cef/, which is never on the search path.
;;
;; Confirmed live on dagon: launching `zoom' fails immediately with
;; "error while loading shared libraries: libcef.so: cannot open shared
;; object file: No such file or directory". Checked nonguix master
;; (2026-08-28) -- the wrap-program phase there is byte-for-byte the same
;; for this version, so bumping the pin would not fix it.
;;
;; This re-wraps the already-built "zoom" launcher with the two missing
;; paths appended to LD_LIBRARY_PATH, on top of everything upstream's
;; phase already sets.

(define-module (peteches packages zoom)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module ((nongnu packages messaging) #:prefix upstream:))

(define-public zoom
  (package/inherit upstream:zoom
    (arguments
     (substitute-keyword-arguments (package-arguments upstream:zoom)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'wrap-where-patchelf-does-not-work 'wrap-cef-lib-path
              (lambda _
                (wrap-program (string-append #$output "/lib/zoom/zoom")
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append #$output "/lib/zoom")
                     ,(string-append #$output "/lib/zoom/cef"))))))))))))
