;; peteches/packages/pocl.scm — pocl with its (failing) test suite disabled.
;;
;; comfyui's Intel Arc / Level Zero OpenCL stack lists upstream `pocl' directly, both in
;; the system profile (#:extra-packages in peteches/systems/comfyui.scm) and as a default
;; runtime-lib-package of the strata service (peteches/services/strata.scm).  At pocl 6.0
;; its own `check' phase fails deterministically on the offload builder
;; (guix-build.spaniel-cordylus.ts.net): ctest is run with --repeat until-pass:5 and still
;; exits non-zero after ~43 minutes, so retrying does not help — it is not a load race but a
;; genuine failure of some OpenCL test in that headless, GPU-less CPU-only environment.
;; Because pocl is spliced into comfyui's system derivation, one red suite aborts the whole
;; deploy exactly like python-fastapi did (see package-with-tests-off there).
;;
;; We do NOT want to edit upstream or rebuild the giant Intel/CUDA/kernel packages that are
;; already built and cached — pocl is a leaf-ish OpenCL *implementation* whose compiled
;; output does not depend on running its own tests.  So disable only its test phase via a
;; local wrapper, keeping name/version identical, and point both comfyui references at it so
;; the closure contains exactly one pocl variant (no profile conflict).
(define-module (peteches packages pocl)
  #:use-module (guix packages)              ;package / inherit / package-arguments
  #:use-module (gnu packages opencl)        ;pocl
  #:export (pocl-tests-off))

(define (package-with-tests-off pkg)
  "Return PKG with its test phase disabled, preserving every other build argument.

Guix's build systems read #:tests? through assq-ref, so the FIRST such key in a package's
arguments list wins.  We prepend our own and drop any the package already carried, which
forces tests off regardless of upstream's setting.  Same mechanism as
package-with-tests-off in (peteches services openvino), kept local here to avoid coupling
this package module to that service."
  (package
    (inherit pkg)
    (arguments
      ;; Prepend our #:tests? #f and rebuild the original argument plist minus any upstream
      ;; #:tests? pair, preserving order and all other keys (assq picks ours first).
      (cons* #:tests? #f
             (let loop ((a (package-arguments pkg)) (out '()))
               (cond
                ((null? a) out)
                ((eq? (car a) #:tests?) (loop (cddr a) out))   ;drop upstream tests? + value
                (else (cons* (car a) (cadr a)
                             (loop (cddr a) out)))))))))

(define-public pocl-tests-off
  ;; Upstream pocl with only the test phase turned off -- see the module comment for why.
  (package-with-tests-off pocl))
