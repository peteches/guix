;; peteches/packages/colibri.scm — Colibrì MoE inference engine.
;;
;; Pure C engine (colibri/olmoe/deepseek_v4 binaries) plus a Python launcher
;; (`coli`) that uses only the standard library for `coli serve` — the extra
;; deps upstream lists (numpy/huggingface_hub for `coli convert`, torch/
;; transformers for the oracle, tokenizers/datasets for `coli bench`) are not
;; provided here since the system service only ever runs `coli serve`.
;;
;; Two variants: `colibri-engine' (CPU-only, CUDA=0) is structurally
;; incapable of touching a GPU regardless of runtime flags — useful for any
;; future non-GPU deployment. `colibri-engine-cuda' adds the CUDA=1 backend,
;; built with CUDA_ARCH=sm_89 pinned to the RTX 4090's compute capability
;; (Ada Lovelace) rather than the Makefile's default CUDA_ARCH=native, which
;; probes the actual device via nvcc and would fail (no GPU inside the build
;; sandbox) or silently target the wrong card if this package is ever built
;; elsewhere. nug.scm's colibri-service-type instance is responsible for
;; capping VRAM use (vram-gb) so it can coexist with ComfyUI — see the
;; module comment in peteches/services/colibri.scm.
;;
;; ARCH=x86-64-v3 (portable AVX2 baseline) rather than the Makefile's default
;; ARCH=native: nug builds locally (#:offload-builds? #f) so `native` would
;; happen to be correct today, but a `native` build is tied to whatever
;; machine happens to run the build and silently wrong if that ever changes
;; (offload re-enabled, substitutes served to another host, etc.) — see the
;; Makefile's own comment on x86-64-v3 being "the portable binary to
;; distribute". The i9-14900KS is nowhere near the AVX2 floor, so this costs
;; negligible throughput for a build that keeps working regardless of where it
;; is actually built.

(define-module (peteches packages colibri)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python)
  #:use-module (guix-science-nonfree packages cuda)
  #:export (colibri-engine colibri-engine-cuda))

(define-public colibri-engine
  (package
    (name "colibri-engine")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JustVugg/colibri")
             (commit "8f512fc8c2f48ffa18cd624cd4a5bcaae4a4abfc"))) ;v1.5.0
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pkwii22cbkq6kflrcii6jnhnx88jn8rsgh2jidkq8az10752vj9"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; `make check` wants live-model fixtures and, for some suites, a GPU;
      ;; none of that belongs in a package build.
      #:tests? #f
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              "CUDA=0"
              "ARCH=x86-64-v3")
      #:phases
      #~(modify-phases %standard-phases
          ;; No ./configure script — it's a hand-written Makefile.
          (delete 'configure))))
    ;; `coli` is a Python 3 launcher script; patch-shebangs (a standard
    ;; gnu-build-system phase) needs python3 on PATH to retarget its
    ;; `#!/usr/bin/env python3` shebang at the store interpreter.
    (inputs (list python))
    (home-page "https://github.com/JustVugg/colibri")
    (synopsis
     "Run huge mixture-of-experts language models by streaming experts from disk")
    (description
     "Colibrì is a pure-C, zero-runtime-dependency inference engine for
large Mixture-of-Experts language models.  It treats VRAM, RAM and disk as
one unified memory hierarchy, streaming routed experts from disk on demand
instead of requiring the whole model to be resident, which makes 700B+
parameter models runnable on consumer hardware.  This build provides the
GLM-5.2, OLMoE, and DeepSeek V4 Flash engines and the `coli' launcher, which
auto-detects which of the three a given model directory needs — the
complete set upstream's own `make install' target builds.  Two further
architectures (Inkling, Kimi K3) exist as engine sources in this release but
have no `install' rule upstream, so they are not built here.  This build is
CPU-only: no CUDA, Metal, or Vulkan backend is compiled in.")
    (license license:asl2.0)))

;; See the module comment for why CUDA_ARCH is pinned rather than left at the
;; Makefile's default `native`.
(define-public colibri-engine-cuda
  (package
    (inherit colibri-engine)
    (name "colibri-engine-cuda")
    (arguments
     (substitute-keyword-arguments (package-arguments colibri-engine)
       ((#:make-flags old-flags)
        #~(list (string-append "PREFIX=" #$output)
                "CUDA=1"
                (string-append "CUDA_HOME=" #$cuda)
                "CUDA_ARCH=sm_89"
                "ARCH=x86-64-v3"))))
    (inputs (cons cuda (package-inputs colibri-engine)))
    (synopsis
     (string-append (package-synopsis colibri-engine)
                    " (CUDA-enabled)"))
    (description
     (string-append (package-description colibri-engine)
                    "  This variant is built with CUDA=1 (CUDA_ARCH=sm_89,"
                    " i.e. Ada Lovelace / RTX 4090) so the GLM-5.2 engine can"
                    " use a GPU expert-cache tier; OLMoE and DeepSeek V4"
                    " Flash have no CUDA backend upstream and remain"
                    " CPU-only.  Requires libcuda.so.1 (the NVIDIA driver's"
                    " userspace library) reachable at runtime, e.g. via"
                    " LD_LIBRARY_PATH."))))
