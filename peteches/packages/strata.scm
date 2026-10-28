;; peteches/packages/strata.scm — Strata MoE inference engine (Intel Arc SYCL)
;; and the Intel GPU userspace runtime it needs.
;;
;; Strata (Niko1221/Strata, MIT) is a Mixture-of-Experts inference engine;
;; Qwen3.8-Flash-Next (125B MoE / 14B active) is one of its supported model
;; families.  On the Intel Arc B70 (comfyui VM) it runs through the
;; project's SYCL port:
;;
;;   * The "Terminal" (strata-control tarball) is a self-contained Python
;;     CLI: it ships a private relocatable CPython 3.12.15 (linked against
;;     glibc only, RPATH $ORIGIN/../lib) plus all of its packages.  It
;;     wraps as a plain tarball.  `strata launch` fetches the matching
;;     engine package (strata-engine-...-sycl.tar.gz, ~186 MB — a prebuilt
;;     SYCL engine bundling the oneAPI runtime: SYCL, MKL-SYCL, the
;;     Unified Runtime loader and the Level Zero adapter) from the
;;     project's signed release feed on first use.  That download is left
;;     to the runtime on purpose: the engine is a runtime artifact of the
;;     project's own update mechanism, not a system package.
;;
;;   * The engine binary (and the private CPython) carry
;;     /lib64/ld-linux-x86-64.so.2 as their program interpreter — an FHS
;;     path that does not exist on a pure Guix system.  The service that
;;     runs Strata (peteches services strata) therefore creates a
;;     /lib64 -> <glibc store dir>/lib symlink at activation and sets
;;     LD_LIBRARY_PATH over the store lib dirs the engine's NEEDED system
;;     libraries come from (see that module).
;;
;;   * Per the project's docs/INTEL_ARC.md the system side must provide:
;;       - the Level Zero loader (libze_loader.so.1) — upstream guix's
;;         `level-zero` package, added to the VM's packages directly;
;;       - libOpenCL.so.1 (the engine links the optional OpenCL adapter) —
;;         upstream guix's `pocl`, likewise added directly;
;;       - the Intel GPU compute runtime ("NEO", libze_intel_gpu.so.1),
;;         the IGC compiler core (libigc.so.2 — required by the engine's
;;         SPIR-V JIT path; the compute runtime declares a hard version
;;         constraint on it) and libigdgmm — NOT packaged in guix or
;;         nonguix, so the three packages below wrap Intel's prebuilt .deb
;;         releases (intel/compute-runtime and intel/intel-graphics-
;;         compiler on GitHub; Intel publishes no Guix/Nix packages for
;;         these).
;;
;; .deb files are ar archives (debian-binary, control.tar.*, data.tar.*).
;; The builders below unpack data.tar with ar+tar and copy the library
;; tree with symlinks preserved.  The fetched archive arrives as
;; %build-inputs["source"] (the trivial-build-system convention, cf.
;; guix's kbd-neo package).
(define-module (peteches packages strata)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages autotools)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (intel-igc-core libigdgmm intel-level-zero-gpu strata))

;;; ── .deb library wrapper ────────────────────────────────────────────────
;;;
;; Each of the three Intel packages below fetches a prebuilt .deb and
;; installs its library tree.  LIB-SRC is the library directory inside the
;; deb's data tree; the whole directory is copied into $out/lib with
;; symlinks preserved (their targets are relative, so they keep working).

(define (deb-libraries-builder lib-src)
  #~(begin
      (use-modules (guix build utils))
      ;; The build environment has no PATH: every tool is referenced by its
      ;; store path from %build-inputs.  'invoke' raises on non-zero exit, so
      ;; a failed step fails the build.
      (let* ((deb (assoc-ref %build-inputs "source"))
             (out (assoc-ref %outputs "out"))
             (ar  (string-append (assoc-ref %build-inputs "binutils") "/bin/ar"))
             (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
             (cp  (string-append (assoc-ref %build-inputs "coreutils") "/bin/cp"))
             (sh  (string-append (assoc-ref %build-inputs "bash") "/bin/sh"))
             (zstd (string-append (assoc-ref %build-inputs "zstd") "/bin/zstd"))
             (gzip (string-append (assoc-ref %build-inputs "gzip") "/bin/gzip"))
             (xz   (string-append (assoc-ref %build-inputs "xz") "/bin/xz"))
             (tmp (string-append (getenv "TMPDIR")
                                 "/strata-deb-"
                                 (number->string (getpid)))))
        (mkdir-p tmp)
        (with-directory-excursion tmp
          (invoke ar "x" deb)
          ;; The build environment has no PATH: tar cannot exec 'zstd' by
          ;; name.  Decompress data.tar.{zst,gz,xz} explicitly with
          ;; absolute-path tools, then run plain 'tar xf'.
          (invoke sh "-c"
                  (string-append
                   "set -e\n"
                   "for f in data.tar.zst data.tar.gz data.tar.xz data.tar; do\n"
                   "  [ -f \"$f\" ] || continue\n"
                   "  case \"$f\" in\n"
                   "    *.zst) " zstd " -d -f \"$f\" -o data.tar ;;\n"
                   "    *.gz)  " gzip " -d -f \"$f\" -o data.tar ;;\n"
                   "    *.xz)  " xz   " -d -f \"$f\" -o data.tar ;;\n"
                   "    *)     : ;;\n"
                   "  esac\n"
                   "  break\n"
                   "done\n"
                   "[ -f data.tar ]\n"
                   "tar=" tar "\n"
                   "$tar xf data.tar\n"
                   "[ -d usr ]"))
          (mkdir-p (string-append out "/lib"))
          (invoke cp "-a"
                  (string-append #$lib-src "/.")
                  (string-append out "/lib")))
        (delete-file-recursively tmp)
        #t)))

(define-public intel-igc-core
  (package
    (name "intel-igc-core")
    (version "2.41.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/intel/intel-graphics-compiler/releases/download/v"
             version
             "/intel-igc-core-2_" version "+22716_amd64.deb"))
       (sha256
        (base32
         "0a7nyjm71vwzzliw8762j1dzhz5knvijm4fn0bxa0rdfcfk68vha"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder (deb-libraries-builder "usr/local/lib")))
    (native-inputs (list binutils tar coreutils bash zstd gzip xz))
    (home-page "https://github.com/intel/intel-graphics-compiler")
    (synopsis "Intel GPU compiler core (libigc.so.2) — SPIR-V JIT backend")
    (description
     "Prebuilt Intel Graphics Compiler core, wrapped from Intel's
@command{intel-igc-core-2} .deb release.  The Strata SYCL engine links
@code{libigc.so.2} for its SPIR-V JIT compilation path; the Intel GPU
compute runtime declares a hard version constraint on this package.")
    (license asl2.0)))

(define-public libigdgmm
  (package
    (name "libigdgmm")
    (version "22.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/intel/compute-runtime/releases/download/26.35.39758.10/"
             "libigdgmm12_" version "_amd64.deb"))
       (sha256
        (base32
         "0m2dw0ahdl76jj2hp0p3hq90c9z7r3r1bz2fq5hww4ladqysccb0"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder (deb-libraries-builder "usr/lib/x86_64-linux-gnu")))
    (native-inputs (list binutils tar coreutils bash zstd gzip xz))
    (home-page "https://github.com/intel/compute-runtime")
    (synopsis "Intel GPU memory management library (libigdgmm.so.12)")
    (description
     "Prebuilt libigdgmm, wrapped from Intel's compute-runtime .deb
release.  A runtime dependency of the Intel GPU compute runtime used by
the Strata SYCL engine on Intel Arc GPUs.")
    (license asl2.0)))

(define-public intel-level-zero-gpu
  (package
    (name "intel-level-zero-gpu")
    (version "26.35.39758.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/intel/compute-runtime/releases/download/"
             version
             "/libze-intel-gpu1_" version "-0_amd64.deb"))
       (sha256
        (base32
         "0w44x30280n7dnzgn0jyk2gn5gw498vfq6ymy6xswm9xjldn96n1"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder (deb-libraries-builder "usr/lib/x86_64-linux-gnu")))
    (native-inputs (list binutils tar coreutils bash zstd gzip xz))
    (home-page "https://github.com/intel/compute-runtime")
    (synopsis "Intel GPU compute runtime (Level Zero driver, NEO)")
    (description
     "Prebuilt Intel GPU compute runtime, wrapped from Intel's
@command{libze-intel-gpu1} .deb release.  This is the Level Zero driver
for Intel Arc GPUs (Battlemage and earlier).  The .deb ships only the
driver library — no Level Zero ICD descriptor — so the runtime that uses
it (peteches services strata) writes the ICD json
(@code{{\"library_path\": ...}}) at start and points @env{ZE_ICD_FILE}
at it, the same thing the oneAPI installer does.")
    (license asl2.0)))

(define-public strata
  (package
    (name "strata")
    (version "0.1.52")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Niko1221/Strata/releases/download/v0.2.7/"
             "strata-control-" version "-linux-x86_64.tar.gz"))
       (sha256
        (base32
         "180hh7y7z9389yi7gn6f35882jsahxixi0nznxx2yar35n7wr7rr"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((tarball (assoc-ref %build-inputs "source"))
                 (out (assoc-ref %outputs "out"))
                 (tar (string-append (assoc-ref %build-inputs "tar") "/bin/tar"))
                 (cp  (string-append (assoc-ref %build-inputs "coreutils") "/bin/cp"))
                 (gzip (string-append (assoc-ref %build-inputs "gzip") "/bin/gzip"))
                 (sh   (string-append (assoc-ref %build-inputs "bash") "/bin/sh"))
                 (tmp (string-append (getenv "TMPDIR")
                                     "/strata-"
                                     (number->string (getpid)))))
            (mkdir-p tmp)
            (with-directory-excursion tmp
              ;; No PATH in the build env: decompress explicitly, then plain tar.
              (invoke sh "-c"
                      (string-append gzip " -d -c " tarball " > bundle.tar"))
              (invoke tar "xf" "bundle.tar")
              (delete-file "bundle.tar")
              (invoke cp "-a" "./." out))
            (delete-file-recursively tmp)
            #t))))
    (native-inputs (list tar coreutils bash gzip))
    (home-page "https://github.com/Niko1221/Strata")
    (synopsis "Strata MoE inference engine — self-contained Terminal CLI")
    (description
     "Strata is a Mixture-of-Experts inference engine; Qwen3.8-Flash-Next
is one of its supported model families.  This package is the
self-contained \"Terminal\" release: a private relocatable CPython 3.12
runtime plus the @command{strata} CLI.  @command{strata launch} fetches
the matching prebuilt engine package from the project's signed release
feed on first use (e.g. @code{strata-engine-...-sycl} for the Intel Arc
SYCL port) and downloads model files into @option{--data-dir}.  See
(peteches services strata) for the system service that runs it.")
    (license expat)))