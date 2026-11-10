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
;;   * Vision (images) is NOT available from the engine package this service
;;     runs: upstream builds the picture reader (`tools/vision`, llama.cpp's
;;     mtmd + the model's mmproj) for CUDA, HIP and Apple only —
;;     `packaging/build_sycl_linux.sh` has no vision target, and
;;     `packaging/ENGINE-COMPONENT.md` packages the reader only "when the
;;     build has one" — so the linux-x86_64/sycl engine folder holds a single
;;     program (`strata`) and `serve/server.py`'s Vision has no reader to
;;     spawn.  `strata-vision` below rebuilds that CPU reader from the same
;;     pin `setup.py` uses; see that definition.
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
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (peteches packages oneapi)
  #:export (intel-igc-core libigdgmm intel-level-zero-gpu strata
            strata-llama-cpp-src strata-vision strata-sycl-engine))

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
    (version "0.1.59.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Niko1221/Strata/releases/download/v0.2.13/"
             "strata-control-" version "-linux-x86_64.tar.gz"))
       (sha256
        (base32
         "1b12q0zqxirqdzz8iiplwdivpfq5ajrjjc32f88gl06g90l63yvk"))))
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

;;; ── picture reader (strata-vision) ────────────────────────────────────────
;;
;; Strata's multimodal path needs a second program beside the engine:
;; `serve/server.py`'s `Vision` spawns the reader (`ENC <image> <out>` lines
;; in, embedding files out, results cached by image hash) and the engine reads
;; those files (`GENI` lines) — so the reader's backend need not match the
;; engine's, and `"gpu": true` in the `vision` sub-config is what adds
;; `--gpu` to its arguments (`Vision.reader_args`).
;;
;; Upstream never builds a Linux CPU reader: `packaging/` has
;; `build_vision_mac.sh` and `build_vision_windows.bat`, the CUDA build
;; script builds one (its release notes say "engine with the CUDA picture
;; reader"), and `packaging/build_sycl_linux.sh` has no vision target at all.
;; `packaging/ENGINE-COMPONENT.md` then packages the reader only "when the
;; build has one", so the engine folder this service runs
;; (0.1.57-linux-x86_64-sycl) holds one program and vision cannot start.
;; Rebuild the reader here from `tools/vision`, CPU-only (`STRATA_PORTABLE=ON`
;; is upstream's default: AVX2 baseline, OpenMP off, so it is not tuned to the
;; building PC and needs no GPU runtime).  `tools/vision` links
;; llama.cpp's `mtmd`/`llama` targets, which must be the same pinned checkout
;; the engine's ggml came from: `setup.py`'s `get_llama_cpp()` names
;; @code{LLAMA_CPP_COMMIT}, downloaded as a zip "as a zip: no git", with
;; @code{tools/ui} skipped — mirrored below.

(define %strata-vision-release-tag "v0.2.13")     ; app tag; the Terminal inside it is 0.1.59.1
(define %strata-vision-llama-commit
  "3cf03257f219afbe7334045ff7c6a06ac68c627d")   ; setup.py LLAMA_CPP_COMMIT at that tag

(define-public strata-llama-cpp-src
  (package
    (name "strata-llama-cpp-src")
    (version %strata-vision-llama-commit)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ggml-org/llama.cpp/archive/"
                           version ".zip"))
       (file-name (string-append "llama.cpp-" version ".zip"))
       (sha256
        (base32
         "0yx43v1c2z1cykzm4ri5v44ldv7wa6700gxvga9x5sl289ckrqnb"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((zip (assoc-ref %build-inputs "source"))
                 (out (assoc-ref %outputs "out"))
                 (unzip (string-append (assoc-ref %build-inputs "unzip")
                                       "/bin/unzip"))
                 (cp (string-append (assoc-ref %build-inputs "coreutils")
                                    "/bin/cp"))
                 (top (string-append "llama.cpp-" #$version)))
            ;; Skip llama.cpp's web UI: setup.py skips it for the same
            ;; reason (its deep paths broke Windows MAX_PATH, #206) and
            ;; nothing here uses it.  -x must follow the archive name, and
            ;; the build environment has no PATH, so copy the tree with
            ;; coreutils' cp by store path (same as the .deb builders).
            (invoke unzip "-q" zip "-x" "*/tools/ui/*")
            (mkdir-p out)
            (invoke cp "-a" (string-append top "/.") out)
            #t))))
    (native-inputs (list coreutils unzip))
    (home-page "https://github.com/ggml-org/llama.cpp")
    (synopsis "Pinned llama.cpp source for Strata's picture reader")
    (description
     "llama.cpp at the commit Strata pins in @code{setup.py}
(@code{get_llama_cpp}: @code{ggml} for the build, @code{gguf-py} for the
tools, @code{mtmd} for images), unpacked without its web UI.  Consumed
only as @code{-DLLAMA_DIR} by @code{strata-vision}; it installs no
program.")
    (license expat)))

(define-public strata-vision
  (package
    (name "strata-vision")
    (version "0.1.59.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Niko1221/Strata/archive/refs/tags/"
             %strata-vision-release-tag ".tar.gz"))
       (file-name (string-append "strata-" version "-source.tar.gz"))
       (sha256
        (base32
         "131yhdgzkazdzf6gq6c21h0mbdx2qf4h3qs3nmq2l0n08vlwafxk"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:generator "Ninja"
      #:build-type "Release"
      #:configure-flags
      #~(list (string-append "-DLLAMA_DIR=" #$strata-llama-cpp-src)
              ;; upstream default: AVX2 baseline, GGML_OPENMP off, no
              ;; AVX-512/AMX — a reader built on one PC runs on another
              ;; (#411 #412 #419: a native build on an AVX-512 machine died
              ;; with an illegal instruction elsewhere)
              "-DSTRATA_PORTABLE=ON")
      #:tests? #f                              ; no ctest suite for this target
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-to-reader
            ;; This Guix has no #:source-dir keyword, and the cmake phase
            ;; takes its source directory from the current directory
            ;; (guix/build/cmake-build-system.scm: abs-srcdir = getcwd), so
            ;; cd into the reader's project folder.
            (lambda _ (chdir "tools/vision")))
          (add-after 'build 'install-reader
            (lambda _
              ;; tools/vision/CMakeLists.txt has no install() rules; the
              ;; target lands in <build>/bin because of
              ;; RUNTIME_OUTPUT_DIRECTORY.
              (let ((hits (find-files "." "^strata-vision$")))
                (mkdir-p (string-append #$output "/bin"))
                (install-file (car hits)
                              (string-append #$output "/bin"))))))))
    (native-inputs (list cmake gcc ninja pkg-config python))
    (home-page "https://github.com/Niko1221/Strata")
    (synopsis "Strata image encoder (llama.cpp mtmd) — CPU picture reader")
    (description
     "The picture reader Strata's server starts for multimodal requests:
its own @code{tools/vision} front end over llama.cpp's @code{mtmd}
library, reading the @code{ENC <image> <out>} line protocol and writing
image embeddings the engine ingests through @code{GENI} lines.  Built
for the CPU: without @code{--gpu} the program puts
@code{CUDA_VISIBLE_DEVICES=-1} before anything reaches a GPU runtime, so
it takes nothing from the card that runs the engine.  Upstream ships a
Linux reader only inside its CUDA engine packages, which is why
@code{(peteches services strata)} could not run vision on the SYCL
engine.")
    (license expat)))

;;; ── SYCL engine from the giridhart fork ─────────────────────────────────
;;;
;; The upstream project's engine packages (strata-engine-...-sycl.tar.gz
;; from its signed release feed) are built from upstream's own SYCL work,
;; which has a non-finite-logits defect: ~25% of generations end in a
;; single-token `!` loop.  The giridhart/Strata fork (branch sycl-b580,
;; commit dfe7e883296c, "sycl: guard non-finite logits in prefill") fixes
;; it in src/prefill/sycl/ (prefill.cpp, gemm.cpp) — fork-specific code
;; that does not exist in upstream v0.2.13.  This package builds that
;; fork's engine component from source with the oneAPI DPC++ compiler
;; (intel-oneapi, peteches/packages/oneapi.scm) and the llama.cpp pin the
;; fork's CMake declares (strata-llama-cpp-src).
;;
;; The output is the engine's component folder (strata binary + its
;; bundled SYCL/MKL-SYCL/UR/Level-Zero libraries + the
;; engine-component.json manifest + BUILD.json), the same layout the
;; project's own packages have; (peteches services strata) drops it into
;; the components directory and pins it (see that module).
(define-public strata-sycl-engine
  (package
    (name "strata-sycl-engine")
    (version "0.1.56")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/giridhart/Strata/archive/"
            "dfe7e883296cd0d52f9f5ae94df36a0578a4794d.tar.gz"))
      (file-name (string-append name "-" version "-fork-src"))
      (sha256
       (base32
        "14yd8cdsz7a115cc90zq74yfaccd1k03rsmban8kxask092zn1fp"))))
    (build-system trivial-build-system)
    (native-inputs
     (list intel-oneapi strata-llama-cpp-src
           cmake ninja binutils patchelf
           glibc (list gcc "lib") zlib
           sed grep coreutils tar bash gzip python-3.12))
    (arguments
     (list
      #:modules '((ice-9 ftw))
      #:builder
      #~(begin
          (use-modules (ice-9 ftw))
          ;; The build sandbox has no (guix build utils), so no mkdir-p /
          ;; invoke.  Use a pure-Guile recursive mkdir and a system-based
          ;; runner that raises on non-zero exit.
          (define (mkdir-p dir)
            (unless (file-exists? dir)
              (let ((parent (dirname dir)))
                (when (and (not (equal? parent dir))
                           (not (file-exists? parent)))
                  (mkdir-p parent))
              (mkdir dir))))
          (define (run cmd)
            (let ((status (system cmd)))
              (unless (zero? status)
                (display (string-append "strata-sycl-engine: command failed (exit "
                                        (number->string status)
                                        "): " cmd)
                         (current-error-port))
                (exit 1))))
          (let* ((src (assoc-ref %build-inputs "source"))
                 (oneapi (assoc-ref %build-inputs "intel-oneapi"))
                 (llama (assoc-ref %build-inputs "strata-llama-cpp-src"))
                 (cmake (assoc-ref %build-inputs "cmake"))
                 (ninja (assoc-ref %build-inputs "ninja"))
                 (binutils (assoc-ref %build-inputs "binutils"))
                 (patchelf (assoc-ref %build-inputs "patchelf"))
                 (glibc (assoc-ref %build-inputs "glibc"))
                 (gcc-lib (assoc-ref %build-inputs "gcc"))
                 (zlib (assoc-ref %build-inputs "zlib"))
                 (sed (assoc-ref %build-inputs "sed"))
                 (grep (assoc-ref %build-inputs "grep"))
                 (coreutils (assoc-ref %build-inputs "coreutils"))
                 (tar (assoc-ref %build-inputs "tar"))
                 (bash (assoc-ref %build-inputs "bash"))
                 (python (assoc-ref %build-inputs "python"))
                 (gzip (assoc-ref %build-inputs "gzip"))
                 (out (assoc-ref %outputs "out"))
                 (extract (string-append out ".extract"))
                 (repo (string-append extract
                                      "/Strata-dfe7e883296cd0d52f9f5ae94df36a0578a4794d"))
                 (engine-out (string-append out ".engine")))
            (setenv "PATH"
                    (string-append
                     gzip "/bin:"
                     cmake "/bin:"
                     ninja "/bin:"
                     binutils "/bin:"
                     patchelf "/bin:"
                     glibc "/bin:"
                     sed "/bin:"
                     grep "/bin:"
                     coreutils "/bin:"
                     bash "/bin:"
                     python "/bin:"
                     (or (getenv "PATH") "")))
            (mkdir-p extract)
            (run (string-append tar "/bin/tar" " xzf " src " -C " extract))
            (chdir repo)
            ;; The script's final smoke test runs the engine under
            ;; `env -i`; the build sandbox has no /lib64, and the engine's
            ;; program interpreter is /lib64/ld-linux-x86-64.so.2, so that
            ;; line can only pass on a real system.  The RUNPATH checks
            ;; (readelf) and the manifest verification (package_engine.py
            ;; --verify, binary_deps.py) stay; the engine is smoke-tested
            ;; at deploy time.
            (run (string-append sed "/bin/sed" " -i \"/env -i /d\" "
                                "packaging/build_sycl_linux.sh"))
            (mkdir-p engine-out)
            ;; glibc's and gcc's lib dirs on LD_LIBRARY_PATH so the
            ;; oneAPI compilers (which link libgcc_s.so.1, absent from the
            ;; sandbox) and `ldd` (collect_libs.py) resolve the system
            ;; libraries; the sourced setvars.sh prepends the oneAPI dirs
            ;; and keeps this value.
            (setenv "LD_LIBRARY_PATH"
                    (string-append glibc "/lib:"
                                   gcc-lib "/lib:"
                                   zlib "/lib:"
                                   (or (getenv "LD_LIBRARY_PATH") "")))
            (setenv "ONEAPI" oneapi)
            (setenv "LLAMA_CPP" llama)
            (setenv "COMMIT" "dfe7e883296c")
            (setenv "OUT" engine-out)
            (setenv "BUILD" (string-append extract "/build-sycl"))
            (setenv "PYTHON" (string-append python "/bin/python3"))
            ;; guix-build: 8 cores but 11 GB RAM; 4 parallel icpx jobs is
            ;; the safe peak.
            (setenv "JOBS" "4")
            ;; guix's binutils `ld` has no default search path and the
            ;; sandbox has no /usr, so the oneAPI drivers' link lines
            ;; (bare crti.o, -lc, -lm, -lgcc, ...) find nothing.  Point
            ;; CMake's per-language flags (inherited by every compile and
            ;; link step; the build script never overrides them) at the
            ;; glibc and gcc lib dirs.  glibc 2.41 ships only the
            ;; versioned stubs (libdl.so.2, libpthread.so.0, librt.so.1,
            ;; libutil.so.1), so -ldl/-lpthread/-lrt/-lutil need
            ;; unversioned symlinks in a writable shim dir.
            (let ((shim (string-append extract "/link-shim")))
              (mkdir-p shim)
              (run (string-append
                    coreutils "/bin/ln -sf " glibc "/lib/libdl.so.2 " shim "/libdl.so; "
                    coreutils "/bin/ln -sf " glibc "/lib/libpthread.so.0 " shim "/libpthread.so; "
                    coreutils "/bin/ln -sf " glibc "/lib/librt.so.1 " shim "/librt.so; "
                    coreutils "/bin/ln -sf " glibc "/lib/libutil.so.1 " shim "/libutil.so"))
              (let ((link-dirs
                     (string-append "-L" glibc "/lib "
                                    "-L" gcc-lib "/lib/gcc/x86_64-unknown-linux-gnu/14.3.0 "
                                    "-L" gcc-lib "/lib "
                                    "-L" shim)))
                (setenv "CMAKE_C_FLAGS_INIT" link-dirs)
                (setenv "CMAKE_CXX_FLAGS_INIT" link-dirs)))
            (run (string-append bash "/bin/sh" " packaging/build_sycl_linux.sh"))
            ;; The script wrote $OUT/<component folder>; move the folder's
            ;; contents to $out.  The sandbox guile has no scandir, so do
            ;; the single-folder check with a shell one-liner.
            (run (string-append
                  "n=$(ls -1d " engine-out "/*/ 2>/dev/null | wc -l); "
                  "if [ \"$n\" -eq 1 ]; then "
                  "f=$(ls -1d " engine-out "/*/); "
                  "f=${f%/}; "
                  coreutils "/bin/cp -a \"$f/.\" " out "; "
                  "else "
                  "echo \"strata-sycl-engine: unexpected OUT layout\" >&2; "
                  "ls -1d " engine-out "/*/; "
                  "exit 1; "
                  "fi"))
            #t))))
    (home-page "https://github.com/giridhart/Strata")
    (synopsis
     "Strata SYCL engine (giridhart fork) with the non-finite-logits fix")
    (description
     "The Strata inference engine for Intel Arc SYCL, built from the
@code{giridhart/Strata} fork (@code{sycl-b580}, commit
@code{dfe7e883296c}) with the oneAPI DPC++ compiler.  The fork fixes the
non-finite-logits defect in upstream's SYCL prefill path — the ~25%
single-token @code{!} loop rate — in @code{src/prefill/sycl/}.  The
output is the engine's component folder: the @command{strata} binary with
its bundled SYCL, MKL-SYCL, Unified Runtime and Level Zero libraries, the
@file{engine-component.json} manifest and @file{BUILD.json}, the same
layout the project's own engine packages have.")
    (license expat)))
