;;; Commentary:
;;;
;; Intel oneAPI DPC++ (icpx/icx) + oneMKL + TBB + UMF, extracted from the
;; official Intel .debs.  Consumed only as a native input by
;; strata-sycl-engine (peteches/packages/strata.scm), which builds the
;; Strata SYCL engine for the Intel Arc B580 with it.
;;;
;;; Code:
(define-module (peteches packages oneapi)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (guix base32)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:export (intel-oneapi))

(define %oneapi-pool "https://apt.repos.intel.com/oneapi/pool/main")

;; (file name, sha256) for the oneAPI 2026.1 .debs the Strata SYCL build
;; needs: the DPC++ compiler (icpx/icx), oneMKL with the MKL-SYCL
;; libraries, TBB, the dev utilities and UMF.  The names are the
;; pool-relative file names from Intel's apt repository; the hashes were
;; taken from the repository's own index (sha256sum of each .deb).
;;
;; Why 2026.1 (upgraded from 2025.3 on 2026-11-11): the giridhart fork's
;; sycl/include/dpct/util.hpp calls
;; sycl::ext::oneapi::experimental::entangle() and chunked_partition<>,
;; the subgroup-partition API oneAPI 2026.x introduced (2025.3 only has
;; the older tangle_group.hpp), so 2025.3 dies with "no member named
;; 'entangle' in namespace sycl::ext::oneapi::experimental".  The fork's
;; packaging/build_sycl_linux.sh documents "icx / icpx 2026.1 or newer"
;; and its dev image builds with 2026.1.
;;
;; Two quirks of Intel's apt repo made the headers look nonexistent (and
;; cost the 2025.3 bundle its build):
;;
;;   1. The compiler HEADERS (include/sycl/sycl.hpp, include/CL/,
;;      syclcompat, ur_api.h) live in the ...-dpcpp-cpp-common deb, which
;;      is `Architecture: all` — listed ONLY in
;;      dists/all/main/binary-all/Packages (not binary-amd64) and named
;;      `..._all.deb`.  Same for ...-compiler-shared-common (67 MB more
;;      headers) and openmp-common.
;;   2. Intel's Packages files put the Filename:/Size:/SHA256: fields
;;      BEFORE the Package:/Version:/Depends: fields of the SAME record,
;;      so a stanza parser keyed on `Package: ... ^$` silently drops them.
;;
;; In the 2026.x generation the MKL base packages (mkl, mkl-devel,
;; mkl-sycl, mkl-classic-devel) are empty metas: the contents moved to
;; mkl-core (libs), mkl-core-devel + mkl-sycl-include + mkl-sycl-devel +
;; mkl-classic-include + mkl-cluster-devel (headers), and the per-domain
;; mkl-sycl-* packages (libs).  The empty metas, the 428 MB
;; dpcpp-debugger (not needed to compile) and the eclipse-cfg /
;; common-vars / common-licensing env packages (this package generates
;; its own setvars.sh) are deliberately NOT fetched.
(define %oneapi-debs
  '(
    ("intel-oneapi-dpcpp-cpp-2026.1-2026.1.1-325_amd64.deb"
     "0qlndzrs3qf0zshcp04wf3fbv5scpmzc8vbkdybb0hzgyv1sm5zs")
    ("intel-oneapi-compiler-dpcpp-cpp-common-2026.1-2026.1.1-325_all.deb"
     "09jdvlznyj8gsvrlfs1dd3q3zvv62zs4614arkwrybzx928fysyj")
    ("intel-oneapi-compiler-dpcpp-cpp-runtime-2026.1-2026.1.1-325_amd64.deb"
     "1706n70gg2p0r5vgkg4bhxvjylg0sxcfkh82pmkrqnpzgkai616z")
    ("intel-oneapi-compiler-shared-2026.1-2026.1.1-325_amd64.deb"
     "16ay1hsfql9n128sgdkgbbscsn016rp4ac2sikh7q6sxhmw980w7")
    ("intel-oneapi-compiler-shared-common-2026.1-2026.1.1-325_all.deb"
     "14b54dcmr7jmhygjd02lhdfvirndh1lr7g7d0a4s77aw55fnffr8")
    ("intel-oneapi-compiler-shared-runtime-2026.1-2026.1.1-325_amd64.deb"
     "0c16zf6hrvrnrrm1pngaf68rbi23k5j2dzdygwzl3w1s4h07giy0")
    ("intel-oneapi-openmp-2026.1-2026.1.1-325_amd64.deb"
     "00mxg1vc6i5y5cxn8dwfqq7ldhnlrjwcfd08zfky2fk1f9fgkzc9")
    ("intel-oneapi-openmp-common-2026.1-2026.1.1-325_all.deb"
     "045qcgh1240b132rbnglh1pm73670q2i31gbjarmikmr3yzp06s3")
    ("intel-oneapi-dev-utilities-2026.0-2026.0.1-16_amd64.deb"
     "1k8z6mjhdzy38lx1v4vhv0an9kmqlkpmpprd25xydfacxfwhpf2h")
    ("intel-oneapi-libdpstd-devel-2022.13-2022.13.0-107_amd64.deb"
     "0dc8099vz52yarsz15cg749zcdlgf3kwkk17j4nrkvzxwi2z7xpc")
    ("intel-oneapi-mkl-core-2026.1-2026.1.0-236_amd64.deb"
     "14xx8yf2vc0kgw5dqh09w30wcdrvi36j0b06f67lzi0azh10q8rv")
    ("intel-oneapi-mkl-core-devel-2026.1-2026.1.0-236_amd64.deb"
     "0hvwrddzc3kp9yzxsf1qjira7r5d25akf16shl06g5ikpcgf4j7p")
    ("intel-oneapi-mkl-sycl-include-2026.1-2026.1.0-236_amd64.deb"
     "0kfgql03mjg1wyrzpacwm4ixy6kpk34m54x0kwpgx2d8zrx7gj1x")
    ("intel-oneapi-mkl-sycl-devel-2026.1-2026.1.0-236_amd64.deb"
     "0zczi2hrrj6h3mp4gf4agb1z535l9fb0a6aszf9hxxg9y17bqn6d")
    ("intel-oneapi-mkl-sycl-blas-2026.1-2026.1.0-236_amd64.deb"
     "10yp5dwjnfv0v27zcj600hkgfkkd9yidm32ajjwf92pnghddas4n")
    ("intel-oneapi-mkl-sycl-data-fitting-2026.1-2026.1.0-236_amd64.deb"
     "09xc5k6i3711n2qngq1iwl1315cmx6akd4hpszvv2nylr80dfhiv")
    ("intel-oneapi-mkl-sycl-dft-2026.1-2026.1.0-236_amd64.deb"
     "0wykb2g34zmqw9c3c2ry6nhdi2sc1wa9772y3v6cl7534p9jv1ii")
    ("intel-oneapi-mkl-sycl-lapack-2026.1-2026.1.0-236_amd64.deb"
     "08friwxr5driqhady3frzfhydsrb2aaa09ssq4qrgcj9bqpq60gp")
    ("intel-oneapi-mkl-sycl-rng-2026.1-2026.1.0-236_amd64.deb"
     "1gvmnbg2696s38vd0zky7dbiqmcb5bxq9ir13pfxwhi05waqwx5h")
    ("intel-oneapi-mkl-sycl-sparse-2026.1-2026.1.0-236_amd64.deb"
     "0ibxw0w56y3kszf4xcwairdm6gy9y0gib22j599asjghqp0rsjba")
    ("intel-oneapi-mkl-sycl-stats-2026.1-2026.1.0-236_amd64.deb"
     "00g12zrxgx94f732486g4k1ik5m3b3994bmw5q7ld93i2iprhqv2")
    ("intel-oneapi-mkl-sycl-vm-2026.1-2026.1.0-236_amd64.deb"
     "1ibhsjf29pkxgas0ciq1w0773glc9mip99h205idg8vy4jd1i8w0")
    ("intel-oneapi-mkl-cluster-2026.1-2026.1.0-236_amd64.deb"
     "1j0iqgsn6kcgszclvc0ac1ihwk5jn3dk0snivpb0nc0ilzkvs5yf")
    ("intel-oneapi-mkl-cluster-devel-2026.1-2026.1.0-236_amd64.deb"
     "1jvm9m79cnkp5qan53wygi2sqbbg2hmjrvqpdhf1cijrz0rv97m4")
    ("intel-oneapi-mkl-classic-include-2026.1-2026.1.0-236_amd64.deb"
     "10rlm8ngr3hqrfm3vnya1xyphmqqq3l6rhimzbgkk0zkv0vkg6jh")
    ("intel-oneapi-tbb-2023.2-2023.2.0-492_amd64.deb"
     "0gf30hci7vl3gpqgmjqrknpxn3dskk4z3zy00hl33qmz2pz6zx67")
    ("intel-oneapi-tbb-devel-2023.2-2023.2.0-492_amd64.deb"
     "0x3ycnvp96ivpdfg68mk9mzqk3s61j2hzzkvbrf0inkzhg8sqz3p")
    ("intel-oneapi-tcm-1.6-1.6.0-178_amd64.deb"
     "1d8rihk99spjx56wjnjny5wn2cyr10831knqz9rf8iqi196gknn5")
    ("intel-oneapi-umf-1.1-1.1.0-340_amd64.deb"
     "0y6fb2wxbhb1nay2jrlbyvwd672ffn2524xl51rgcxic0jbsj1c7")))

;; Each oneAPI .deb is its own package: a fixed-output derivation that
;; fetches the .deb from Intel's official apt pool with its sha256
;; verified, and installs it into the store as a single file.  The
;; assembly package (intel-oneapi, below) takes all of these as native
;; inputs and extracts them.  Using one standard url-fetch origin per
;; .deb is the robust pattern: each is independently cacheable and
;; hash-verified, and there is no custom origin method to get wrong.
(define (make-oneapi-deb-package row index)
  (package
    (name (string-append "intel-oneapi-deb-" (number->string index)))
    (version "2026.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append %oneapi-pool "/" (car row)))
      (sha256 (base32 (cadr row)))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:builder
      #~(begin
          (mkdir %output)
          (let ((src #$(package-source this-package))
                (dst (string-append %output "/" #$(package-name this-package))))
            ;; copy-file with file paths (not ports): the guix-build
            ;; sandbox guile (3.0.9) has a restricted default module in
            ;; which the bytevector port procedures (get-bytevector-n,
            ;; read-byte, ...) are unbound, and its copy-file takes file
            ;; names, not ports.
            (copy-file src dst)))))
    (synopsis "An Intel oneAPI .deb (fetched from Intel's official apt pool)")
    (home-page %oneapi-pool)
    (description
     "A single Intel oneAPI .deb, fetched from Intel's official apt pool
with its sha256 verified.  It is a native input to @command{intel-oneapi},
which extracts it into the oneAPI tree.")
    (license license:asl2.0)))

(define intel-oneapi-deb-packages
  (let loop ((rows %oneapi-debs)
             (i 0)
             (acc '()))
    (if (null? rows)
        (reverse acc)
        (loop (cdr rows)
              (+ i 1)
              (cons (make-oneapi-deb-package (car rows) i)
                    acc)))))

(define-public intel-oneapi
  (package
    (name "intel-oneapi")
    (version "2026.1")
    (source #f)
    (build-system trivial-build-system)
    (native-inputs (append (list dpkg coreutils tar findutils patchelf glibc)
                           intel-oneapi-deb-packages))
    (arguments
     (list
      #:modules '((ice-9 ftw))
      #:builder
      #~(begin
          (use-modules (ice-9 ftw))
          ;; dpkg-deb -x invokes tar as a subprocess by name, and the
          ;; trivial-build environment does not put the inputs' bin dirs
          ;; on PATH, so do it here.
          (setenv "PATH" (string-append (assoc-ref %build-inputs "tar") "/bin:"
                                        (assoc-ref %build-inputs "coreutils") "/bin:"
                                        (or (getenv "PATH") "")))
          (let* ((dpkg-deb (string-append (assoc-ref %build-inputs "dpkg")
                                          "/bin/dpkg-deb"))
                 (cp (string-append (assoc-ref %build-inputs "coreutils")
                                    "/bin/cp"))
                 (out (assoc-ref %outputs "out"))
                 (extract (string-append out ".extract")))
            ;; mkdir -p extract (pure Guile, recursive; the build sandbox
            ;; has no (guix build utils), so no mkdir-p).
            (let mp ((dir extract))
              (unless (file-exists? dir)
                (let ((parent (dirname dir)))
                  (when (and (not (equal? parent dir))
                             (not (file-exists? parent)))
                    (mp parent))
                (mkdir dir))))
            ;; dpkg -x each .deb (from its own input directory) into one
            ;; tree: they all install under /opt/intel/oneapi, so the
            ;; extractions merge.
            (for-each
             (lambda (i)
               (let* ((deb-name (string-append "intel-oneapi-deb-"
                                               (number->string i)))
                      (deb (string-append (assoc-ref %build-inputs deb-name)
                                          "/" deb-name)))
                 (system (string-append dpkg-deb " -x " deb " " extract))))
             (iota #$(length %oneapi-debs)))
            ;; The .deb binaries carry /lib64/ld-linux-x86-64.so.2 as
            ;; their program interpreter — an FHS path that does not
            ;; exist in the Guix build sandbox (or on a pure Guix
            ;; system), so the kernel cannot exec them ("cannot execute:
            ;; required file not found").  Rewrite the interpreter on
            ;; every executable in the extracted tree to glibc's loader.
            ;; The extract tree is in the build directory (writable), so
            ;; patchelf can modify files in place; non-ELF executables
            ;; (shell scripts) fail patchelf and are skipped.
            (system
             (string-append
              (assoc-ref %build-inputs "findutils") "/bin/find " extract "/opt/intel/oneapi "
              "-type f -perm -u+x -print | "
              "while IFS= read -r f; do "
              (assoc-ref %build-inputs "patchelf") "/bin/patchelf --set-interpreter "
              (assoc-ref %build-inputs "glibc") "/lib/ld-linux-x86-64.so.2 \"$f\" 2>/dev/null || true; "
              "done"))
            (mkdir out)
            (system (string-append cp " -a " extract "/opt/intel/oneapi/. " out))
            ;; The .deb postinst scripts create the `latest` symlinks;
            ;; dpkg-deb -x does not run them, so make them here with a
            ;; shell one-liner (the sandbox guile has no scandir, and
            ;; readdir needs a directory port, not a path).
            (system
             (string-append
              "for top in " out "/*/; do "
              "top=${top%/}; "
              "for ver in \"$top\"/*/; do "
              "ver=${ver%/}; "
              "[ -d \"$ver\" ] || continue; "
              "if [ ! -e \"$top/latest\" ]; then "
              "ln -s \"$ver\" \"$top/latest\"; "
              "fi; "
              "done; "
              "done"))
            ;; A self-contained setvars.sh: the Strata build script sources
            ;; it and expects icpx (and icx) on PATH afterwards.
            (with-output-to-file (string-append out "/setvars.sh")
              (lambda _
                (display "#!/bin/sh
ONEAPI_ROOT=${ONEAPI:-$(cd \"$(dirname \"$0\")\" && pwd)}
export ONEAPI_ROOT
export MKLROOT=$ONEAPI_ROOT/mkl/latest
export TBBROOT=$ONEAPI_ROOT/tbb/latest
export PATH=$ONEAPI_ROOT/compiler/latest/bin:$ONEAPI_ROOT/tbb/latest/bin:$ONEAPI_ROOT/mkl/latest/bin:$PATH
export CPATH=$ONEAPI_ROOT/compiler/latest/include:$ONEAPI_ROOT/mkl/latest/include:$ONEAPI_ROOT/tbb/latest/include:$CPATH
export LIBRARY_PATH=$ONEAPI_ROOT/compiler/latest/lib:$ONEAPI_ROOT/compiler/latest/lib/sycl:$ONEAPI_ROOT/mkl/latest/lib:$ONEAPI_ROOT/tbb/latest/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=$ONEAPI_ROOT/compiler/latest/lib:$ONEAPI_ROOT/compiler/latest/lib/sycl:$ONEAPI_ROOT/mkl/latest/lib:$ONEAPI_ROOT/tbb/latest/lib:$LD_LIBRARY_PATH
")))
            #t))))
    (home-page "https://www.intel.com/content/www/us/en/developer/tools/oneapi/")
    (synopsis "Intel oneAPI DPC++ + oneMKL + TBB + UMF, from the official .debs")
    (description
     "The Intel oneAPI components the Strata SYCL engine build needs
(@command{icpx}/@command{icx} from the DPC++ compiler, oneMKL with the
MKL-SYCL libraries, TBB and UMF), extracted from Intel's official .debs
into a self-contained oneAPI root with @file{latest} symlinks and a
self-contained @file{setvars.sh}.  It installs no program of its own; it
exists as a native input to @command{strata-sycl-engine}.")
    (license license:asl2.0)))
