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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages base)
  #:export (intel-oneapi))

(define %oneapi-pool "https://apt.repos.intel.com/oneapi/pool/main")

;; (file name, sha256) for the oneAPI 2025.3 .debs the Strata SYCL build
;; needs: the DPC++ compiler (icpx/icx), oneMKL with the MKL-SYCL
;; libraries, TBB, the dev utilities and UMF.  The names are the
;; pool-relative file names from Intel's apt repository; the hashes were
;; taken from the repository's own index (sha256sum of each .deb).
(define %oneapi-debs
  '(("intel-oneapi-compiler-dpcpp-cpp-2025.3-2025.3.3-30_amd64.deb"
     "1wxbgqxvm9axmrxy4x83lzh5gfrwhp3ab6appbwxq6189zr0happ")
    ("intel-oneapi-compiler-dpcpp-cpp-runtime-2025.3-2025.3.3-30_amd64.deb"
     "10ig2rnrk83nckfnnh80vb04rl5696yg1d6x26h0d8x0pvmmkhbp")
    ("intel-oneapi-compiler-shared-2025.3-2025.3.3-30_amd64.deb"
     "01b78779vy145ck7fsybs2gfgkdx3bk2vknjk9809vndsllgll83")
    ("intel-oneapi-compiler-shared-runtime-2025.3-2025.3.3-30_amd64.deb"
     "1fjfsvhb97bh5rk5zvrrd1r6gaghyxfn6m031cw3mddphja90apa")
    ("intel-oneapi-dev-utilities-2025.3-2025.3.1-15_amd64.deb"
     "0zbclxhr0a29wh2jpk4n9j5m1xbk3wzzdxp88z82mg7di23jgav1")
    ("intel-oneapi-dpcpp-cpp-2025.3-2025.3.3-30_amd64.deb"
     "0glkd160fpipvj3awvlkg1p4dlsvllhf68sgrx6x16a9lcjapycs")
    ("intel-oneapi-dpcpp-debugger-2025.3-2025.3.1-7_amd64.deb"
     "1cidzd02cj9hkrfcmd2c9m8vwwlai47ji7irici075iq3l8yz2z1")
    ("intel-oneapi-libdpstd-devel-2022.10-2022.10.0-275_amd64.deb"
     "0ka786rgzjad8cdd07dgnrrc9lzzg4wpkf6mck4an7zknm9j85xp")
    ("intel-oneapi-mkl-2025.3-2025.3.1-8_amd64.deb"
     "17yfg8m55pcdz4k9dlmy2wvywzkafw4clm82hlm21yzp6r2mhgvw")
    ("intel-oneapi-mkl-classic-2025.3-2025.3.1-8_amd64.deb"
     "186bx11dyj6z4hqil3hhl0yhl9rg9ka1ibja82pkiaqbnf5kjag6")
    ("intel-oneapi-mkl-cluster-2025.3-2025.3.1-8_amd64.deb"
     "0z8f5jcmsr2ic0whwg0z98n3r1yg6mhzv88fyn7k3vvwvcmclj91")
    ("intel-oneapi-mkl-core-2025.3-2025.3.1-8_amd64.deb"
     "0byf5a8v8hzbccsnx8ml7kkdn01vl9xnwv25chh6s555c8348j0n")
    ("intel-oneapi-mkl-sycl-2025.3-2025.3.1-8_amd64.deb"
     "1y4x4vkz7iixljxf5z8g8all8n67k844xqdyzsjhpmqp0nc33nyk")
    ("intel-oneapi-mkl-sycl-blas-2025.3-2025.3.1-8_amd64.deb"
     "1avabq19xyd783ppaqb5qb0isddkxpap1clfxsk18b4216li7d6y")
    ("intel-oneapi-mkl-sycl-data-fitting-2025.3-2025.3.1-8_amd64.deb"
     "1a5cd2kb67nzah16spb821f345v4zzj7vjl7n01qaywf3dhlg6ya")
    ("intel-oneapi-mkl-sycl-dft-2025.3-2025.3.1-8_amd64.deb"
     "1s199pwnaq78x2smdycp5w20in6a64nvg89cwcq33fgqhlk8f9g8")
    ("intel-oneapi-mkl-sycl-lapack-2025.3-2025.3.1-8_amd64.deb"
     "19d0fnkddpx890pivsizyvabbqr34lb53n7b82py724njr5fz5jj")
    ("intel-oneapi-mkl-sycl-rng-2025.3-2025.3.1-8_amd64.deb"
     "01y5c0ppflrw7243vs8m32r0amh5bq6hnjbwx7sghxydrpks0nc5")
    ("intel-oneapi-mkl-sycl-sparse-2025.3-2025.3.1-8_amd64.deb"
     "1bxgh8caa3am69xanmrf4jr0hhqlmx9ijm5s8b3gqbg57hdywq8j")
    ("intel-oneapi-mkl-sycl-stats-2025.3-2025.3.1-8_amd64.deb"
     "1fm9m765hx5105p209n8n454dwsg3c1a1a1f1s9piwjfbxwxfvs2")
    ("intel-oneapi-mkl-sycl-vm-2025.3-2025.3.1-8_amd64.deb"
     "1sgplx9pwlkg2wlq3pbdx3wlbnngj8nfmkwkvsccr6ycgzxadya7")
    ("intel-oneapi-openmp-2025.3-2025.3.3-30_amd64.deb"
     "1vbap2n72as2zm0w49gv869fj388sd5qpbg8742f2bvx38933dcr")
    ("intel-oneapi-tbb-2022.3-2022.3.1-400_amd64.deb"
     "0h86a6yhgyav6j1fdvglbrsqsxzw1xjq946i9v9k0vqd2d8pbb81")
    ("intel-oneapi-tbb-devel-2022.3-2022.3.1-400_amd64.deb"
     "0ir96sjhipq169idb71z0myav73y2sy2xlvfbvd9w31360cml27b")
    ("intel-oneapi-tcm-1.4-1.4.1-445_amd64.deb"
     "0y6fnpbxnm2bvv9nxxp0ia9d5mxam0zd48ravx0rv70zqwgd4bwf")
    ("intel-oneapi-umf-1.0-1.0.3-17_amd64.deb"
     "1612mlvjz7drn93xm51ln1svpyh3d6qp4hd92cbw69nz88zwz4qb")))

(define-public intel-oneapi
  (package
    (name "intel-oneapi")
    (version "2025.3")
    (source
     (origin
      (method
       ;; Download the .debs one by one (each hash-verified) into a
       ;; directory; Guix stores that directory as the origin output.
       (lambda _
         (use-modules (guix build utils) (guix build download)
                      (guix base32))
         (let ((tmp (string-append (or (getenv "TMPDIR") "/tmp")
                                   "/oneapi-debs-"
                                   (number->string (getpid)))))
           (mkdir-p tmp)
           (for-each
            (lambda (row)
              (unless (url-fetch (string-append %oneapi-pool "/" (car row))
                                 (string-append tmp "/" (car row))
                                 #:hashes
                                 (list (cons 'sha256
                                             (nix-base32-string->bytevector
                                              (cadr row)))))
                (throw 'oneapi-deb-download-failed (car row))))
            %oneapi-debs)
           tmp)))
      (uri %oneapi-pool)
      (file-name (string-append name "-" version "-debs"))
      ;; Dummy: the real verification is per-.deb inside the method
      ;; above; this only names the fixed-output derivation.
      (sha256
       (base32
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
    (build-system trivial-build-system)
    (native-inputs (list dpkg coreutils))
    (arguments
     (list
      #:modules '((guix build utils) (ice-9 ftw))
      #:builder
      #~(begin
          (use-modules (guix build utils) (ice-9 ftw))
          (let* ((debs (assoc-ref %build-inputs "source"))
                 (dpkg (string-append (assoc-ref %build-inputs "dpkg")
                                      "/bin/dpkg"))
                 (cp (string-append (assoc-ref %build-inputs "coreutils")
                                    "/bin/cp"))
                 (out (assoc-ref %outputs "out"))
                 (extract (string-append out ".extract")))
            (mkdir-p extract)
            ;; dpkg -x each .deb into one tree: they all install under
            ;; /opt/intel/oneapi, so the extractions merge.
            (for-each
             (lambda (name)
               (invoke dpkg "-x" (string-append debs "/" name) extract))
             (scandir debs (lambda (n) (string-suffix? ".deb" n))))
            (mkdir-p out)
            (invoke cp "-a" (string-append extract "/opt/intel/oneapi/.") out)
            ;; The .deb postinst scripts create the `latest` symlinks;
            ;; dpkg -x does not run them, so make them here (skip the
            ;; ones the package data already carries).
            (for-each
             (lambda (top)
               (let ((topdir (string-append out "/" top)))
                 (when (file-exists? topdir)
                   (for-each
                    (lambda (ver)
                      (let ((latest (string-append topdir "/latest")))
                        (unless (file-exists? latest)
                          (symlink (string-append topdir "/" ver)
                                   latest))))
                    (filter (lambda (n)
                              (file-directory?
                               (string-append topdir "/" n)))
                            (scandir topdir
                                     (lambda (n)
                                       (not (member n (list "." ".."))))))))))
             (scandir out (lambda (n)
                            (file-directory? (string-append out "/" n)))))
            ;; A self-contained setvars.sh: the Strata build script sources
            ;; it and expects icpx (and icx) on PATH afterwards.
            (with-output-to-file (string-append out "/setvars.sh")
              (lambda _
                (display "#!/bin/sh
ONEAPI_ROOT=$(cd \"$(dirname \"$0\")\" && pwd)
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
