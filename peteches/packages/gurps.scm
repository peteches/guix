(define-module (peteches packages gurps)
  ;; Core Guix modules
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)

  ;; Runtime bits
  #:use-module (gnu packages base)       ; glibc
  #:use-module (gnu packages gcc)        ; gcc
  #:use-module (gnu packages gl)         ; mesa
  #:use-module (gnu packages xorg)       ; libx11
  #:use-module (gnu packages fontutils)  ; fontconfig, freetype
  #:use-module (gnu packages elf))       ; patchelf

(define-public gurpscharactersheet
  (package
    (name "gurpscharactersheet")
    (version "5.42.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://github.com/richardwilkes/gcs/releases/download/v"
              version "/gcs-" version "-linux-amd64.tgz"))
        (sha256
         (base32 "01zj123w68n4q6zmcvj12jcmzxsdmic3x28yx0ajzbrmmi1rafvp"))))
    (build-system copy-build-system)

    ;; patchelf only needed at build time
    (native-inputs
      (list patchelf))

    ;; Runtime shared libs to satisfy the binary
    (inputs
      (list
       glibc
       mesa
       libx11
       fontconfig
       freetype
       (list gcc "lib")))         ; we’ll look this up as "gcc" below

    (arguments
      (list
       ;; Copy the single 'gcs' binary from the tarball into $out/bin/gcs
       ;; This assumes the tarball unpacks a top-level file named "gcs".
       #:install-plan
       #~'(("gcs" "bin/"))

       #:phases
       #~(modify-phases %standard-phases
           ;; After install, patch interpreter and RPATH on the binary.
           (add-after 'install 'patch-elf
             (lambda* (#:key outputs inputs #:allow-other-keys)
               (let* ((out      (assoc-ref outputs "out"))
                      (bin      (string-append out "/bin/gcs"))
                      ;; Inputs we need for RPATH
                      (glibc    (assoc-ref inputs "glibc"))
                      (mesa     (assoc-ref inputs "mesa"))
                      (libx11   (assoc-ref inputs "libx11"))
                      (fontconf (assoc-ref inputs "fontconfig-minimal"))
                      (freetype (assoc-ref inputs "freetype"))
                      ;; (list gcc "lib") above => key "gcc" here
                      (gcc-lib  (assoc-ref inputs "gcc"))
                      ;; Dynamic linker for x86_64 glibc
                      (interp   (string-append
                                 glibc "/lib/ld-linux-x86-64.so.2"))
                      ;; RPATH covering all needed libs
                      (rpath    (string-append
                                  glibc "/lib:"
                                  mesa "/lib:"
                                  libx11 "/lib:"
                                  fontconf "/lib:"
                                 freetype "/lib:"
                                 gcc-lib "/lib")))
		 (format #t "fontconfig = ~s~%" fontconf)
                 (invoke "patchelf" "--set-interpreter" interp bin)
                 (invoke "patchelf" "--set-rpath" rpath bin)
                 #t))))))

    (synopsis "GURPS Character Sheet (GCS)")
    (description
     "GURPS Character Sheet (GCS) is a standalone, interactive character
editor for the GURPS Fourth Edition roleplaying game.  It provides tools
for creating and maintaining detailed character sheets.")
    (home-page "https://gurpscharactersheet.com/")
    (license license:mpl2.0)))
