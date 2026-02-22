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
  #:use-module (gnu packages elf)       ; patchelf
  #:use-module (gnu packages bash))


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
   (inputs
    (list glibc
          mesa
          libx11
          fontconfig
          freetype
	  bash
          (list gcc "lib")))
   (arguments
    (list
     ;; Vendor ELF: don't strip, and runpath validation is irrelevant because
     ;; the executable users run is a wrapper script.
     #:strip-binaries? #f
     #:validate-runpath? #f

     #:install-plan
     #~'(("." "share/gcs/"))		; keep full tarball payload

     #:phases
     #~(modify-phases %standard-phases
		      (add-after 'install 'make-launcher
				 (lambda* (#:key outputs inputs #:allow-other-keys)
				   (define (libdir so)
				     (dirname (search-input-file inputs (string-append "/lib/" so))))

				   (define (root-of-lib so)
				     (dirname (libdir so))) ; parent of /lib

				   (let* ((out   (assoc-ref outputs "out"))
					  (real  (string-append out "/share/gcs/gcs"))
					  (bin   (string-append out "/bin/gcs"))
					  (bash  (search-input-file inputs "/bin/bash"))
					  (ldso  (search-input-file inputs "/lib/ld-linux-x86-64.so.2"))

					  ;; Find actual lib directories by locating specific SONAME files.
					  (glibc-lib  (libdir "libc.so.6"))
					  (mesa-lib   (libdir "libGL.so.1"))
					  (x11-lib    (libdir "libX11.so.6"))
					  (fc-lib     (libdir "libfontconfig.so.1"))
					  (ft-lib     (libdir "libfreetype.so.6"))
					  (stdcxx-lib (libdir "libstdc++.so.6"))

					  (fc-root (root-of-lib "libfontconfig.so.1"))
					  (libpath (string-append
						    glibc-lib ":"
						    mesa-lib ":"
						    x11-lib ":"
						    fc-lib ":"
						    ft-lib ":"
						    stdcxx-lib)))

				     (mkdir-p (string-append out "/bin"))
				     (call-with-output-file bin
				       (lambda (p)
					 (format p "#!~a\n" bash)
					 (format p "export FONTCONFIG_PATH=~a/etc/fonts\n" fc-root)
					 (format p "export FONTCONFIG_FILE=~a/etc/fonts/fonts.conf\n" fc-root)
					 (format p "exec ~s --library-path ~s ~s \"$@\"\n"
						 ldso libpath real)))
				     (chmod bin #o555)
				     #t))))))
   (synopsis "GURPS Character Sheet (GCS)")
   (description "GURPS Character Sheet (GCS) is a standalone character editor for GURPS 4e.")
   (home-page "https://gurpscharactersheet.com/")
   (license license:mpl2.0)))
gurpscharactersheet
