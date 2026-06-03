;; peteches/packages/prowlarr.scm — Prowlarr indexer manager.
;;
;; Self-contained .NET app.  The main binary links glibc/libstdc++/libgcc_s
;; and bundled native libs use LD_LIBRARY_PATH for dlopen at runtime.
;; Pattern mirrors peteches/packages/jellyfin.scm.

(define-module (peteches packages prowlarr)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (agpl3+))
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages compression))

(define-public prowlarr
  (package
    (name "prowlarr")
    (version "2.3.5.5327")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Prowlarr/Prowlarr/releases/download/v"
             version "/Prowlarr.master." version ".linux-core-x64.tar.gz"))
       (sha256 (base32 "1l59j81b7rknwszz33767cgy7747xnicp80ckcin0phbz40a5hkh"))))
    (build-system copy-build-system)
    (inputs
     (list (list "glibc"   glibc)
           (list "gcc:lib" gcc "lib")
           (list "openssl" openssl)
           (list "icu4c"   icu4c)
           (list "zlib"    zlib)))
    (native-inputs (list patchelf))
    (arguments
     (list
      #:install-plan
      #~'(("." "share/prowlarr/"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'validate-runpath)
          (delete 'strip)
          (delete 'make-dynamic-linker-cache)
          (add-after 'install 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out      (assoc-ref outputs "out"))
                     (libc     (assoc-ref inputs "glibc"))
                     (gcc-lib  (assoc-ref inputs "gcc:lib"))
                     (ssl      (assoc-ref inputs "openssl"))
                     (icu      (assoc-ref inputs "icu4c"))
                     (zlib     (assoc-ref inputs "zlib"))
                     (pe       (search-input-file inputs "/bin/patchelf"))
                     (loader   (string-append libc "/lib/ld-linux-x86-64.so.2"))
                     (app-dir  (string-append out "/share/prowlarr"))
                     (libpath  (string-join
                                (list app-dir
                                      (string-append libc "/lib")
                                      (string-append gcc-lib "/lib")
                                      (string-append ssl "/lib")
                                      (string-append icu "/lib")
                                      (string-append zlib "/lib"))
                                ":"))
                     (real     (string-append app-dir "/Prowlarr"))
                     (wrapper  (string-append out "/bin/prowlarr")))
                (chmod real #o755)
                (invoke pe "--set-interpreter" loader "--set-rpath" libpath real)
                (mkdir-p (string-append out "/bin"))
                (call-with-output-file wrapper
                  (lambda (port)
                    (format port
                            "#!/bin/sh\nexport LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:}${LD_LIBRARY_PATH}\"\nexec ~a \"$@\"\n"
                            libpath real)))
                (chmod wrapper #o755)))))))
    (home-page "https://prowlarr.com/")
    (synopsis "Indexer manager for the *Arr suite")
    (description
     "Prowlarr is an indexer manager and proxy built on the popular *Arr
framework.  It integrates with Sonarr, Radarr, and other apps to manage
Usenet and torrent indexers from a single interface.")
    (license agpl3+)))
