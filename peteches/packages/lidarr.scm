;; peteches/packages/lidarr.scm — Lidarr music automation.
;;
;; Self-contained .NET app.  Pattern mirrors peteches/packages/sonarr.scm.

(define-module (peteches packages lidarr)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:select (gpl3+))
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages sqlite))

(define-public lidarr
  (package
    (name "lidarr")
    (version "3.1.0.4875")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Lidarr/Lidarr/releases/download/v" version
             "/Lidarr.master." version ".linux-core-x64.tar.gz"))
       (sha256
        (base32 "199gpi9xwrk8wfj7xadjbpdzskjhmr6m8wkbw0mdw25rqap7bw90"))))
    (build-system copy-build-system)
    (inputs (list (list "glibc" glibc)
                  (list "gcc:lib" gcc "lib")
                  (list "openssl" openssl)
                  (list "icu4c" icu4c)
                  (list "zlib" zlib)
                  (list "sqlite" sqlite)))
    (native-inputs (list patchelf))
    (arguments
     (list
      #:install-plan
      #~'(("." "share/lidarr/"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'validate-runpath)
          (delete 'strip)
          (delete 'make-dynamic-linker-cache)
          (add-after 'install 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (libc (assoc-ref inputs "glibc"))
                     (gcc-lib (assoc-ref inputs "gcc:lib"))
                     (ssl (assoc-ref inputs "openssl"))
                     (icu (assoc-ref inputs "icu4c"))
                     (zlib (assoc-ref inputs "zlib"))
                     (sqlite (assoc-ref inputs "sqlite"))
                     (pe (search-input-file inputs "/bin/patchelf"))
                     (loader (string-append libc "/lib/ld-linux-x86-64.so.2"))
                     (app-dir (string-append out "/share/lidarr"))
                     (libpath (string-join (list app-dir
                                                 (string-append libc "/lib")
                                                 (string-append gcc-lib "/lib")
                                                 (string-append ssl "/lib")
                                                 (string-append icu "/lib")
                                                 (string-append zlib "/lib")
                                                 (string-append sqlite "/lib"))
                                           ":"))
                     (real (string-append app-dir "/Lidarr"))
                     (wrapper (string-append out "/bin/lidarr")))
                (chmod real #o755)
                (invoke pe
                        "--set-interpreter"
                        loader
                        "--set-rpath"
                        libpath
                        real)
                (mkdir-p (string-append out "/bin"))
                (call-with-output-file wrapper
                  (lambda (port)
                    (format port
                     "#!/bin/sh
export LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:}${LD_LIBRARY_PATH}\"
exec ~a \"$@\"
"
                     libpath real)))
                (chmod wrapper #o755)))))))
    (home-page "https://lidarr.audio/")
    (synopsis "Music collection manager for Usenet and BitTorrent")
    (description
     "Lidarr is a music collection manager for Usenet and BitTorrent users.
It monitors artists for new releases, grabs, sorts, and renames albums,
optionally passing them to a download client.")
    (license gpl3+)))
