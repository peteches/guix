;; peteches/packages/radarr.scm — Radarr movie automation.
;;
;; Self-contained .NET app.  Pattern mirrors peteches/packages/jellyfin.scm.

(define-module (peteches packages radarr)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages icu4c))

(define-public radarr
  (package
    (name "radarr")
    (version "6.1.1.10360")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Radarr/Radarr/releases/download/v"
             version "/Radarr.master." version ".linux-core-x64.tar.gz"))
       (sha256 (base32 "1pbp9lk665rdvv5grnp6inl3cin6111y77wraq46avi0fpwg859r"))))
    (build-system copy-build-system)
    (inputs
     (list (list "glibc"   glibc)
           (list "gcc:lib" gcc "lib")
           (list "openssl" openssl)
           (list "icu4c"   icu4c)))
    (native-inputs (list patchelf))
    (arguments
     (list
      #:install-plan
      #~'(("." "share/radarr/"))
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
                     (pe       (search-input-file inputs "/bin/patchelf"))
                     (loader   (string-append libc "/lib/ld-linux-x86-64.so.2"))
                     (app-dir  (string-append out "/share/radarr"))
                     (libpath  (string-join
                                (list app-dir
                                      (string-append libc "/lib")
                                      (string-append gcc-lib "/lib")
                                      (string-append ssl "/lib")
                                      (string-append icu "/lib"))
                                ":"))
                     (real     (string-append app-dir "/Radarr"))
                     (wrapper  (string-append out "/bin/radarr")))
                (chmod real #o755)
                (invoke pe "--set-interpreter" loader "--set-rpath" libpath real)
                (mkdir-p (string-append out "/bin"))
                (call-with-output-file wrapper
                  (lambda (port)
                    (format port
                            "#!/bin/sh\nexport LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:}${LD_LIBRARY_PATH}\"\nexec ~a \"$@\"\n"
                            libpath real)))
                (chmod wrapper #o755)))))))
    (home-page "https://radarr.video/")
    (synopsis "Movie collection manager for Usenet and BitTorrent")
    (description
     "Radarr is a movie collection manager for Usenet and BitTorrent users.
It monitors for new releases, grabs, sorts, and renames movies, optionally
passing them to a download client.")
    (license gpl3+)))
