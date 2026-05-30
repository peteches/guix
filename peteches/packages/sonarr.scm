;; peteches/packages/sonarr.scm — Sonarr TV show automation.
;;
;; Self-contained .NET app.  Pattern mirrors peteches/packages/jellyfin.scm.

(define-module (peteches packages sonarr)
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

(define-public sonarr
  (package
    (name "sonarr")
    (version "4.0.17.2952")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Sonarr/Sonarr/releases/download/v"
             version "/Sonarr.main." version ".linux-x64.tar.gz"))
       (sha256 (base32 "0ykp66lbnvnczbdagwwafpcyrkh10vbb39qw89wpjwkx0rr2656m"))))
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
      #~'(("." "share/sonarr/"))
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
                     (app-dir  (string-append out "/share/sonarr"))
                     (libpath  (string-join
                                (list app-dir
                                      (string-append libc "/lib")
                                      (string-append gcc-lib "/lib")
                                      (string-append ssl "/lib")
                                      (string-append icu "/lib"))
                                ":"))
                     (real     (string-append app-dir "/Sonarr"))
                     (wrapper  (string-append out "/bin/sonarr")))
                (chmod real #o755)
                (invoke pe "--set-interpreter" loader "--set-rpath" libpath real)
                (mkdir-p (string-append out "/bin"))
                (call-with-output-file wrapper
                  (lambda (port)
                    (format port
                            "#!/bin/sh\nexport LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:}${LD_LIBRARY_PATH}\"\nexec ~a \"$@\"\n"
                            libpath real)))
                (chmod wrapper #o755)))))))
    (home-page "https://sonarr.tv/")
    (synopsis "TV series automation for Usenet and BitTorrent")
    (description
     "Sonarr is a PVR for Usenet and BitTorrent users.  It monitors multiple
RSS feeds for new TV show episodes, grabs them, sorts and renames them, and
optionally hands them off to download clients.")
    (license gpl3+)))
