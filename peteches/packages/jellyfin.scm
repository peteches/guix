;; peteches/packages/jellyfin.scm — Jellyfin media server + custom FFmpeg.
;;
;; jellyfin-ffmpeg: portable tar.xz with only glibc deps; fixed via patchelf.
;;
;; jellyfin: self-contained .NET 9 app.  The main binary (74 KiB) links
;; libstdc++, libgcc_s, and glibc.  Bundled native libs (libcoreclr.so etc.)
;; embed RUNPATH "$ORIGIN/netcoredeps" which does not exist in the tarball,
;; so they fall back to LD_LIBRARY_PATH.  libSystem.Security.Cryptography.
;; Native.OpenSsl.so uses dlopen at runtime and likewise relies on
;; LD_LIBRARY_PATH to find libssl.  The wrapper sets both --library-path
;; (initial ELF loading) and LD_LIBRARY_PATH (runtime dlopen) to cover both.

(define-module (peteches packages jellyfin)
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

;;; ── jellyfin-ffmpeg ──────────────────────────────────────────────────────

(define-public jellyfin-ffmpeg
  (package
    (name "jellyfin-ffmpeg")
    (version "7.1.4-1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jellyfin/jellyfin-ffmpeg/releases/download/v"
             version "/jellyfin-ffmpeg_" version "_portable_linux64-gpl.tar.xz"))
       (sha256 (base32 "1hk4axd23430mmkj0cjbn2xjn2p94q9mcagq82b6wb1c4wldv5g1"))))
    (build-system copy-build-system)
    (inputs (list glibc))
    (native-inputs (list patchelf))
    (arguments
     (list
      #:install-plan
      #~'(("ffmpeg"  "bin/jellyfin-ffmpeg")
          ("ffprobe" "bin/ffprobe"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patchelf-fix
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out    (assoc-ref outputs "out"))
                     (libc   (assoc-ref inputs "glibc"))
                     (pe     (search-input-file inputs "/bin/patchelf"))
                     (interp (string-append libc "/lib/ld-linux-x86-64.so.2"))
                     (rpath  (string-append libc "/lib")))
                (for-each (lambda (f)
                            (chmod f #o755)
                            (invoke pe "--set-interpreter" interp
                                    "--set-rpath" rpath f))
                          (list (string-append out "/bin/jellyfin-ffmpeg")
                                (string-append out "/bin/ffprobe")))))))))
    (home-page "https://github.com/jellyfin/jellyfin-ffmpeg")
    (synopsis "Custom FFmpeg build for Jellyfin with hardware acceleration")
    (description
     "Jellyfin FFmpeg is a custom FFmpeg build with additional patches for
hardware-accelerated transcoding (VAAPI, NVENC, QuickSync) and tone mapping
(OpenCL, CUDA) optimised for use with the Jellyfin media server.")
    (license gpl2+)))

;;; ── jellyfin ─────────────────────────────────────────────────────────────

(define-public jellyfin
  (package
    (name "jellyfin")
    (version "10.11.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://repo.jellyfin.org/files/server/linux/stable/v"
             version "/amd64/jellyfin_" version "-amd64.tar.gz"))
       (sha256 (base32 "1jhaqm5p0dnr2n051j3pwajgmak9c64mji4yan2222321h40a7zj"))))
    (build-system copy-build-system)
    (inputs
     (list (list "glibc"   glibc)
           (list "openssl" openssl)
           (list "gcc:lib" gcc "lib")
           (list "icu4c"   icu4c)))
    (native-inputs (list patchelf))
    (arguments
     (list
      ;; The 105 MiB archive contains many bundled DLLs and native libs;
      ;; validate-runpath and strip would both fail / be meaningless here.
      #:install-plan
      #~'(("." "share/jellyfin/"))
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
                     (app-dir  (string-append out "/share/jellyfin"))
                     (libpath  (string-join
                                (list app-dir
                                      (string-append libc "/lib")
                                      (string-append gcc-lib "/lib")
                                      (string-append ssl "/lib")
                                      (string-append icu "/lib"))
                                ":"))
                     (real     (string-append app-dir "/jellyfin"))
                     (wrapper  (string-append out "/bin/jellyfin")))
                (chmod real #o755)
                (invoke pe "--set-interpreter" loader "--set-rpath" libpath real)
                (mkdir-p (string-append out "/bin"))
                (call-with-output-file wrapper
                  (lambda (port)
                    ;; LD_LIBRARY_PATH covers runtime dlopen calls (libssl, libcrypto).
                    (format port
                            "#!/bin/sh\nexport LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:}${LD_LIBRARY_PATH}\"\nexec ~a \"$@\"\n"
                            libpath real)))
                (chmod wrapper #o755)))))))
    (home-page "https://jellyfin.org/")
    (synopsis "Free Software media server")
    (description
     "Jellyfin is a Free Software media system that lets you control and
stream your media.  It is an alternative to proprietary services such as
Emby and Plex, providing media from a dedicated server to end-user devices
via a web interface or native clients.")
    (license lgpl2.1+)))
