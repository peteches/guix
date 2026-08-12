(define-module (peteches packages feishin)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bash))

(define-public feishin-bin
  (package
    (name "feishin-bin")
    (version "1.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jeffvli/feishin/releases/download/v"
             version "/Feishin-linux-x86_64.AppImage"))
       (file-name (string-append "feishin-" version "-x86_64.AppImage"))
       (sha256
        (base32 "05b5p7j0q9bskga01aq4m20dw7x661gilba5i4q3zl9rm8byj12x"))))
    (build-system chromium-binary-build-system)
    (native-inputs (list bash-minimal squashfs-tools))
    (arguments
     (list
      #:install-plan
      #~'(("squashfs-root" "share/feishin"))

      #:wrapper-plan
      #~'("squashfs-root/feishin" "squashfs-root/chrome-sandbox"
          "squashfs-root/chrome_crashpad_handler")

      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "sh" "-c"
                      (string-append "set -eu\n"
                       "for off in $(grep -aob 'hsqs' "
                       source
                       " | cut -d: -f1); do\n"
                       "  if unsquashfs -s -o \"$off\" "
                       source
                       " >/dev/null 2>&1; then\n"
                       "    unsquashfs -f -d squashfs-root -o \"$off\" "
                       source
                       "\n"
                       "    exit 0\n"
                       "  fi\n"
                       "done\n"
                       "echo 'failed to find AppImage SquashFS offset' >&2
"
                       "exit 1\n"))))

          (add-after 'install 'install-feishin-launcher
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (appdir (string-append out "/share/feishin")))
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/feishin")
                  (lambda (port)
                    (format port
                     "#!~a
export LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH\"
cd ~a
exec ~a/feishin \"$@\"
"
                     #$(file-append bash-minimal "/bin/sh")
                     appdir
                     appdir
                     appdir)))
                (chmod (string-append bin "/feishin") #o755)))))))
    (home-page "https://github.com/jeffvli/feishin")
    (synopsis "Music player for Jellyfin, Navidrome, and Subsonic servers")
    (description
     "Feishin is a music player and library browser for self-hosted music
servers that implement the Jellyfin, Navidrome, or OpenSubsonic APIs.
Binary package distributed upstream as an AppImage.")
    (license license:gpl3)))
