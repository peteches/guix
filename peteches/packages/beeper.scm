(define-module (peteches packages beeper)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bash))

(define-public beeper-bin
  (package
    (name "beeper-bin")
    (version "4.3.57")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://beeper-desktop.download.beeper.com/builds/" "Beeper-"
             version "-x86_64.AppImage"))
       (file-name (string-append "beeper-" version "-x86_64.AppImage"))
       (sha256
        (base32 "0nwrfyh36d0yd255jg681frs3c8zps1b8vf423f42s8nhnbiqrbb"))))
    (build-system chromium-binary-build-system)
    (native-inputs (list bash-minimal squashfs-tools))
    (arguments
     (list
      #:install-plan
      #~'(("squashfs-root" "share/beeper"))

      #:wrapper-plan
      #~'("squashfs-root/beepertexts" "squashfs-root/chrome-sandbox"
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

          (add-after 'install 'install-beeper-launcher
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (appdir (string-append out "/share/beeper")))
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/beeper")
                  (lambda (port)
                    (format port
                     "#!~a
export LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH\"
cd ~a
exec ~a/beepertexts \"$@\"
"
                     #$(file-append bash-minimal "/bin/sh")
                     appdir
                     appdir
                     appdir)))
                (chmod (string-append bin "/beeper") #o755)))))))
    (home-page "https://www.beeper.com/")
    (synopsis "Universal messaging desktop app")
    (description
     "Binary package of Beeper Desktop, distributed as an AppImage.")
    (license #f)))
