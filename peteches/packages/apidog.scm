(define-module (peteches packages apidog)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages java)
  #:use-module (gnu packages node))

(define-public apidog
  (package
    (name "apidog")
    (version "2.8.35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://file-assets.apidog.com/download/" version "/Apidog-"
             version ".AppImage"))
       (file-name (string-append "apidog-" version ".AppImage"))
       (sha256
        (base32 "1k785dzfl7qc9amhhhv5ndvnjx4k33mdjcdmf7ry3g9izwq9z7w3"))))
    (build-system chromium-binary-build-system)
    (native-inputs (list bash-minimal squashfs-tools))
    ;; Apidog's bundled DB-script/mock-server features shell out to "java"
    ;; and "node" from PATH (see resources/app.asar.unpacked/dist/assets/
    ;; JarExecuter-*.jar and the oracledb/ibm_db/kerberos node_modules
    ;; inside the AppImage); chromium-binary-build-system's install-wrapper
    ;; phase puts every input's bin/ on PATH automatically.
    (inputs (list openjdk node))
    (arguments
     (list
      #:install-plan
      #~'(("squashfs-root" "share/apidog"))

      #:wrapper-plan
      #~'("squashfs-root/apidog" "squashfs-root/chrome-sandbox"
          "squashfs-root/chrome_crashpad_handler")

      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              ;; AppImages are an ELF stub followed by a SquashFS image;
              ;; locate the "hsqs" magic and unpack it directly rather than
              ;; running the AppImage (which needs FUSE).
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
                       "echo 'failed to find AppImage SquashFS offset' >&2\n"
                       "exit 1\n"))))

          (add-after 'install 'install-apidog-launcher
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (appdir (string-append out "/share/apidog")))
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/apidog")
                  (lambda (port)
                    (format port
                     "#!~a
export LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH\"
cd ~a
exec ~a/apidog \"$@\"
"
                     #$(file-append bash-minimal "/bin/sh")
                     appdir
                     appdir
                     appdir)))
                (chmod (string-append bin "/apidog") #o755))))

          (add-after 'install-apidog-launcher 'install-desktop-entry
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (apps (string-append out "/share/applications"))
                     (icons
                      (string-append
                       out "/share/icons/hicolor/512x512/apps")))
                (mkdir-p apps)
                (call-with-output-file
                    (string-append apps "/apidog.desktop")
                  (lambda (port)
                    (format port
                     "[Desktop Entry]
Name=Apidog
Comment=Design. Debug. Test. Document. Mock. Build APIs Faster & Together.
Exec=~a/bin/apidog %U
Icon=apidog
Type=Application
StartupNotify=true
Categories=Development;Utility;
StartupWMClass=apidog
MimeType=x-scheme-handler/apidog
"
                     out)))
                (mkdir-p icons)
                (copy-file (string-append out "/share/apidog/apidog.png")
                           (string-append icons "/apidog.png")))))) ))
    (home-page "https://apidog.com")
    (synopsis "All-in-one API design, testing, and documentation platform")
    (description
     "Apidog is a desktop application for designing, debugging, testing,
mocking, and documenting APIs, aimed at replacing separate tools like
Postman and Swagger with a single collaborative workflow.  This package
repackages the officially distributed Linux AppImage build; it is
proprietary software.")
    (license #f)))
