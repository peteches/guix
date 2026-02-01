(define-module (peteches packages dank-material-shell)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module (gnu packages elf)         ; patchelf
  #:use-module (gnu packages base)        ; glibc
  #:use-module (gnu packages gcc)         ; gcc:lib (libstdc++)
  #:use-module (gnu packages compression) ; zlib
  #:use-module (gnu packages tls)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages wm))        ; openssl

(define %dms-version "1.2.3")

(define (system->dms-arch system)
  (match system
    ("x86_64-linux"  "amd64")
    ("aarch64-linux" "arm64")
    (_ (error "dank-material-shell: unsupported system" system))))

(define-public dank-material-shell
  (package
   (name "dank-material-shell")
   (version %dms-version)
   (supported-systems '("x86_64-linux" "aarch64-linux"))
   (source
    (origin
     (method url-fetch)
     (uri (let ((sys (or (%current-target-system) (%current-system))))
            (string-append
             "https://github.com/AvengeMedia/DankMaterialShell/releases/download/v"
             version "/dms-full-" (system->dms-arch sys) ".tar.gz")))
     (sha256
      (base32 "0mq12qci06dkimdzxvyi2mf6a7mn7sb6da3941cchy7vdbpgbls5"))))
   (build-system copy-build-system)

   ;; patchelf runs at build time => native-input
   (native-inputs
    (list patchelf))
   (propagated-inputs
    (list quickshell dgop))


   ;; runtime deps for the ELF to actually start on Guix
   (inputs
    (list glibc
          `(,gcc "lib")
          zlib
          openssl))
   (arguments
 (list
  #:install-plan
  #~'(("dms"         "share/quickshell/dms")
      ("bin/dms"     "bin/dms")
      ("docs"        "share/doc/dank-material-shell/docs")
      ("INSTALL.md"  "share/doc/dank-material-shell/INSTALL.md")
      ("completions/completion.bash"
       "share/bash-completion/completions/dms")
      ("completions/completion.zsh"
       "share/zsh/site-functions/_dms")
      ("completions/completion.fish"
       "share/fish/vendor_completions.d/dms.fish"))

  #:phases
  #~(let ((unpack-root #f))
      (modify-phases %standard-phases

        ;; Some phases apparently chdir into ./bin etc. Force install to
        ;; run from the right place.
        (add-before 'install 'enter-unpack-root
          (lambda _
            (chdir "../")
            #t))))))

   (synopsis "Material Design shell components from DankMaterialShell release tarball")
   (description
    "Installs the DankMaterialShell QML sources, the dms command, shell completion
files, and documentation from the upstream dms-full release tarball.  The dms
binary is patched with the correct dynamic loader and RPATH for Guix.")
   (home-page "https://github.com/AvengeMedia/DankMaterialShell")
   (license license:expat)))
dank-material-shell
