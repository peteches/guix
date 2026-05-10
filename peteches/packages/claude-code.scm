(define-module (peteches packages claude-code)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base))

(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.138")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@anthropic-ai/claude-code-linux-x64/-/claude-code-linux-x64-"
             version ".tgz"))
       (sha256
        (base32 "01rij52ll7ii73kdb8jdywgjgcqzlgrj1kw3b8irid4mi6yb7373"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan ''(("claude" "lib/claude-bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'strip)
          (delete 'validate-runpath)
          (add-after 'install 'make-wrapper
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out  (assoc-ref outputs "out"))
                     (bin  (string-append out "/bin/claude"))
                     (real (string-append out "/lib/claude-bin/claude")))
                (mkdir-p (string-append out "/bin"))
                (call-with-output-file bin
                  (lambda (port)
                    (format port "#!/bin/sh
exec ~a/lib/ld-linux-x86-64.so.2 --library-path ~a/lib ~a \"$@\"\n"
                            #$(file-append glibc)
                            #$(file-append glibc)
                            real)))
                (chmod bin #o755)))))))
    (inputs (list glibc))
    (supported-systems '("x86_64-linux"))
    (synopsis "Claude Code — Anthropic's AI coding assistant CLI")
    (description
     "Claude Code is a command-line tool that integrates Claude, Anthropic's AI
assistant, directly into your terminal.")
    (home-page "https://github.com/anthropics/claude-code")
    (license
     (license:non-copyleft
      "https://github.com/anthropics/claude-code/blob/main/README.md"
      "Anthropic proprietary license"))))

claude-code
