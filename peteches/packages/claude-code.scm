(define-module (peteches packages claude-code)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf))

(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.235")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://registry.npmjs.org/@anthropic-ai/claude-code-linux-x64/-/claude-code-linux-x64-"
             version ".tgz"))
       (sha256
        (base32 "1rq4cdvlwl6nmzbafvcaq1146mhh0cgfgkqshansbwxfxrrv4l4f"))))
    (build-system copy-build-system)
    (native-inputs (list patchelf))
    (arguments
     (list
      #:install-plan ''(("claude" "lib/claude-bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'strip)
          (delete 'validate-runpath)
          (add-after 'install 'patchelf-and-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (libc (assoc-ref inputs "glibc"))
                     (pe (search-input-file inputs "/bin/patchelf"))
                     (interp (string-append libc "/lib/ld-linux-x86-64.so.2"))
                     (libdir (string-append libc "/lib"))
                     (real (string-append out "/lib/claude-bin/claude"))
                     (bin (string-append out "/bin/claude")))
                (chmod real #o755)
                ;; Only the interpreter is patched onto the ELF itself.
                ;; This binary is a Bun-compiled single-executable app with
                ;; an embedded payload; `patchelf --set-rpath` on it corrupts
                ;; that payload and the result segfaults on startup (verified
                ;; by bisecting patchelf flags against the built binary).
                ;; LD_LIBRARY_PATH in the wrapper covers glibc discovery
                ;; instead, same as the original --library-path invocation.
                (invoke pe "--set-interpreter" interp real)
                (mkdir-p (string-append out "/bin"))
                ;; Plain exec of the already-patched ELF (not ld.so with the
                ;; real binary as an argument) so /proc/self/exe still
                ;; resolves to a directly-runnable executable — claude's own
                ;; shell-integration shims and `--bg`/daemon spawn rely on
                ;; self-locating via /proc/self/exe.
                (call-with-output-file bin
                  (lambda (port)
                    (format port
                     "#!/bin/sh
export DISABLE_AUTOUPDATER=1
export LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:}${LD_LIBRARY_PATH}\"
exec ~a \"$@\"
"
                     libdir real)))
                (chmod bin #o755)))))))
    (inputs (list glibc))
    (supported-systems '("x86_64-linux"))
    (synopsis "Claude Code — Anthropic's AI coding assistant CLI")
    (description
     "Claude Code is a command-line tool that integrates Claude, Anthropic's AI
assistant, directly into your terminal.")
    (home-page "https://github.com/anthropics/claude-code")
    (license (license:non-copyleft
              "https://github.com/anthropics/claude-code/blob/main/README.md"
              "Anthropic proprietary license"))))
