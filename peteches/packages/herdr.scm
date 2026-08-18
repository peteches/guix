(define-module (peteches packages herdr)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public herdr
  (let ((pkg-version "0.8.0"))
    (package
      (name "herdr")
      (version pkg-version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://github.com/herdrdev/herdr/releases/download/v"
               pkg-version "/herdr-linux-x86_64"))
         (sha256
          (base32 "0a6dk9p5zczmyg9ga8n60fsbfvgj3cmvdjbshmzb2b7s81zflwmq"))))
      (build-system copy-build-system)
      (arguments
       (list
        ;; static-pie linked, no dynamic loader / NEEDED entries at all --
        ;; unlike claude-code's Bun binary, no patchelf is required.
        #:install-plan
        #~(list (list "herdr-linux-x86_64" "bin/herdr"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'make-herdr-executable
              (lambda* (#:key outputs #:allow-other-keys)
                (chmod (string-append (assoc-ref outputs "out") "/bin/herdr")
                       #o555))))))
      (supported-systems '("x86_64-linux"))
      (home-page "https://herdr.dev/")
      (synopsis "Persistent terminal runtime for coding agents")
      (description
       "Herdr is a background server that provides persistent terminal
sessions for coding agents such as Claude Code and Codex.  Agents keep
running in Herdr's managed terminals across disconnects and machine
restarts, and sessions can be reattached later locally or remotely over
SSH.")
      (license asl2.0))))
