(define-module (peteches packages herdr-mx)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

;; herdr-mx (2lab-ai/herdr-mx) is a community downstream distribution of
;; herdr that adds a multi-remote client -- attaching to several herdr
;; servers from one sidebar -- ahead of upstream shipping it natively (see
;; https://github.com/herdrdev/herdr/discussions/515). Installed as
;; `bin/herdr' so it is a drop-in replacement for the upstream package;
;; only wired into the desktop configs (nug/nyarlothotep) that act as
;; multi-remote clients. The claude-workstation VM accounts keep the
;; standard `herdr' package from (peteches packages herdr) as their
;; server, since they are attached *to*, not multi-remote clients.
(define-public herdr-mx
  (let ((pkg-version "0.8.0-mx.1"))
    (package
      (name "herdr-mx")
      (version pkg-version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://github.com/2lab-ai/herdr-mx/releases/download/v"
               pkg-version "/herdr-linux-x86_64"))
         (sha256
          (base32 "07q281lqa1lq0m2kn4d1vs4w4ms0kzdgih8mv4782ii55azbxqjz"))))
      (build-system copy-build-system)
      (arguments
       (list
        ;; static-pie linked, like upstream herdr -- no patchelf needed.
        #:install-plan
        #~(list (list "herdr-linux-x86_64" "bin/herdr"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'make-herdr-executable
              (lambda* (#:key outputs #:allow-other-keys)
                (chmod (string-append (assoc-ref outputs "out") "/bin/herdr")
                       #o555))))))
      (supported-systems '("x86_64-linux"))
      (home-page "https://github.com/2lab-ai/herdr-mx")
      (synopsis "Herdr distribution with a multi-remote client")
      (description
       "herdr-mx is a community downstream distribution of herdr, the
persistent terminal runtime for coding agents, adding a multi-remote
client: attach to herdr servers on several machines from one sidebar
instead of nesting SSH sessions.  It tracks upstream herdr closely and
installs as a drop-in @command{herdr} binary.")
      (license asl2.0))))
