;; peteches/packages/caddy.scm — Caddy web server with desec DNS plugin.
;;
;; Caddy is a pure-Go statically-linked binary, so no glibc wrapper is needed.
;; We use the Caddy download API which builds a binary with the requested
;; plugins included.  The idempotency parameter pins this build reproducibly.

(define-module (peteches packages caddy)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public caddy
  (package
    (name "caddy")
    (version "2.11.3")
    (source
     (origin
       (method url-fetch)
       ;; Download API builds Caddy with the desec DNS-01 challenge plugin.
       ;; The idempotency string pins the exact build; update both version and
       ;; idempotency together and re-run `guix download <url>` for the new hash.
       (uri (string-append
             "https://caddyserver.com/api/download"
             "?os=linux&arch=amd64"
             "&p=github.com%2Fcaddy-dns%2Fdesec"
             "&idempotency=2.10.0"))
       (file-name "caddy")
       (sha256 (base32 "012sz6vm4qz28s5774i5b5ahqpvxgih9d1ldrq2y8dpkgj0l4lrn"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'chmod-binary
            (lambda* (#:key outputs #:allow-other-keys)
              (chmod (string-append (assoc-ref outputs "out") "/bin/caddy")
                     #o755))))
      #:install-plan #~'(("caddy" "bin/caddy"))))
    (home-page "https://caddyserver.com/")
    (synopsis "Web server with automatic HTTPS and desec DNS-01 challenge plugin")
    (description
     "Caddy is a fast, multi-platform web server with automatic HTTPS.
This build includes the caddy-dns/desec plugin, enabling DNS-01 ACME challenges
via the deSEC API for certificate issuance without exposing port 80 publicly.")
    (license asl2.0)))
