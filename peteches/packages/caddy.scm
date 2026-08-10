;; peteches/packages/caddy.scm — Caddy web server with desec DNS plugin.
;;
;; Caddy is a pure-Go statically-linked binary, so no glibc wrapper is needed.
;; We use the Caddy download API which builds a binary with the requested
;; plugins included.
;;
;; KNOWN ISSUE: the `idempotency' parameter does NOT actually pin the build.
;; Caddy's own maintainer has confirmed the endpoint has no version-pinning
;; and no reliability guarantee (https://caddy.community/t/download-site-url-script/15768).
;; It silently started serving 2.11.4 under a URL/idempotency string that had
;; previously returned 2.11.3, breaking the pinned sha256 with no upstream
;; changelog to follow.  TODO: migrate to an xcaddy source build (go-build-system,
;; pinned Caddy release tag + vendored desec plugin) for real reproducibility.

(define-module (peteches packages caddy)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public caddy
  (package
    (name "caddy")
    (version "2.11.4")
    (source
     (origin
       (method url-fetch)
       ;; Download API builds Caddy with the desec DNS-01 challenge plugin.
       ;; idempotency is just a short-lived build-cache key upstream, not a
       ;; version pin — see the KNOWN ISSUE note above before trusting it.
       (uri (string-append "https://caddyserver.com/api/download"
                           "?os=linux&arch=amd64"
                           "&p=github.com%2Fcaddy-dns%2Fdesec"
                           "&idempotency=2.10.0"))
       (file-name "caddy")
       (sha256
        (base32 "1jw8rlkdd2qm4hb5h203p3bmlhi8477q7n49n7jb1z6jfb79j2x6"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'chmod-binary
            (lambda* (#:key outputs #:allow-other-keys)
              (chmod (string-append (assoc-ref outputs "out") "/bin/caddy")
                     #o755))))
      #:install-plan
      #~'(("caddy" "bin/caddy"))))
    (home-page "https://caddyserver.com/")
    (synopsis
     "Web server with automatic HTTPS and desec DNS-01 challenge plugin")
    (description
     "Caddy is a fast, multi-platform web server with automatic HTTPS.
This build includes the caddy-dns/desec plugin, enabling DNS-01 ACME challenges
via the deSEC API for certificate issuance without exposing port 80 publicly.")
    (license asl2.0)))
