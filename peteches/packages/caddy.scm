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
;; changelog to follow, and it moved again on 2026-11-05: the pinned 2.11.4 hash
;; failed with `hash mismatch for store item`, and `go version -m` on the
;; downloaded bundle showed it was built from github.com/caddyserver/caddy/v2
;; v2.11.8 + github.com/caddy-dns/desec v1.1.0 — so the =version= field here is
;; only as true as the endpoint happens to be, and the hash has to be re-chased
;; whenever the bundle changes. Verify the bundle's real contents before
;; accepting a new hash, and do it this way, because ordinary =strings= output is
;; full of Go dependency tags that look like versions:
;;   go version -m /gnu/store/<hash>-caddy | grep caddyserver/caddy
;; TODO: migrate to an xcaddy source build (go-build-system,
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
    (version "2.11.8")
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
        (base32 "1c39s88p444a4syb7aq7ssb23z2shqn467w7bfidk262ybam8wkb"))))
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
