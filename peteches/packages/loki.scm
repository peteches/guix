;; peteches/packages/loki.scm — Grafana Loki pre-built binary package.

(define-module (peteches packages loki)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages compression))

(define-public loki
  (package
    (name "loki")
    (version "3.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/grafana/loki/releases/download/v"
             version "/loki-linux-amd64.zip"))
       (sha256 (base32 "0sdxx14ib893yy88ls365vib3hbh4glhmhpz0xrah1x6j56vybfx"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'chmod-binary
            (lambda* (#:key outputs #:allow-other-keys)
              (chmod (string-append (assoc-ref outputs "out") "/bin/loki")
                     #o755))))
      #:install-plan
      #~'(("loki-linux-amd64" "bin/loki"))))
    (home-page "https://grafana.com/oss/loki/")
    (synopsis "Like Prometheus, but for logs")
    (description
     "Loki is a horizontally-scalable, highly-available log aggregation system
inspired by Prometheus.  It indexes only log metadata (labels), not log
content, keeping storage costs low.")
    (license agpl3+)))
