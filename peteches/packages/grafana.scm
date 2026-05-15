;; peteches/packages/grafana.scm — Grafana pre-built binary package.

(define-module (peteches packages grafana)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public grafana
  (package
    (name "grafana")
    (version "13.0.1-security-01")
    (source
     (origin
       (method url-fetch)
       ;; Security releases use a non-standard filename with embedded build number.
       (uri "https://dl.grafana.com/grafana/release/13.0.1+security-01/grafana_13.0.1+security-01_25720641773_linux_amd64.tar.gz")
       (sha256 (base32 "0p4nh2vi2lhxzkqsfqmjfab49898nrywfk5dqbwnwy7viblhbkqq"))))
    (build-system copy-build-system)
    ;; Copy the full tarball tree so --homepath can find public/ and conf/.
    (arguments
     (list
      #:install-plan
      #~'(("." "."))))
    (home-page "https://grafana.com/")
    (synopsis "Open-source analytics and monitoring platform")
    (description
     "Grafana lets you query, visualise, alert on, and understand your metrics
from multiple data sources including Prometheus.")
    (license agpl3+)))
