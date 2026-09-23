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
    (version "13.2.2")
    (source
     (origin
       (method url-fetch)
       ;; Release filenames embed the CI build number.
       (uri
        "https://dl.grafana.com/grafana/release/13.2.2/grafana_13.2.2_34846740809_linux_amd64.tar.gz")
       (sha256
        (base32 "13scaignw7h1lng60g8gn90zab5n8pfzpxp5faqgs94ql0wchqln"))))
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
