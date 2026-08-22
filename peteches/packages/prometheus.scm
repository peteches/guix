(define-module (peteches packages prometheus)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public prometheus
  (let* ((version "3.14.0")
         (tarball (string-append
                   "https://github.com/prometheus/prometheus/releases/download/v"
                   version "/prometheus-" version ".linux-amd64.tar.gz")))
    (package
      (name "prometheus")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri tarball)
         (sha256
          (base32 "13a76636fw649d89nx4apwbv9jckjxyhrlqmr6cs6yzb37dccrgn"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~(list (list "prometheus" "bin/prometheus")
                (list "promtool" "bin/promtool"))))
      (home-page "https://prometheus.io/")
      (synopsis "Monitoring system and time series database")
      (description
       "Prometheus is an open-source monitoring and alerting toolkit
built for reliability and scalability.  It collects and stores metrics as time
series data, with a powerful query language (PromQL) for analysis.")
      (license asl2.0))))
