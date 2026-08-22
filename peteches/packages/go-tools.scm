(define-module (peteches packages go-tools)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public go-golangci-lint
  (let* ((pkg-version "2.13.1")
         (tarball (string-append
                   "https://github.com/golangci/golangci-lint/releases/download/v"
                   pkg-version "/golangci-lint-" pkg-version
                   "-linux-amd64.tar.gz")))
    (package
      (name "go-golangci-lint")
      (version pkg-version)
      (source
       (origin
         (method url-fetch)
         (uri tarball)
         (sha256
          (base32 "1k5rrzi1jqn22p7vlwn0f34010ixpi035kpiykkqp95ask4znyxi"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~(list (list "golangci-lint" "bin/golangci-lint"))))

      (home-page "https://golangci-lint.run/")
      (synopsis "Golangci-lint is a fast linters runner for Go")
      (description
       "It runs linters in parallel, uses caching, supports YAML configuration,
integrates with all major IDEs, and includes over a hundred linters.")
      (license gpl3))))
