(define-module (peteches packages go-enum)
  #:use-module (guix)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system go))

(define-public go-enum
  (package
    (name "go-enum")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abice/go-enum")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05zinyamqfa8qz89iwv7dp5w7h5jcl9vlafsr3vsiclf73cdqabd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/abice/go-enum"))
    (home-page "https://github.com/abice/go-enum")
    (synopsis "An enum generator for go")
    (description "An enum generator for go")
    (license license:expat)))
