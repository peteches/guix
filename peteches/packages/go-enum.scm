(define-module (peteches packages aider)
  #:use-module (guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system go))

(define-public go-enum
  (package
   (name "go-enum")
   (version "v0.6.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
	   (url "https://github.com/abice/go-enum")
	   (commit version)))
     (file-name (git-file-name name version))
     (sha256 (base32 "05zinyamqfa8qz89iwv7dp5w7h5jcl9vlafsr3vsiclf73cdqabd"))))
   (build-system go-build-system)
   (home-page "https://github.com/abice/go-enum")
   (synopsis "An enum generator for go")
   (description "An enum generator for go")
   (license license:expat)))

;; This allows you to run guix shell -f guix-packager.scm.
;; Remove this line if you just want to define a package.
go-enum
