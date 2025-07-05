(define-module (peteches packages aider)
  #:use-module (guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages tree-sitter)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages python-build))

(define-public aider
  (package
   (name "aider")
   (version "v0.75.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
	   (url "https://github.com/Aider-AI/aider")
	   (commit version)))
     (file-name (git-file-name name version))
     (sha256 (base32 "1k2bly81a6jm8lqxfhyphg373mi6bl1h44wwn3ds9gvcgh16yypr"))))
   (build-system pyproject-build-system)
   (arguments '(#:tests? #f))
   (native-inputs
    (list
     python-tree-sitter
     python-setuptools
     python-wheel))
   (home-page "https://aider.chat/")
   (synopsis "AI pair programming in your terminal")
   (description "AI pair programming")
   (license license:apsl2)))

;; This allows you to run guix shell -f guix-packager.scm.
;; Remove this line if you just want to define a package.
aider
