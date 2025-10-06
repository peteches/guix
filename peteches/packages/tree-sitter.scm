;; -*- scheme -*-
(define-module (peteches packages tree-sitter)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system tree-sitter)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (tree-sitter-yaml))

(define %yaml-commit
  "6129a83eeec7d6070b1c0567ec7ce3509ead607c")
(define %yaml-version
  (git-version "0.0.0" "1" %yaml-commit))

(define-public tree-sitter-yaml
  (package
    (name "tree-sitter-yaml")
    (version %yaml-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ikatyang/tree-sitter-yaml")
                    (commit %yaml-commit)))
              (file-name (git-file-name name version))
	      (sha256
               (base32 "1bimf5fq85wn8dwlk665w15n2bj37fma5rsfxrph3i9yb0lvzi3q"))))         ; <- fill in below
    (build-system tree-sitter-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/ikatyang/tree-sitter-yaml")
    (synopsis "YAML grammar for Tree-sitter")
    (description "Tree-sitter grammar for the YAML language.")
    (license license:expat)))
