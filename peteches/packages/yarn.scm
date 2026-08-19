(define-module (peteches packages yarn)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module ((gnu packages node) #:select (node)))

(define-public yarn
  (package
    (name "yarn")
    (version "1.22.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/yarnpkg/yarn/releases/download/v"
             version "/yarn-v" version ".tar.gz"))
       (sha256
        (base32 "181nvynhhrbga3c209v8cd9psk6lqjkc1s9wyzy125lx35j889l8"))))
    (build-system copy-build-system)
    (propagated-inputs (list node))
    (arguments
     (list
      #:install-plan
      #~(list (list "bin" "bin")
              (list "lib" "lib")
              (list "package.json" "share/yarn/package.json"))))
    (home-page "https://yarnpkg.com/")
    (synopsis "Fast, reliable, and secure dependency management for Node.js")
    (description
     "Yarn (Classic, 1.x) is a package manager for JavaScript.  It relies
on @code{node} being available on @code{PATH} at run time.")
    (license bsd-2)))
