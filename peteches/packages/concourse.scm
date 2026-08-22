(define-module (peteches packages concourse)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system copy))

(define %version
  "8.3.0")
(define %base-url
  (string-append "https://github.com/concourse/concourse/releases/download/v"
                 %version))

(define %concourse-tarball-url
  (string-append %base-url "/concourse-" %version "-linux-amd64.tgz"))

(define %fly-tarball-url
  (string-append %base-url "/fly-" %version "-linux-amd64.tgz"))

(define-public concourse
  (package
    (name "concourse")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri %concourse-tarball-url)
       (sha256
        (base32 "0b4yy1dmbknpdw3mn618h46mvi8mnn7g7wni2f13g4s847g0q2rj"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("bin" "bin")
          ("resource-types" "resource-types"))))
    (home-page "https://concourse-ci.org/")
    (synopsis "Concourse is an open-source continuous thing-doer.")
    (description
     "Concourse is an automation system written in Go. It is most commonly used for CI/CD, and is built to scale to any kind of automation pipeline, from simple to complex.")
    (license asl2.0)))

(define-public fly
  (package
    (name "fly")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri %fly-tarball-url)
       (sha256
        (base32 "0hqgh0xfg4b5axvkyfwqcrg0wpw1hcb6wpgjwjymqrg1205ncpqb"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("fly" "bin/fly"))))
    (home-page "https://concourse-ci.org/")
    (synopsis
     "Fly is a cli to interact with Concourse which is an open-source continuous thing-doer.")
    (description
     "Fly is the CLI tool to manage Concourse which is an automation system written in Go. It is most commonly used for CI/CD, and is built to scale to any kind of automation pipeline, from simple to complex.")
    (license asl2.0)))
