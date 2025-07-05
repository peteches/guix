(define-module (peteches packages scripts)
  #:use-module (guix packages)
  #:use-module (gnu packages video)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages password-utils)
  #:use-module (nongnu packages mozilla)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gawk)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public shell-scripts
  (package
   (name "shell-scripts")
   (version "0.0.1")
   (synopsis "My personal shell scripts")
   (description "I like to use wofi to manage certain tasks and this package will install them for firefox, pass and screenshotting")
   (license gpl2)
   (home-page "https://github.com/peteches/scripts")
   (inputs (list wofi gawk grimblast clipman zbar pass-otp firefox wf-recorder))
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/peteches/scripts.git")
		  (commit "aad274384d4bdd25e8950e2dea2251d36fc8e4d3")))
	    (sha256 (base32 "0n18q757ck9pjy0hbz9wyfpydahd7aq7kg7cw5dxcc9ymndqra9b"))))
   (build-system copy-build-system)

   (arguments (list
	       #:install-plan #~'(("./" "bin/"))))))
shell-scripts
