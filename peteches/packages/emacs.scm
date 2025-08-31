(define-module (peteches packages emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public emacs-mcp
  (package
    (name "emacs-mcp")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lizqwerscott/mcp.el")
                    (commit "2fcda27b15395fe1ed55d15a88e6187a69af09e9")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0q4ycgb4dx4p0v449vncmb4j0nklpy14wasz4x7pkyq6yhsx8lmz"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/lizqwerscott/mcp.el")
    (synopsis "Prompt manager for interacting with LLMs in Emacs")
    (description
     "mcp.el is an Emacs interface for managing reusable prompt templates and interacting with LLMs like ChatGPT via GPTel.")
    (license gpl3+)))
emacs-mcp

;; (define-public emacs-gotest
;;   (package
;;    (name "emacs-gotest")
;;    (version "v0.16.0")
;;    (source (origin
;; 	    (method git-fetch)
;; 	    (uri (git-reference
;; 		  (url "https://github.com/nlamirault/gotest.el.git")
;; 		  (commit version)))
;; 	    (sha256
;; 	     (base32 "19lpr9wa73415jmdl1acijz54h5sdsj95wxigigbiqdhq6pd301p"))
;; 	    (file-name "gotest")))
;;    (build-system emacs-build-system)
;;    (inputs (list emacs))
;;    (synopsis "Emacs mode to go unit test command line tool")
;;    (description "Emacs mode to go unit test command line tool")
;;    (home-page "https://github.com/nlamirault/gotest.el")
;;    (license gpl2)))

;; (define-public emacs-go-playground
;;   (package
;;    (name "emacs-go-playground")
;;    (version "v1.10.0")
;;    (source (origin
;; 	    (method git-fetch)
;; 	    (uri (git-reference
;; 		  (url "https://github.com/grafov/go-playground.git")
;; 		  (commit version)))
;; 	    (sha256
;; 	     (base32 "1kwy8wkj5m31z51zkaw5lmgg13jv1zxq091pzkbfkc9f2r75yhkb"))
;; 	    (file-name (git-file-name name version))))
;;    (build-system emacs-build-system)
;;    (inputs (list emacs-gotest))
;;    (propagated-inputs (list emacs-go-mode emacs-gotest))
;;    (synopsis "GNU/Emacs mode that setup local Go playground for code snippets like play.golang.org or even better :)")
;;    (description "A simple mode for GNU/Emacs to set up a local Go language playground, offering features similar to — and in some ways surpassing — the play.golang.org service, thanks to go-mode.  Treat it as a simple REPL for Go.  Unlike play.golang.org, this mode provides a complete local playground within Emacs, eliminating the need for a browser or even internet connection to experiment with code snippets.")
;;    (home-page "https://github.com/grafov/go-playground.git")
;;    (license gpl3)))
;; emacs-go-playground
