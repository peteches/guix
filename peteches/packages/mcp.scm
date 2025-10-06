;; peteches/packages/mcp-alt.scm
;; Guix packages for MCP servers without Node/TypeScript.
;; - Filesystem (Go): github.com/mark3labs/mcp-filesystem-server
;; - Git (Python):    PyPI mcp-server-git
;; - GitHub (Go):     github.com/github/github-mcp-server
;;
;; Usage:
;;   guix build -L ~/area_51/guix mcp-filesystem-server-go
;;   guix build -L ~/area_51/guix mcp-server-git-python
;;   guix build -L ~/area_51/guix github-mcp-server-go

(define-module (peteches packages mcp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (peteches packages go-deps))

;; ------------------------------
;; Filesystem MCP (Go)
;; ------------------------------
(define-public mcp-server-filesystem-go
  (package
   (name "mcp-server-filesystem-go")
   ;; Pin to a tagged release (example: v0.11.1 seen upstream).
   (version "v0.11.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/mark3labs/mcp-filesystem-server")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0hrfqal9fkhzyq810fy2xqa80dw6klw2zzb2jvny31fy3rarlpjn"))))  ; run: guix hash -r <checkout>  OR `guix download -g …`
   (build-system go-build-system)
   (native-inputs
    (list
     go-github-com-stretchr-testify))
   (arguments
    (list
     #:go go-1.24
     ;; Top-level has main.go, so the import path is the module root.
     #:import-path "github.com/mark3labs/mcp-filesystem-server"))
   (inputs
    (list go-github-com-djherbis-times
	  go-github-com-gabriel-vasile-mimetype
	  go-github-com-gobwas-glob
	  go-github-com-mark3labs-mcp-go-source))
   (home-page "https://github.com/mark3labs/mcp-filesystem-server")
   (synopsis "MCP filesystem server (Go)")
   (description
    "A Go implementation of an MCP server exposing secure filesystem tools.")
   (license license:expat)))

(define-public go-github-com-sonirico-mcp-shell
  (package
    (name "go-github-com-sonirico-mcp-shell")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sonirico/mcp-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "008fc75xljp0lvp09x64nv8ai9a6w3xmh9s207fzisxqmsfch2bn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.24
      #:import-path "github.com/sonirico/mcp-shell"))
    (propagated-inputs (list go-gopkg-in-yaml-v3 go-github-com-rs-zerolog
                             go-github-com-mark3labs-mcp-go-source
                             go-github-com-joho-godotenv))
    (home-page "https://github.com/sonirico/mcp-shell")
    (synopsis "mcp-shell 🐚")
    (description
     "This package provides a robust Model Context Protocol (MCP) server that provides
secure shell command execution capabilities to AI assistants and other MCP
clients.  In other words: the brain thinks, this runs the commands.")
    (license license:expat)))
