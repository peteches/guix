;; go-deps.scm — Go library packages needed for MCP servers
(define-module (peteches packages go-deps)
  ;; Guix core
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)
  #:use-module (guix utils)

  ;; Deps
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  
  ;; Licensing
  #:use-module ((guix licenses) #:prefix license:))

;; --- Go libraries (drop these next to your package) -------------------------
;; peteches/packages/mcp-go-source.scm

(define-public go-github-com-mark3labs-mcp-go-source
  (package
   (name "go-github-com-mark3labs-mcp-go-source")
   (version "v0.36.0")			; pick the tag you want
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/mark3labs/mcp-go")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256 (base32 "08kg20ycqarnaqv1gyrnpg3bry8nzqi866lmgwxhd90ghysrxkr5"))))
   (build-system copy-build-system)
       (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              (let* ((out  #$output)
                     (dest (string-append out "/src/github.com/mark3labs/mcp-go")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
   (propagated-inputs (list
		       go-github-com-google-uuid
		       go-github-com-spf13-cast
		       go-github-com-davecgh-go-spew
		       go-github-com-pmezard-go-difflib
		       go-github-com-invopop-jsonschema
		       go-github-com-yosida95-uritemplate
		       go-gopkg-in-yaml-v3))
   (home-page "https://github.com/mark3labs/mcp-go")
   (synopsis "MCP-Go source bundle for Guix Go builds")
   (description
    "Installs the mark3labs/mcp-go repository as sources under
$out/src/github.com/mark3labs/mcp-go so downstream packages can import any
subpackage (mcp, server, util, etc.) without separate package definitions.")
   (license license:expat)))

(define-public go-github-com-yosida95-uritemplate
  (package
   (name "go-github-com-yosida95-uritemplate")
   (version "v3.0.2")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/yosida95/uritemplate")
		  (commit version)))
	    (file-name (git-file-name name version))
	    (sha256 (base32 "0csrdr64hjhwxlkcbb8y7bz1ccnyzl9c87fva00gr078nw52qxff"))))
   (build-system go-build-system)
   (arguments (list #:import-path "github.com/yosida95/uritemplate"))
   (home-page "https://github.com/yosida95/uritemplate")
   (synopsis "URI Template (RFC6570) Implementation in Go")
   (description "uritemplate is a Go implementation of URI Template [RFC6570] with full functionality of URI Template Level 4.")
   (license license:bsd-3)))

(define-public go-github-com-invopop-jsonschema
  (package
    (name "go-github-com-invopop-jsonschema")
    (version "v0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/invopop/jsonschema")
              (commit version)))           ; tag v0.13.0
       (file-name (git-file-name name version))
       ;; placeholder random base32; replace with the one from `guix hash -rx`.
       (sha256 (base32 "0my5j2fycl0xf3vn02xzy6fr7dkf8nkn62f8y5i2xish69007vhm"))))
    (build-system go-build-system)
    (arguments
     (list
       ;; Library lives at repo root as "github.com/invopop/jsonschema"
       #:import-path "github.com/invopop/jsonschema"
       ;; Keep tests enabled; we add testify as a native-input below.
       #:tests? #t))
    ;; Test-only deps go in native-inputs; library deps go in propagated-inputs.
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-wk8-go-ordered-map)) ; required at runtime/build
    (home-page "https://github.com/invopop/jsonschema")
    (synopsis "Generate JSON Schemas from Go types (library)")
    (description
     "Go library that uses reflection to generate JSON Schema (Draft 2020-12)
from Go types. This is the maintained fork of alecthomas/jsonschema.")
    (license license:expat)))

(define-public go-github-com-wk8-go-ordered-map
  (package
    (name "go-github-com-wk8-go-ordered-map")
    (version "v2.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/wk8/go-ordered-map")
              (commit version)))   ; tag v2.1.8
       (file-name (git-file-name name version))
       ;; Replace with the actual hash from `guix hash -rx .`
       (sha256
        (base32 "0vzl2j6m9pz8ckikf9z2da9zxdbi7fwhcwq8rmzpmf34zl8cjn1g"))))
    (build-system go-build-system)
    (native-inputs
     (list
      go-github-com-stretchr-testify))
    (propagated-inputs
     (list
      go-gopkg-in-yaml-v3
      go-github-com-buger-jsonparser
      go-github-com-bahlo-generic-list-go
      go-github-com-mailru-easyjson))
    (arguments
     (list
      ;; Note: the v2 module path includes `/v2` suffix
      #:import-path "github.com/wk8/go-ordered-map/v2"
      #:tests? #t))
    ;; No extra deps; it’s a standalone library.
    (home-page "https://github.com/wk8/go-ordered-map")
    (synopsis "Ordered map implementation for Go")
    (description
     "An ordered map implementation for Go 1.18+ with support for generics.
It preserves insertion order while providing efficient key lookups.")
    (license license:expat)))

(define-public go-github-com-buger-jsonparser
  (package
    (name "go-github-com-buger-jsonparser")
    (version "v1.1.1")      ;; adjust if you need a different tag
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/buger/jsonparser")
             (commit version)))
       (file-name (git-file-name name version))
       ;; Replace with the real hash from:  guix download -g https://github.com/buger/jsonparser --commit=v1.1.1
       (sha256 (base32 "0qv2lsh2biwxn927941gqiv5pqg7n4v58j0i536pjp7pr17pq7dp"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/buger/jsonparser"
      ;; Keep tests off unless you package any extra test deps they might use.
      #:tests? #f))
    (home-page "https://github.com/buger/jsonparser")
    (synopsis "Fast JSON parser for Go")
    (description
     "A fast, zero-allocation JSON parser for Go that lets you retrieve fields
without unmarshalling into structs.")
    (license license:expat)))  ;; MIT

;; ---------------------------------------------------------------------------
;; github.com/bahlo/generic-list-go — generic doubly linked list
;; ---------------------------------------------------------------------------
(define-public go-github-com-bahlo-generic-list-go
  (package
    (name "go-github-com-bahlo-generic-list-go")
    (version "v0.2.0")     ;; adjust as needed
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bahlo/generic-list-go")
             (commit version)))
       (file-name (git-file-name name version))
       ;; Replace with the real hash from:  guix download -g https://github.com/bahlo/generic-list-go --commit=v0.2.0
       (sha256 (base32 "1nif01xg2y7ihhik65xkx74kszamgvz9ykknj81p71mmdv0fm304"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; v0/v1 module path has no suffix; (if you ever use v2+, remember to append /v2)
      #:import-path "github.com/bahlo/generic-list-go"
      #:tests? #t))  ;; this library’s tests are stdlib-only; safe to run
    (home-page "https://github.com/bahlo/generic-list-go")
    (synopsis "Generic doubly linked list for Go 1.18+")
    (description
     "A generic doubly linked list implementation for Go (requires generics).")
    (license license:expat)))  ;; MIT


;; 1) github.com/google/go-github/v74  (REST v3 client)
;;
;; We stage the whole repo under:
;;   $out/src/github.com/google/go-github
;; so imports like "github.com/google/go-github/v74/github" resolve.
(define-public go-github-com-google-go-github-v74-source
  (package
    (name "go-github-com-google-go-github-v74-source")
    (version "v74.0.0")                    ; pick the tag you need
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/go-github")
                     (commit version)))     ; e.g. v74.0.0
              (file-name (git-file-name name version))
              (sha256 (base32 "0ckhf71wp1k4b8m7z400rbbm95gp0r831l9xvrnzfj0vbsc0b781"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              (let* ((out  #$output)
                     (dest (string-append out "/src/github.com/google/go-github")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/google/go-github")
    (synopsis "Source bundle for google/go-github (v74)")
    (description "Stages google/go-github under GOPATH so downstream Go builds can import v74 packages offline.")
    (license license:bsd-3)))

;; 2) github.com/migueleliasweb/go-github-mock  (helpers/mocks for go-github)
;;
;; Staged under:
;;   $out/src/github.com/migueleliasweb/go-github-mock
(define-public go-github-com-migueleliasweb-go-github-mock-source
  (package
    (name "go-github-com-migueleliasweb-go-github-mock-source")
    (version "v1.4.0")                     ; adjust to the tag you need
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/migueleliasweb/go-github-mock")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32 "07b3bjy1zqzpb05gq6bcc8s4b09b2ajia9vbyz61vfmiiynpiyxk"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              (let* ((out  #$output)
                     (dest (string-append out "/src/github.com/migueleliasweb/go-github-mock")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/migueleliasweb/go-github-mock")
    (synopsis "Source bundle for go-github-mock")
    (description "Stages go-github-mock for offline builds that rely on google/go-github mocks.")
    (license license:expat)))              ; MIT (verify on the tag you pin)

;; 3) github.com/shurcooL/githubv4  (GraphQL v4 client)
;;
;; Staged under:
;;   $out/src/github.com/shurcooL/githubv4
(define-public go-github-com-shurcool-githubv4-source
  (package
    (name "go-github-com-shurcool-githubv4-source")
    (version "v0.0.0-20240108")            ; pick the tag/commit you need
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/shurcooL/githubv4")
                     (commit version)))     ; tag or commit
              (file-name (git-file-name name version))
              (sha256 (base32 "05ca46fzsdscdxaj64z039wwcbwafyvcl8dkmi8fcg10dswxns10"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda _
              (let* ((out  #$output)
                     (dest (string-append out "/src/github.com/shurcooL/githubv4")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/shurcooL/githubv4")
    (synopsis "Source bundle for shurcooL/githubv4")
    (description "Stages shurcooL/githubv4 under GOPATH for offline imports.")
    (license license:expat)))              ; MIT
