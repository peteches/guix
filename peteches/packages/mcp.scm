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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python) ;pypi-uri
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (peteches packages go-deps)
  #:use-module (peteches packages python-deps))

;; ------------------------------
;; Outline MCP (Python)
;; ------------------------------
(define-public mcp-outline
  (package
    (name "mcp-outline")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.pythonhosted.org/packages/ad/3b/"
             "0acbc614fec384ded116446af9b235b2414b789d745c516d064beeede2a0/"
             "mcp_outline-" version ".tar.gz"))
       (sha256
        (base32 "0r72lq71ddhpbrrhlhl6b59bpg9p8ax5wxr4rq0bwp89v7j35rci"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Upstream's test suite uses dev-only pytest dependencies and includes
      ;; integration/e2e tests that expect an Outline instance or Docker.
      #:tests? #f))
    (native-inputs (list python-setuptools python-setuptools-scm python-wheel))
    (propagated-inputs (list python-httpx
                             python-mcp-1.26
                             python-dotenv
                             python-typer
                             python-uvicorn
                             python-starlette
                             python-sse-starlette
                             python-pyjwt
                             python-pydantic-settings
                             python-jsonschema
                             python-httpx-sse))
    (home-page "https://github.com/Vortiago/mcp-outline")
    (synopsis "MCP server for Outline document management")
    (description
     "MCP Outline is a Model Context Protocol server that lets MCP clients
search, read, create, update, archive, and manage documents and collections in
Outline knowledge bases.")
    (license license:expat)))

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
        (base32 "0hrfqal9fkhzyq810fy2xqa80dw6klw2zzb2jvny31fy3rarlpjn")))) ;run: guix hash -r <checkout>  OR `guix download -g …`
    (build-system go-build-system)
    (native-inputs (list go-github-com-stretchr-testify))
    (arguments
     (list
      #:go go-1.24
      ;; Top-level has main.go, so the import path is the module root.
      #:import-path "github.com/mark3labs/mcp-filesystem-server"))
    (inputs (list go-github-com-djherbis-times
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
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sonirico/mcp-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m32wydj6hj4y9b5zjq0rpb80a4g9w0h2sgh1i3aqm2vl9f5wk1d"))))
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

;; ------------------------------
;; Plane MCP (Python)
;; ------------------------------
(define-public plane-mcp-server
  (package
    (name "plane-mcp-server")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "plane_mcp_server" version))
       (sha256
        (base32 "12i4miqr97fnq0rzh1p6ar1fmjid13pkyygagfqg44f5kl6ypg7x"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Tests need a live Plane workspace / Redis.
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; The lua extra (lupa, not in Guix) is only needed for
          ;; fakeredis' server-side script emulation, which nothing in
          ;; plane_mcp exercises.
          (add-after 'unpack 'drop-fakeredis-lua-extra
            (lambda _
              (substitute* "pyproject.toml"
                (("fakeredis\\[lua\\]")
                 "fakeredis"))))
          ;; Importing plane_mcp segfaults the interpreter inside the
          ;; build container only; the same import (and the
          ;; plane_mcp.__main__ entry point) works in a normal profile,
          ;; e.g.: guix shell -L . -D plane-mcp-server python -- \
          ;; python3 -c 'import plane_mcp.__main__'
          (delete 'sanity-check))))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-authlib-1.7
                             python-boto3
                             python-fakeredis
                             python-fastmcp
                             python-mcp-1.26
                             python-plane-sdk
                             python-py-key-value-aio
                             python-pyjwt-2.13
                             python-redis))
    (home-page "https://github.com/makeplane/plane-mcp-server")
    (synopsis "MCP server for the Plane project management platform")
    (description
     "A Model Context Protocol server that lets MCP clients manage
projects, work items, cycles, modules, and initiatives in Plane via its
API.")
    (license license:expat)))

;; ------------------------------
;; Grafana MCP (Go)
;; ------------------------------
;;
;; One server covers the whole observability stack: Prometheus metrics,
;; Loki logs (LogQL), dashboards and datasources.  That is why there is
;; no separate prometheus/loki MCP package here.
;;
;; Packaging note: the module graph is ~218 modules deep (it pulls in
;; prometheus/prometheus, grafana-plugin-sdk-go, apache arrow and the
;; AWS SDK), so the usual one-Guix-package-per-Go-module approach is not
;; practical.  Instead `mcp-grafana-vendor' is a *fixed-output*
;; derivation: it is allowed network access, runs `go mod vendor', and
;; is pinned by a single hash — the same shape as Rust/cargo packaging.
;;
;; This is reproducible because Guix hashes the output as a nar, and nar
;; serialisation records only file names, contents and the executable
;; bit — never timestamps.  `go mod vendor' output is otherwise fully
;; determined by the pinned go.mod/go.sum.
;;
;; The real build then runs with GOFLAGS=-mod=vendor and GOPROXY=off, so
;; it is completely offline and cannot silently drift.
;;
;; Bumping the version means refreshing BOTH hashes: the source hash and
;; the vendor hash.  Set them to a dummy value and read the correct hash
;; out of the resulting "hash mismatch" build error.

(define mcp-grafana-version
  "1.1.0")

(define mcp-grafana-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/grafana/mcp-grafana")
          (commit (string-append "v" mcp-grafana-version))))
    (file-name (git-file-name "mcp-grafana" mcp-grafana-version))
    (sha256 (base32 "0ywdgs9y4zwvi7izipkb59k2b6g3cfx0i765qzsxpbc1qamdl7gg"))))

(define mcp-grafana-vendor
  (computed-file (string-append "mcp-grafana-" mcp-grafana-version "-vendor")
                 (with-imported-modules '((guix build utils))
                                        #~(begin
                                            (use-modules (guix build utils))
                                            (setenv "PATH"
                                                    (string-append #$go-1.26
                                                                   "/bin:"
                                                                   #$git-minimal
                                                                   "/bin"))
                                            ;; Go insists on a writable HOME and cache dirs.
                                            (setenv "HOME" "/tmp")
                                            (setenv "GOPATH" "/tmp/gopath")
                                            (setenv "GOCACHE" "/tmp/gocache")
                                            (setenv "GOMODCACHE"
                                                    "/tmp/gomodcache")
                                            ;; Never let Go fetch a different toolchain over the network —
                                            ;; that would make the output depend on upstream's toolchain
                                            ;; line rather than on the go-1.26 pinned here.
                                            (setenv "GOTOOLCHAIN" "local")
                                            (setenv "GOFLAGS" "-mod=mod")
                                            ;; nss-certs ships a hashed cert directory, not a single bundle
                                            ;; file, so only SSL_CERT_DIR is meaningful here.
                                            (setenv "SSL_CERT_DIR"
                                                    #$(file-append nss-certs
                                                       "/etc/ssl/certs"))
                                            ;; The store source is read-only; go writes go.sum entries.
                                            (copy-recursively #$mcp-grafana-source
                                                              "/tmp/src")
                                            (for-each make-file-writable
                                                      (find-files "/tmp/src"
                                                                  #:directories?
                                                                  #t))
                                            (chdir "/tmp/src")
                                            (invoke "go" "mod" "vendor")
                                            (copy-recursively
                                             "/tmp/src/vendor"
                                             #$output)))
                 #:options `(#:hash-algo sha256
                             #:recursive? #t
                             #:hash ,(base32
                                      "1y1lsb47sqb87nm93bf2b4w657wld812lcw7xvyxva2k9ksx32sa")
                             ;; Keep it off the build offload path; this needs network.
                             #:local-build? #t)))

(define-public mcp-grafana
  (package
    (name "mcp-grafana")
    (version mcp-grafana-version)
    (source
     mcp-grafana-source)
    (build-system gnu-build-system)
    (arguments
     (list
      ;; Upstream's suite is largely integration tests wanting a live
      ;; Grafana, Prometheus and Loki behind docker-compose.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'check)
          (add-after 'unpack 'install-vendor
            (lambda _
              (copy-recursively #$mcp-grafana-vendor "vendor")))
          (replace 'build
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "GOCACHE" "/tmp/gocache")
              (setenv "GOTOOLCHAIN" "local")
              ;; Offline: everything must come from vendor/.
              (setenv "GOFLAGS" "-mod=vendor")
              (setenv "GOPROXY" "off")
              (invoke "go"
                      "build"
                      "-ldflags"
                      "-s -w"
                      "-o"
                      "mcp-grafana"
                      "./cmd/mcp-grafana")))
          (replace 'install
            (lambda _
              (install-file "mcp-grafana"
                            (string-append #$output "/bin")))))))
    (native-inputs (list go-1.26))
    (home-page "https://github.com/grafana/mcp-grafana")
    (synopsis "MCP server for Grafana, Prometheus and Loki")
    (description
     "This package provides a Model Context Protocol server for Grafana.  It
lets MCP clients query Prometheus metrics and Loki logs with PromQL and LogQL,
search and read dashboards and folders, inspect datasources, and generate
deeplinks back into Grafana.

Tool groups are selected with @option{--enabled-tools}, and
@option{--disable-write} restricts the server to read-only operations.")
    (license license:asl2.0)))

;; ------------------------------
;; Slack MCP (Go)
;; ------------------------------
;;
;; Same fixed-output vendor-derivation shape as mcp-grafana above: the
;; go.mod graph pulls in the full charmbracelet TUI stack (bubbletea, huh,
;; lipgloss, …), rusq/slackdump, openai-go and golang.ngrok.com/ngrok, so
;; one-package-per-Go-module is not practical here either.

(define slack-mcp-server-version
  "1.3.0")

(define slack-mcp-server-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/korotovsky/slack-mcp-server")
          (commit (string-append "v" slack-mcp-server-version))))
    (file-name (git-file-name "slack-mcp-server" slack-mcp-server-version))
    (sha256 (base32 "1kmi7fsx6nvq42qjm3cnd6bz1qqg91rzv8ksqrd7n1bllp4gm1r3"))))

(define slack-mcp-server-vendor
  (computed-file (string-append "slack-mcp-server-"
                                slack-mcp-server-version "-vendor")
                 (with-imported-modules '((guix build utils))
                                        #~(begin
                                            (use-modules (guix build utils))
                                            (setenv "PATH"
                                                    (string-append #$go-1.26
                                                                   "/bin:"
                                                                   #$git-minimal
                                                                   "/bin"))
                                            (setenv "HOME" "/tmp")
                                            (setenv "GOPATH" "/tmp/gopath")
                                            (setenv "GOCACHE" "/tmp/gocache")
                                            (setenv "GOMODCACHE"
                                                    "/tmp/gomodcache")
                                            (setenv "GOTOOLCHAIN" "local")
                                            (setenv "GOFLAGS" "-mod=mod")
                                            (setenv "SSL_CERT_DIR"
                                                    #$(file-append nss-certs
                                                       "/etc/ssl/certs"))
                                            (copy-recursively
                                             #$slack-mcp-server-source
                                             "/tmp/src")
                                            (for-each make-file-writable
                                                      (find-files "/tmp/src"
                                                                  #:directories?
                                                                  #t))
                                            (chdir "/tmp/src")
                                            (invoke "go" "mod" "vendor")
                                            (copy-recursively
                                             "/tmp/src/vendor"
                                             #$output)))
                 #:options `(#:hash-algo sha256
                             #:recursive? #t
                             #:hash ,(base32
                                      "0gw179yyzd9qam2n5p7ybks92kn4ac47crm030kbz85x6cw13r7s")
                             #:local-build? #t)))

(define-public slack-mcp-server
  (package
    (name "slack-mcp-server")
    (version slack-mcp-server-version)
    (source slack-mcp-server-source)
    (build-system gnu-build-system)
    (arguments
     (list
      ;; Upstream's suite exercises real Slack API calls (slackdump,
      ;; slackauth) and needs live xoxc/xoxd/xoxp credentials; nothing here
      ;; can supply those in a build sandbox.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'check)
          (add-after 'unpack 'install-vendor
            (lambda _
              (copy-recursively #$slack-mcp-server-vendor "vendor")))
          (replace 'build
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "GOCACHE" "/tmp/gocache")
              (setenv "GOTOOLCHAIN" "local")
              (setenv "GOFLAGS" "-mod=vendor")
              (setenv "GOPROXY" "off")
              (invoke "go"
                      "build"
                      "-ldflags"
                      "-s -w"
                      "-o"
                      "slack-mcp-server"
                      "./cmd/slack-mcp-server")))
          (replace 'install
            (lambda _
              (install-file "slack-mcp-server"
                            (string-append #$output "/bin")))))))
    (native-inputs (list go-1.26))
    (home-page "https://github.com/korotovsky/slack-mcp-server")
    (synopsis "MCP server for Slack workspaces")
    (description
     "This package provides a Model Context Protocol server for Slack.  It
lets MCP clients read channel history and threads, search messages, list
channels and users, post messages and manage reactions (both gated behind
explicit opt-in environment variables), subject to whichever Slack token
type it is given: a bot token (@code{xoxb-}), a user OAuth token
(@code{xoxp-}), or a browser session token pair (@code{xoxc-}/@code{xoxd-}).")
    (license license:expat)))
