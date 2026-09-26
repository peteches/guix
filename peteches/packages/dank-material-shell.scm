;; peteches/packages/dank-material-shell.scm
;;
;; Guix's own `dank-material-shell'/`dank-material-shell-minimal' (in
;; gnu/packages/window-management.scm, via the `guix' channel) is pinned to
;; v0.5.1 (Nov 2025) and has never been bumped upstream.  That predates
;; DankMaterialShell's fix for Hyprland's Lua config migration (workspace
;; switcher clicks silently doing nothing) -- see PRs #2419 and #2443
;; upstream, first released in DMS v1.5.0.  This file bumps DMS to v1.6.2
;; locally, plus the handful of Go module dependencies it grew in the
;; ~30 releases since 0.5.1 that Guix proper doesn't have yet, and a few
;; existing Guix Go packages that are slightly too old for DMS's go.mod
;; floor versions.
;;
;; Dependency versions here were derived from a real `go list -deps` over
;; DMS v1.5.0's core/cmd/dms build (not just eyeballing go.mod), to avoid
;; over-including tailscale.com's full dependency closure -- DMS only uses
;; tailscale.com's client-side/type packages (client/local, ipn, tailcfg,
;; etc.), not the wireguard/gvisor networking internals, so go-tailscale-com
;; below stays a source-only propagation package rather than pulling in
;; tailscaled's full dependency tree.
;;
;; The final `dank-material-shell'/`dank-material-shell-minimal' packages
;; here intentionally share their `name' field with Guix proper's (now
;; superseded) packages of the same name, so `guix package -A` and
;; `peteches/home/modules/base.scm' both keep using the plain name -- disambiguate
;; on the CLI with an explicit version:
;;
;; Usage:
;;   guix build -L ~/area_51/guix dank-material-shell@1.6.2

(define-module (peteches packages dank-material-shell)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-vcs)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages window-management))

;;;
;;; Version bumps of existing Guix Go packages (DMS v1.5.0's go.mod floor
;;; versions are newer than what Guix proper currently packages).  Local
;;; overrides only -- upstream gnu/packages/*.scm is left untouched.
;;;

(define-public go-github-com-fsnotify-fsnotify-1.10
  (package
    (inherit go-github-com-fsnotify-fsnotify)
    (name "go-github-com-fsnotify-fsnotify")
    (version "1.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fsnotify/fsnotify")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dc2bwbji8slb5fc17az9m4q788i0p35cjh4lq6ak73qr214pc78"))))
    (arguments
     (list #:import-path "github.com/fsnotify/fsnotify"
           #:tests? #f))))

(define-public go-golang-org-x-image-0.39
  (package
    (inherit go-golang-org-x-image)
    (version "0.39.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://go.googlesource.com/image")
             (commit (string-append "v" version))))
       (file-name (git-file-name (package-name go-golang-org-x-image) version))
       (sha256
        (base32 "1ri588s7psw6jr07q8dl5r23majmdidy822pq1wy51b6hipkh5ly"))))))

(define-public go-github-com-dlclark-regexp2-1.12
  (package
    (inherit go-github-com-dlclark-regexp2)
    (version "1.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlclark/regexp2")
             (commit (string-append "v" version))))
       (file-name (git-file-name (package-name go-github-com-dlclark-regexp2)
                                  version))
       (sha256
        (base32 "1hnc0s6fanbspy4wm1gwcikxwckgnww6qhhdr0pnv25462ngcm9x"))))
    (arguments
     (list #:import-path "github.com/dlclark/regexp2"
           #:tests? #f))))

(define-public go-github-com-alecthomas-chroma-v2-2.27
  (package
    (inherit go-github-com-alecthomas-chroma-v2)
    (version "2.27.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/chroma")
             ;; Pinned to the exact commit the v2.27.0 tag pointed at when
             ;; fetched -- the tag was re-pointed upstream within hours of
             ;; the first fetch, which broke the fixed-output hash.
             (commit "a6d00fe2cdfc88da0b91396e577da16c75c9c7fb")))
       (file-name (git-file-name
                   (package-name go-github-com-alecthomas-chroma-v2) version))
       (sha256
        (base32 "18p9inwlwkbsgi9frqhxhqqwdymnwm02iqpjdgk7dg8sa0ab92w8"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files, packaged separately
            ;; upstream and not needed here:
            ;;
            ;; - github.com/alecthomas/chroma/v2/cmd/chroma
            ;; - github.com/alecthomas/chroma/v2/cmd/chromad
            (delete-file-recursively "cmd")))))
    (arguments
     (list #:import-path "github.com/alecthomas/chroma/v2"
           ;; The inherited channel package pins go-1.25, whose toolchain
           ;; build is not in the store (and the offload host that would
           ;; build it is down); go-1.26 is in the store and satisfies
           ;; chroma v2.27.0's 'go 1.25' floor.
           #:go go-1.26
           #:tests? #f))
    (native-inputs '())
    ;; 2.27.0 migrated its Regexp usage from dlclark/regexp2 v1 to v2
    ;; (CompiledRule.Regexp is now *regexp2/v2.Regexp) -- DMS v1.6.2's
    ;; raku.go passes v2 regexps into chroma's CompiledRule and only
    ;; compiles against this chroma version.
    (propagated-inputs
     (list go-github-com-dlclark-regexp2-v2))))

(define-public go-github-com-mdlayher-socket-0.6
  (package
    (inherit go-github-com-mdlayher-socket)
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/socket")
             (commit (string-append "v" version))))
       (file-name (git-file-name (package-name go-github-com-mdlayher-socket)
                                  version))
       (sha256
        (base32 "1gx1x4h03aj1whvcy1b1x4xhnink0kriv90l3f029aqgdpm2x722"))))
    (arguments
     (list #:import-path "github.com/mdlayher/socket"
           #:tests? #f))))

(define-public go-github-com-mdlayher-netlink-1.11
  (package
    (inherit go-github-com-mdlayher-netlink)
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdlayher/netlink")
             (commit (string-append "v" version))))
       (file-name (git-file-name (package-name go-github-com-mdlayher-netlink)
                                  version))
       (sha256
        (base32 "1n3r2p3y1bivgnzh5fhfr7a7w3js6kwnmq4rz11sgv23qvwpghsq"))))
    (arguments
     (list #:import-path "github.com/mdlayher/netlink"
           #:tests? #f))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs go-github-com-mdlayher-netlink)
       (replace "go-github-com-mdlayher-socket" go-github-com-mdlayher-socket-0.6)))))

;;;
;;; Genuinely new Go module packages -- not in Guix proper at all yet.
;;;

(define-public go-github-com-yeqown-reedsolomon
  (package
    (name "go-github-com-yeqown-reedsolomon")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yeqown/reedsolomon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a7nwx3zw72f330jddklc7na941d2zdyahswifwdjk4giycyzasm"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/yeqown/reedsolomon"
           #:tests? #f))
    (home-page "https://github.com/yeqown/reedsolomon")
    (synopsis "Reed-Solomon erasure coding in Go")
    (description
     "This package provides a Reed-Solomon erasure coding implementation,
used by @code{go-qrcode} for QR code error correction.")
    (license license:expat)))

(define-public go-github-com-yeqown-go-qrcode
  (package
    (name "go-github-com-yeqown-go-qrcode")
    (version "2.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yeqown/go-qrcode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02131hnls752nh62lzndwsq2b003bk0h9jr1cs89ibhmxyqcz0v8"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/yeqown/go-qrcode/v2"
           #:unpack-path "github.com/yeqown/go-qrcode"
           #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               ;; This module declares itself as
               ;; "github.com/yeqown/go-qrcode/v2" in go.mod (Go's semantic
               ;; import versioning) but keeps its .go files at the repo
               ;; root rather than under a real "v2" subdirectory.  Guix's
               ;; go-build-system resolves import paths against the
               ;; filesystem (GO111MODULE=off), so materialize a real "v2"
               ;; subdirectory duplicating the repo root -- a self-symlink
               ;; ("v2" -> ".") compiles fine but collides with the later
               ;; 'install phase's own recursive source copy.
               (add-after 'unpack 'add-v2-self-copy
                 (lambda _
                   (let ((root "src/github.com/yeqown/go-qrcode"))
                     (copy-recursively root (string-append root "-v2-tmp"))
                     (rename-file (string-append root "-v2-tmp")
                                  (string-append root "/v2"))))))))
    (propagated-inputs
     (list go-github-com-yeqown-reedsolomon))
    (home-page "https://github.com/yeqown/go-qrcode")
    (synopsis "QR code encoder for Go")
    (description "Package qrcode implements a QR code encoder.")
    (license license:expat)))

(define-public go-github-com-yeqown-go-qrcode-writer-standard
  (package
    (name "go-github-com-yeqown-go-qrcode-writer-standard")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yeqown/go-qrcode")
             (commit "writer/standard/v1.3.0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ng6ppg6m5a0ydyznlpq3dzwq8ll96nz1dvwdkl92p7ba2cy2zkv"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/yeqown/go-qrcode/writer/standard"
           #:unpack-path "github.com/yeqown/go-qrcode"
           #:tests? #f))
    (propagated-inputs
     (list go-github-com-fogleman-gg
           go-github-com-pkg-errors
           go-github-com-yeqown-go-qrcode
           go-golang-org-x-image-0.39))
    (home-page "https://github.com/yeqown/go-qrcode")
    (synopsis "Standard image writer for go-qrcode")
    (description
     "Standard Writer draws a QR Code into an @code{io.Writer}, normally a
file, as a PNG/JPEG image.")
    (license license:expat)))

(define-public go-github-com-sblinch-kdl-go
  (package
    (name "go-github-com-sblinch-kdl-go")
    (version "0.0.0-20260121213736-8b7053306ca6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sblinch/kdl-go")
             (commit "8b7053306ca6")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1flr5sxvglajiw9nh65flpzc8c17nzgisqx8y284f53a80x00ja9"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/sblinch/kdl-go"
           #:tests? #f))
    (home-page "https://github.com/sblinch/kdl-go")
    (synopsis "KDL v1 document language library for Go")
    (description
     "kdl-go is a Go library for version 1 of the KDL Document Language.  It
supports encoding and decoding KDL documents, marshaling and unmarshaling
them into Go structs.")
    (license license:expat)))

(define-public go-tailscale-com
  ;; DMS only imports tailscale.com's client-side/type packages
  ;; (client/local, ipn, ipn/ipnstate, tailcfg, types/key, types/views and
  ;; their transitive support packages) to talk to a locally-running
  ;; tailscaled over its LocalAPI -- not tailscaled's own wireguard/gvisor
  ;; networking internals.  The module root package (`tailscaleroot') only
  ;; embeds a couple of text files and has no dependencies of its own, so
  ;; building/installing just that lets the *source* of the whole module be
  ;; propagated (via the default #:install-source? #t) for DMS to import
  ;; whichever subpackages it actually needs at its own build time, without
  ;; this package needing to compile the rest of the tree.
  (package
    (name "go-tailscale-com")
    (version "1.96.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tailscale/tailscale")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xrprnng8vy9llcwc1x5n1rdrhbsi4sx71k1iknpgfddcgxip1mx"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "tailscale.com"
           #:tests? #f))
    (propagated-inputs
     (list go-filippo-io-edwards25519
           go-github-com-cloudflare-circl
           go-github-com-coder-websocket
           go-github-com-fxamacker-cbor-v2
           go-github-com-go-json-experiment-json
           go-github-com-hdevalence-ed25519consensus
           go-github-com-jsimonetti-rtnetlink
           go-github-com-mdlayher-netlink-1.11
           go-github-com-mdlayher-socket-0.6
           go-github-com-mitchellh-go-ps
           go-go4-org-mem
           go-go4-org-netipx
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-text))
    (home-page "https://tailscale.com/")
    (synopsis "Tailscale Go client and type libraries")
    (description
     "This package provides the @code{tailscale.com} Go module's
client-facing packages (@code{client/local}, @code{ipn}, @code{tailcfg},
etc.) used by third-party Go programs to talk to a locally running
@code{tailscaled} over its LocalAPI.  It is not the Tailscale daemon or CLI
themselves -- see the @code{tailscale} package for those.")
    (license license:bsd-3)))

;;; Source-only propagation packages for the Go modules DMS v1.6.2 grew
;;; since the v1.5.0 closure was written.  None of these modules has a
;;; buildable root package (or has its v2/v3 module at the repo root, which
;;; Guix's GO111MODULE=off import-path resolution can't map), so instead of
;;; compiling them we copy their source into the GOPATH layout under their
;;; full import path -- the same pattern as
;;; go-github-com-mark3labs-mcp-go-source above.  DMS's own build then
;;; compiles only the subpackages it actually imports; every third-party
;;; dependency of those subpackages is already in DMS's input list, as
;;; confirmed by the exhaustive 'cannot find package' list from the failed
;;; v1.6.2 build.
;;;

(define-public go-github-com-avengemedia-dankgo
  (package
    (name "go-github-com-avengemedia-dankgo")
    (version "1.6.1-0.20260908011626-07e1ef7caaea")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AvengeMedia/dankgo")
             (commit "07e1ef7caaea834ba30b98f45fe05b8ade6304ef")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p478gkmz2bq8jh9psnf8z9yr52rxvn8yy7i9a9dgkl10xi9p077"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-source
            (lambda _
              (let* ((out #$output)
                     (dest (string-append out
                                          "/src/github.com/AvengeMedia/dankgo")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/AvengeMedia/dankgo")
    (synopsis "DankGo wayland/clipboard/shell support libraries (source)")
    (description
     "Source-only bundle of the AvengeMedia/dankgo Go module.  DMS imports
its wayland client, clipboard, logging and shell-app subpackages; they are
compiled by DMS's own build.")
    (license license:expat)))

(define-public go-github-com-avengemedia-dgop
  (package
    (name "go-github-com-avengemedia-dgop")
    (version "1.6.1-0.20260916132520-8a12dbc6e287")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AvengeMedia/dgop")
             (commit "8a12dbc6e28715592a14bf53ccb1ba22b6b76718")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m6y7vxyhli8hcj427vjjjiwq4lx56q016i69707jk64p4sw3s1r"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-source
            (lambda _
              (let* ((out #$output)
                     (dest (string-append out
                                          "/src/github.com/AvengeMedia/dgop")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/AvengeMedia/dgop")
    (synopsis "DGoP process/disk management libraries (source)")
    (description
     "Source-only bundle of the AvengeMedia/dgop Go module.  DMS imports
its gops subpackage; it is compiled by DMS's own build.")
    (license license:expat)))

;; Second resolution layer: dgop/gops's own imports.  huma's core package
;; and its casing/negotiation/validation/yaml subpackages have no
;; third-party dependencies at all; the gopsutil subpackages DMS uses
;; (cpu, disk, host, load, mem, net, process, sensors) import only x/sys
;; (channel) on Linux -- their plan9stats/perfstat/wmi/purego imports are
;; in Plan9/BSD/Windows/darwin build-tagged files that a Linux build never
;; compiles -- plus tklauser/go-sysconf from cpu_linux.go and
;; process_linux.go, which needs tklauser/numcpus.

(define-public go-github-com-danielgtaylor-huma-v2
  (package
    (name "go-github-com-danielgtaylor-huma-v2")
    (version "2.39.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/danielgtaylor/huma")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08xq0vw5ijw6085yx9cf6s3j8j89h6m8dn94rm33bb16jcz1zqha"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-source
            (lambda _
              (let* ((out #$output)
                     (dest (string-append out
                                          "/src/github.com/danielgtaylor/huma/v2")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/danielgtaylor/huma")
    (synopsis "Huma REST framework (source)")
    (description
     "Source-only bundle of the danielgtaylor/huma v2 Go module.  dgop's
gops subpackage imports its core package, which has no third-party
dependencies; compiled by the packages that import it.")
    (license license:expat)))

(define-public go-github-com-shirou-gopsutil-v4
  (package
    (name "go-github-com-shirou-gopsutil-v4")
    (version "4.26.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shirou/gopsutil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ng12fgfd6v6gxq2m30kxa6z1m35i2x8c1g8pwq5v3hyqkfg69a8"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-source
            (lambda _
              (let* ((out #$output)
                     (dest (string-append out
                                          "/src/github.com/shirou/gopsutil/v4")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/shirou/gopsutil")
    (synopsis "gopsutil/v4 system metrics (source)")
    (description
     "Source-only bundle of the shirou/gopsutil v4 Go module; compiled by
the packages that import it.")
    (license license:bsd-3)))

(define-public go-github-com-tklauser-go-sysconf
  (package
    (name "go-github-com-tklauser-go-sysconf")
    (version "0.3.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tklauser/go-sysconf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ipwx1q9wxsy84iwx97x7z9rcw6jqc1wpkfi5rdwzy069nr5pmc4"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-source
            (lambda _
              (let* ((out #$output)
                     (dest (string-append out
                                          "/src/github.com/tklauser/go-sysconf")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/tklauser/go-sysconf")
    (synopsis "go-sysconf getconf wrapper (source)")
    (description
     "Source-only bundle of the tklauser/go-sysconf Go module; compiled by
the packages that import it.")
    (license license:bsd-3)))

(define-public go-github-com-tklauser-numcpus
  (package
    (name "go-github-com-tklauser-numcpus")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tklauser/numcpus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nzx13c5j8byijq8anc25cgm3jnvigcwmdpkapq22iigl0d9vg1r"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-source
            (lambda _
              (let* ((out #$output)
                     (dest (string-append out
                                          "/src/github.com/tklauser/numcpus")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/tklauser/numcpus")
    (synopsis "numcpus CPU count detection (source)")
    (description
     "Source-only bundle of the tklauser/numcpus Go module, a dependency of
go-sysconf; compiled by the packages that import it.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-dlclark-regexp2-v2
  (package
    (name "go-github-com-dlclark-regexp2-v2")
    (version "2.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlclark/regexp2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xld5hb2v2ih8sjjz8n0x38fv0d4xdwykhfl94yfdjc7f6ym7vik"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-source
            (lambda _
              (let* ((out #$output)
                     (dest (string-append out
                                          "/src/github.com/dlclark/regexp2/v2")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/dlclark/regexp2")
    (synopsis "regexp2/v2 .NET-compatible regular expressions (source)")
    (description
     "Source-only bundle of the dlclark/regexp2 v2 Go module; compiled by
the packages that import it.")
    (license license:expat)))

(define-public go-github-com-nadim147c-material-v3
  (package
    (name "go-github-com-nadim147c-material-v3")
    (version "3.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Nadim147c/material")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g7czr154pcbnzhchkqqclq70s5ym09lm691r8kbri9z5dv544pn"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-source
            (lambda _
              (let* ((out #$output)
                     (dest (string-append out
                                          "/src/github.com/Nadim147c/material/v3")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))
    (home-page "https://github.com/Nadim147c/material")
    (synopsis "Material color/quantization libraries (source)")
    (description
     "Source-only bundle of the Nadim147c/material v3 Go module; compiled
by the packages that import it.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-klauspost-compress-1.19
  (package
    (inherit go-github-com-klauspost-compress)
    (version "1.19.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/klauspost/compress")
             (commit (string-append "v" version))))
       (file-name (git-file-name (package-name go-github-com-klauspost-compress)
                                  version))
       (sha256
        (base32 "0wc04kf6692mq5xdy5n55wd7hsvq4c5rv18h8m1gr0dkpd7q9i66"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-source
            (lambda _
              (let* ((out #$output)
                     (dest (string-append out
                                          "/src/github.com/klauspost/compress")))
                (mkdir-p dest)
                (copy-recursively "." dest)))))))))

;;;
;;; DankMaterialShell itself, bumped to v1.6.2.
;;;

(define-public dank-material-shell-minimal-1.5.0
  (package
    (inherit dank-material-shell-minimal)
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AvengeMedia/DankMaterialShell")
             (commit (string-append "v" version))
             ;; quickshell/DankCommon is a relative symlink to
             ;; ../dank-qml-common/DankCommon -- a separate git submodule at
             ;; the repo root, not nested under quickshell/.  Without a
             ;; recursive checkout that submodule directory is empty and the
             ;; symlink dangles, so quickshell silently drops every QML file
             ;; that imports Common/Widgets from DankCommon (dozens of
             ;; "Ignoring unresolvable import" warnings, then a blank shell).
             (recursive? #t)))
       (file-name (git-file-name "dank-material-shell-minimal" version))
       (sha256
        (base32 "0pg0wwi0kw955p9ifx00xs0nlvl5mc1hdy84rgqbal28bd2zyl2k"))))
    (arguments
     (list
      #:import-path "github.com/AvengeMedia/DankMaterialShell/core/cmd/dms"
      #:unpack-path "github.com/AvengeMedia/DankMaterialShell"
      ;; DMS v1.6.2's go.mod declares `go 1.26.5' -- it uses errors.AsType,
      ;; a generic errors.As variant only added in Go 1.26.  The default Go
      ;; go-build-system picks (1.25.x) is too old for it.
      #:go go-1.26
      #:tests? #f
      #:install-source? #f
      ;; Propagated dependencies (tailscale.com's VERSION.txt/ALPINE.txt,
      ;; chroma's embedded lexer/style data, ...) land in this build's
      ;; GOPATH as symlinks into other packages' store items, and Go's
      ;; //go:embed refuses to embed a symlinked file.  go-build-system's
      ;; 'fix-embed-files phase replaces symlinks matching #:embed-files
      ;; with real copies; match everything since only actual symlinks are
      ;; touched.
      #:embed-files #~(list ".*")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-config
            (lambda _
              (let* ((src (string-append #$source "/quickshell"))
                     (tgt (string-append #$output "/share/quickshell"))
                     (common-src (string-append #$source "/dank-qml-common"))
                     (common-tgt (string-append #$output "/share/dank-qml-common")))
                (mkdir-p tgt)
                (copy-recursively src tgt)
                (mkdir-p common-tgt)
                (copy-recursively common-src common-tgt)))))))
    (native-inputs
     (list go-github-com-charmbracelet-bubbles
           go-github-com-charmbracelet-bubbletea
           go-github-com-charmbracelet-lipgloss
           go-github-com-charmbracelet-log
           go-github-com-godbus-dbus-v5
           go-github-com-spf13-cobra
           go-github-com-stretchr-testify
           go-github-com-wifx-gonetworkmanager-v2
           go-github-com-yaslama-go-wayland-wayland
           go-golang-org-x-exp
           go-github-com-go-git-go-git-v6
           go-github-com-spf13-afero
           go-github-com-alecthomas-chroma-v2-2.27
           go-github-com-fsnotify-fsnotify-1.10
           go-github-com-holoplot-go-evdev
           go-github-com-pilebones-go-udev
           go-github-com-sblinch-kdl-go
           go-github-com-yeqown-go-qrcode
           go-github-com-yeqown-go-qrcode-writer-standard
           go-github-com-yuin-goldmark
           go-github-com-yuin-goldmark-highlighting-v2
           go-go-etcd-io-bbolt
           go-go4-org-mem
           go-golang-org-x-image-0.39
           go-tailscale-com
           ;; New in v1.6.x (see the source-only definitions above):
           go-github-com-avengemedia-dankgo
           go-github-com-avengemedia-dgop
           go-github-com-dlclark-regexp2-v2
           go-github-com-nadim147c-material-v3
           go-github-com-klauspost-compress-1.19
           go-github-com-danielgtaylor-huma-v2
           go-github-com-shirou-gopsutil-v4
           go-github-com-tklauser-go-sysconf
           go-github-com-tklauser-numcpus))))

(define-public dank-material-shell-1.5.0
  (package
    (inherit dank-material-shell-minimal-1.5.0)
    (name "dank-material-shell")
    (propagated-inputs
     (package-propagated-inputs dank-material-shell))))
