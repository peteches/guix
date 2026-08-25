;; peteches/packages/dank-material-shell.scm
;;
;; Guix's own `dank-material-shell'/`dank-material-shell-minimal' (in
;; gnu/packages/window-management.scm, via the `guix' channel) is pinned to
;; v0.5.1 (Nov 2025) and has never been bumped upstream.  That predates
;; DankMaterialShell's fix for Hyprland's Lua config migration (workspace
;; switcher clicks silently doing nothing) -- see PRs #2419 and #2443
;; upstream, first released in DMS v1.5.0.  This file bumps DMS to v1.5.0
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
;;   guix build -L ~/area_51/guix dank-material-shell@1.5.0

(define-module (peteches packages dank-material-shell)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
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

(define-public go-github-com-alecthomas-chroma-v2-2.24
  (package
    (inherit go-github-com-alecthomas-chroma-v2)
    (version "2.24.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alecthomas/chroma")
             (commit (string-append "v" version))))
       (file-name (git-file-name
                   (package-name go-github-com-alecthomas-chroma-v2) version))
       (sha256
        (base32 "0kww0yhwqzzgl8n3alislf0136rk7kh507bbvr32a30wmk5dzi81"))
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
           #:tests? #f))
    (native-inputs '())
    (propagated-inputs
     (list go-github-com-dlclark-regexp2-1.12))))

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

;;;
;;; DankMaterialShell itself, bumped to v1.5.0.
;;;

(define-public dank-material-shell-minimal-1.5.0
  (package
    (inherit dank-material-shell-minimal)
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AvengeMedia/DankMaterialShell")
             (commit (string-append "v" version))))
       (file-name (git-file-name "dank-material-shell-minimal" version))
       (sha256
        (base32 "14qh2ar16k2pvd2kfhv3yr6rqx7ig3g25bigzxicbr64ccyfrlfd"))))
    (arguments
     (list
      #:import-path "github.com/AvengeMedia/DankMaterialShell/core/cmd/dms"
      #:unpack-path "github.com/AvengeMedia/DankMaterialShell"
      ;; DMS v1.5.0's go.mod declares `go 1.26.1' -- it uses errors.AsType,
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
                     (tgt (string-append #$output "/share/quickshell")))
                (mkdir-p tgt)
                (copy-recursively src tgt)))))))
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
           go-github-com-alecthomas-chroma-v2-2.24
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
           go-tailscale-com))))

(define-public dank-material-shell-1.5.0
  (package
    (inherit dank-material-shell-minimal-1.5.0)
    (name "dank-material-shell")
    (propagated-inputs
     (package-propagated-inputs dank-material-shell))))
