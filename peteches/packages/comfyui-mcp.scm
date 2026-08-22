;; peteches/packages/comfyui-mcp.scm — the comfyui-mcp MCP server.
;;
;; Generated via `guix import npm-binary -r comfyui-mcp@0.38.1` and
;; hand-curated.  All but five of the transitive dependencies already
;; exist in the channel (see the #:use-module lines below); only the
;; packages defined here were missing.
;;
;; `node-better-sqlite3' is the interesting one: upstream ships an
;; install script that reaches for a prebuilt binary
;; (`prebuild-install || node-gyp rebuild --release').  Neither half
;; works in a Guix build — the first wants the network, the second is
;; not what npm runs from a tarball install.  We delete the install
;; script entirely so npm falls back to its *default* install script
;; for a package with a binding.gyp, namely `node-gyp rebuild', which
;; node-build-system's `install' phase runs inside the store output.
;; The SQLite amalgamation is vendored in deps/sqlite3, so the build
;; stays offline.

(define-module (peteches packages comfyui-mcp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system node)
  #:use-module (guix git-download)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (peteches packages seerr-deps)
  #:use-module (peteches packages claude-agent-acp))

(define-public node-better-sqlite3-13.0.3
  (package
    (name "node-better-sqlite3")
    (version "13.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/better-sqlite3/-/better-sqlite3-13.0.3.tgz")
       (sha256
        (base32 "1jv8d3apzhby0jz0bnkyg04k2h1zmnspyk7crqxznsd4q4ym3q3p"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "cli-color"
                                                  "fs-extra"
                                                  "mocha"
                                                  "nodemark"
                                                  "prebuild"
                                                  "sqlite"
                                                  "sqlite3"
                                                  ;; Only used by the
                                                  ;; install script we
                                                  ;; delete below.
                                                  "prebuild-install")))))
          ;; Drop `prebuild-install || node-gyp rebuild --release' so
          ;; npm supplies its default `node-gyp rebuild' instead, which
          ;; builds the addon from the vendored amalgamation.
          (add-after 'delete-dev-dependencies 'build-from-source
            (lambda _
              (modify-json (delete-fields '("scripts.install")))))
          ;; node-gyp's generated makefiles default CC/CXX to `cc'/`c++',
          ;; neither of which exists in a Guix build environment, and it
          ;; looks for Python on PATH rather than in the store.
          (add-before 'install 'set-node-gyp-toolchain
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((python (search-input-file inputs "/bin/python3")))
                (setenv "PYTHON" python)
                (setenv "npm_config_python" python))
              (setenv "CC" "gcc")
              (setenv "CXX" "g++")))
          ;; The addon is built above, into the store output.  But with
          ;; a binding.gyp present and no install script, npm supplies
          ;; its default `node-gyp rebuild' whenever a *dependent*
          ;; package installs this one — which would rebuild the addon
          ;; in the dependent's build environment, where neither Python
          ;; nor a compiler is available.  node-build-system's own
          ;; `avoid-node-gyp-rebuild' phase only rewrites an existing
          ;; install script, so there is nothing for it to neutralise
          ;; here; we install the no-op ourselves.
          (add-after 'install 'no-rebuild-for-dependents
            (lambda* (#:key outputs #:allow-other-keys)
              (modify-json #:file (search-input-file outputs
                                   "/lib/node_modules/better-sqlite3/package.json")
                           (lambda (meta)
                             ;; The "files" allowlist does not cover build/, so npm
                             ;; would drop the addon we just built when a dependent
                             ;; package installs this one.  Drop the allowlist.
                             (assoc-set! (assoc-remove! meta "files")
                                         "scripts"
                                         (assoc-set! (or (assoc-ref meta
                                                                    "scripts")
                                                         '()) "install"
                                          "echo Guix: addon already built")))))))))
    (native-inputs (list python))
    (inputs (list node-bindings-1.5.0))
    (home-page "http://github.com/WiseLibs/better-sqlite3")
    (synopsis "Fast and simple SQLite library for Node.js")
    (description
     "@code{better-sqlite3} provides synchronous bindings to SQLite for
Node.js.  This package builds the native addon from the SQLite
amalgamation vendored in the upstream tarball.")
    (license license:expat)))

(define-public node-stable-canvas-comfyui-client-1.5.9
  (package
    (name "node-stable-canvas-comfyui-client")
    (version "1.5.9")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@stable-canvas/comfyui-client/-/comfyui-client-1.5.9.tgz")
       (sha256
        (base32 "1w4j1bsg7c69k51grisdi2qfpiwr62pzrv7by8b19rbgd8j64d1r"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/jest" "@types/node"
                                                  "@types/ws"
                                                  "eventemitter3"
                                                  "jest"
                                                  "microbundle"
                                                  "prettier"
                                                  "rollup"
                                                  "rollup-plugin-dts"
                                                  "ts-jest"
                                                  "ts-morph"
                                                  "ts-node"
                                                  "tslib"
                                                  "typedoc"
                                                  "typescript"
                                                  "ws"))))))))
    (home-page "https://github.com/StableCanvas/comfyui-client#readme")
    (synopsis "ComfyUI API client for Node.js and the browser")
    (description "API client for ComfyUI supporting both Node.js and browser
environments, covering the RESTful and WebSocket APIs.")
    (license license:expat)))

;; `utf-8-validate' and `bufferutil' are optional peer dependencies of
;; ws — native accelerators that ws falls back away from when absent.
;; They are deliberately not packaged.
(define-public node-ws-8.21.3
  (package
    (name "node-ws")
    (version "8.21.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ws/-/ws-8.21.3.tgz")
       (sha256
        (base32 "1hrd1jn7vgi9f82x60bzkymf3gzrvii1f95rnm8cx4ap43pm8d6z"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "mocha"
                                                  "eslint"
                                                  "globals"
                                                  "prettier"
                                                  "benchmark"
                                                  "@eslint/js"
                                                  "bufferutil"
                                                  "utf-8-validate"
                                                  "eslint-config-prettier"
                                                  "eslint-plugin-prettier"))))))))
    (home-page "https://github.com/websockets/ws")
    (synopsis "WebSocket client and server for Node.js")
    (description
     "Simple to use, blazing fast and thoroughly tested WebSocket client
and server implementation for Node.js.")
    (license license:expat)))

(define-public node-comfyui-mcp
  (package
    (name "node-comfyui-mcp")
    (version "0.52.53")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/comfyui-mcp/-/comfyui-mcp-0.52.53.tgz")
       (sha256
        (base32 "0yv9237q0z0g68k9bl9v7brr7zm069hplqy7h2qc37m24zvhwji1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; dist/ ships prebuilt in the tarball, so the TypeScript
          ;; build is neither needed nor possible (no typescript here).
          (delete 'build)
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/better-sqlite3"
                                                  "@types/node"
                                                  "@types/ws"
                                                  "cross-env"
                                                  "tsx"
                                                  "typescript"
                                                  "vitest"
                                                  ;; Optional providers for the bundled agent sidebar.
                                                  ;; None are needed to serve MCP against a local
                                                  ;; ComfyUI, and packaging them would drag in most of
                                                  ;; the AI SDK ecosystem.
                                                  "@anthropic-ai/claude-agent-sdk"
                                                  "@openai/codex"
                                                  "@ai-sdk/anthropic"
                                                  "@ai-sdk/google"
                                                  "@ai-sdk/openai"
                                                  "@aws-sdk/client-s3"
                                                  "@azure/storage-blob"
                                                  "ai"
                                                  "cloudflared"))))))))
    (inputs (list node-zod-4.4.3
                  node-yaml-2.9.0
                  node-ws-8.21.3
                  node-sharp-native-0.35.3
                  node-dotenv-16.6.1
                  node-better-sqlite3-13.0.3
                  node-stable-canvas-comfyui-client-1.5.9
                  node-modelcontextprotocol-sdk-1.29.0))
    (home-page "https://comfyui-mcp.artokun.io/docs")
    (synopsis "MCP server for driving ComfyUI")
    (description
     "@code{comfyui-mcp} is a Model Context Protocol server that exposes a
running ComfyUI instance to MCP clients, letting an agent inspect and
drive the live graph.  It talks to ComfyUI over its REST and WebSocket
APIs; set @env{COMFYUI_URL} to point it at the instance.")
    (license license:expat)))

;; sharp needs libvips >= 8.17.3; Guix currently carries 8.17.0.  This
;; variant is local to this module precisely so that bumping it does not
;; rebuild every other vips consumer in the channel.
(define-public vips-for-sharp
  (package
    (inherit vips)
    (name "vips")
    (version "8.18.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libvips/libvips")
             (commit (string-append "v" version))))
       (file-name (git-file-name "vips" version))
       (sha256
        (base32 "07lh6lw6bcakg71zxc1ga868069w3q77z1cf76hrcf3ywmxgij0l"))))))

;; The channel's other node-sharp (in seerr-deps) deletes its build phase
;; and so ships no native binary at all — requiring it throws "Could not
;; load the sharp module using the linux-x64 runtime".  comfyui-mcp
;; imports sharp eagerly, so it needs one that actually builds.
;;
;; Upstream's default is to download a prebuilt libvips; we instead force
;; the "global libvips" path, where binding.gyp discovers vips through
;; pkg-config and links against it.
(define-public node-sharp-native-0.35.3
  (package
    (name "node-sharp")
    (version "0.35.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/sharp/-/sharp-0.35.3.tgz")
       (sha256
        (base32 "0m5f0qfymi50d3r5kxvypa99vrj49kwsdv4p42q107gq0dapyqsk"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json
               ;; Every optionalDependency is a prebuilt binary for some
               ;; platform, fetched from the registry.  We build instead.
               (delete-fields '("optionalDependencies")
                              #:strict? #f)
               ;; node-addon-api is deliberately *kept*: binding.gyp
               ;; resolves it at build time.  node-gyp is dropped so that
               ;; no node_modules/.bin/node-gyp shadows the newer copy
               ;; bundled with npm (see configure-native-build).
               (delete-dependencies '("icc" "tsd"
                                      "node-gyp"
                                      "emnapi"
                                      "tar-fs"
                                      "@types/node"
                                      "exif-reader"
                                      "extract-zip"
                                      "@cpplint/cli"
                                      "@biomejs/biome"
                                      "@emnapi/runtime"
                                      "jsdoc-to-markdown"
                                      "@img/sharp-libvips-dev"
                                      "@img/sharp-libvips-dev-wasm32"
                                      "@img/sharp-libvips-win32-x64"
                                      "@img/sharp-libvips-win32-ia32"
                                      "@img/sharp-libvips-win32-arm64")))))
          (add-before 'build 'configure-native-build
            (lambda* (#:key inputs #:allow-other-keys)
              ;; binding.gyp evaluates *all* of its variables before
              ;; picking a branch, including the prebuilt-libvips version
              ;; it reads out of optionalDependencies — which we deleted
              ;; so npm would not try to fetch those binaries.  Put a stub
              ;; back now that `npm install' has already run; only the
              ;; unused prebuilt branch ever reads it.
              (modify-json (lambda (meta)
                             (assoc-set! meta "optionalDependencies"
                                         (list (cons
                                                "@img/sharp-libvips-linux-x64"
                                                "1.2.4")))))
              (let ((python (search-input-file inputs "/bin/python3")))
                (setenv "PYTHON" python)
                (setenv "npm_config_python" python))
              (setenv "CC" "gcc")
              (setenv "CXX" "g++")
              ;; Skip the prebuilt-binary search entirely.
              (setenv "SHARP_FORCE_GLOBAL_LIBVIPS" "1")
              ;; The packaged node-gyp is 8.4.1, whose vendored gyp still
              ;; imports `distutils' — removed in Python 3.12.  npm ships
              ;; node-gyp 11, which does not.  Put it on PATH (sharp
              ;; spawns `node-gyp rebuild') and on NODE_PATH (install/
              ;; build.js requires it to report a version).
              (let* ((node (dirname (dirname (search-input-file inputs
                                                                "/bin/node"))))
                     (npm-modules (string-append node
                                   "/lib/node_modules/npm/node_modules"))
                     (bin (string-append (getcwd) "/../node-gyp-bin")))
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/node-gyp")
                  (lambda (port)
                    (format port
                     "#!/bin/sh
exec ~a/bin/node ~a/node-gyp/bin/node-gyp.js \"$@\"
" node
                     npm-modules)))
                (chmod (string-append bin "/node-gyp") #o755)
                (setenv "PATH"
                        (string-append bin ":"
                                       (getenv "PATH")))
                (setenv "NODE_PATH" npm-modules))))
          ;; As for better-sqlite3: stop npm re-running the install script
          ;; (`node install/check.js || npm run build') in the build
          ;; environment of every dependent package.
          (add-after 'install 'no-rebuild-for-dependents
            (lambda* (#:key outputs #:allow-other-keys)
              (modify-json #:file (search-input-file outputs
                                   "/lib/node_modules/sharp/package.json")
                           (lambda (meta)
                             ;; As for better-sqlite3: "files" omits src/build, where
                             ;; the addon lands, so dependents would install a sharp
                             ;; with no binary.
                             (assoc-set! (assoc-remove! meta "files")
                                         "scripts"
                                         (assoc-set! (or (assoc-ref meta
                                                                    "scripts")
                                                         '()) "install"
                                          "echo Guix: addon already built")))))))))
    (native-inputs (list pkg-config python))
    (inputs (list vips-for-sharp node-node-addon-api-8.8.0
                  node-detect-libc-2.1.2 node-img-colour-1.1.0
                  node-semver-7.8.2))
    (home-page "https://sharp.pixelplumbing.com")
    (synopsis "High performance Node.js image processing")
    (description
     "High performance Node.js image processing; the fastest module to
resize JPEG, PNG, WebP, GIF, AVIF and TIFF images.  This package links
against the system libvips rather than downloading a prebuilt copy.")
    (license license:asl2.0)))
