(define-module (peteches packages pi-coding-agent)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module (guix-science packages rstudio-node)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages xorg)
  ;; For node-standardwebhooks-1.0.0 (a runtime dependency of
  ;; node-anthropic-ai-sdk-0.124.0, needed by pi-ai 0.87.1).  That
  ;; module is self-contained and does not import this one, so no
  ;; circular dependency.
  #:use-module (peteches packages claude-agent-acp-deps))

;; pi (https://pi.dev), the AI coding-agent CLI from
;; @earendil-works/pi-mono (github.com/earendil-works/pi-mono), by
;; Mario Zechner.  The npm tarball ships prebuilt `dist/*.js` (ESM,
;; "type": "module") -- no TypeScript compile step is needed here,
;; just installing the tree and its node_modules dependency closure.
;;
;; Scope reduction: pi's model backend (@earendil-works/pi-ai)
;; lazy-loads every cloud-provider SDK via dynamic import() at the
;; point a given provider is actually used
;; (dist/api/*.lazy.js -- e.g. bedrock-converse-stream.lazy.js wraps
;; its @aws-sdk/client-bedrock-runtime import in an async lazyApi()
;; closure specifically so bundlers/Node don't need it at load time;
;; same pattern for google-generative-ai.lazy.js,
;; anthropic-messages.lazy.js, openai-completions.lazy.js, etc).  So
;; the @aws-sdk/client-bedrock-runtime and @google/genai closures
;; (~33 @smithy/* + aws-sdk/* packages, and google-genai's own
;; google-auth-library/gaxios/protobufjs/ws chain) are omitted here
;; entirely -- pi starts and runs fine without them; only invoking
;; the Bedrock or Gemini providers specifically would fail with a
;; module-not-found error.  @mariozechner/clipboard (a Rust NAPI
;; native addon with prebuilt per-OS/arch binaries for OS clipboard
;; access) is dropped too -- pi works fine without it, just no
;; clipboard copy/paste integration inside the TUI.
(define-public node-minimatch-10.2.6
  (package
    (name "node-minimatch")
    (version "10.2.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/minimatch/-/minimatch-10.2.6.tgz")
       (sha256
        (base32 "1y2m0dvd682xw3phaacnlxb1i54h2257pr17axk2k0m2fj02qgas"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's `prepare' script runs `tshy', which is not
          ;; available here and makes every dependent package fail
          ;; to install this one.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "tshy"
                                                  "mkdirp"
                                                  "oxlint"
                                                  "typedoc"
                                                  "prettier"
                                                  "@types/node"
                                                  "oxlint-tsgolint"))))))))
    (inputs (list node-brace-expansion-5.0.9))
    (home-page "https://github.com/isaacs/minimatch#readme")
    (synopsis "a glob matcher in javascript")
    (description "a glob matcher in javascript")
    (license license:blue-oak1.0.0)))

(define-public node-minipass-7.1.3
  (package
    (name "node-minipass")
    (version "7.1.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/minipass/-/minipass-7.1.3.tgz")
       (sha256
        (base32 "04kxs8if6f6vj9vkhhrnzp3y58fls2300mlfr7yy6m9pfjz63b2j"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's `prepare' script runs `tshy', which is not
          ;; available here and makes every dependent package fail
          ;; to install this one.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/end-of-stream"
                                                  "@types/node"
                                                  "end-of-stream"
                                                  "node-abort-controller"
                                                  "prettier"
                                                  "tap"
                                                  "through2"
                                                  "tshy"
                                                  "typedoc"))))))))
    (home-page "https://github.com/isaacs/minipass#readme")
    (synopsis "minimal implementation of a PassThrough stream")
    (description "minimal implementation of a PassThrough stream")
    (license license:blue-oak1.0.0)))

(define-public node-path-scurry-2.0.2
  (package
    (name "node-path-scurry")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/path-scurry/-/path-scurry-2.0.2.tgz")
       (sha256
        (base32 "1h9yq8i7j1hl3vixpqnx23hwfl10n0ri26w7npy2h360n7df78yy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's `prepare' script runs `tshy', which is not
          ;; available here and makes every dependent package fail
          ;; to install this one.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@nodelib/fs.walk"
                                                  "@types/node"
                                                  "mkdirp"
                                                  "prettier"
                                                  "rimraf"
                                                  "tap"
                                                  "ts-node"
                                                  "tshy"
                                                  "typedoc")))))
          ;; Skip '--install-links' and bin-links: npm's bundled
          ;; tar/minizlib crashes with a silently-swallowed "ZlibError:
          ;; zlib: stream error" while copying inputs via
          ;; '--install-links' on this node-24.18.0 toolchain.
          ;; Symlinking instead avoids the crash and is equivalent for
          ;; a build-time dependency; '--no-bin-links' avoids a related
          ;; EROFS chmod failure for any input with a "bin" script.
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "node") "/bin/npm")
                      "--offline" "--ignore-scripts" "--no-bin-links"
                      "--no-audit" "install")
              ;; npm's symlinks (created because '--install-links' is
              ;; skipped above) are relative to wherever the build
              ;; happens to sit right now.  This package still gets
              ;; tar'd up and reinstalled at a different depth by the
              ;; 'repack/'install phases below, which would silently
              ;; leave the relative text pointing at the wrong place
              ;; (e.g. into /tmp instead of /gnu/store) once any later
              ;; consumer copies this tree elsewhere.  Absolutize now,
              ;; while the relative targets are still resolvable.
              (for-each (lambda (f)
                          (unless (string-prefix? "/" (readlink f))
                            (let ((target (canonicalize-path f)))
                              (delete-file f)
                              (symlink target f))))
                        (find-files "node_modules"
                                    (lambda (f s)
                                      (eq? 'symlink (stat:type s)))))))
          (replace 'install
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "node") "/bin/npm")
                      "--prefix" (assoc-ref outputs "out")
                      "--global" "--offline" "--loglevel" "info"
                      "--production" "--no-bin-links"
                      "install" "../package.tgz")
              ;; See the matching comment in 'configure above: this
              ;; second npm invocation (extracting into the final
              ;; store output) creates its own fresh relative symlinks,
              ;; independent of whatever 'configure already fixed.
              ;; Absolutize them too, or any later consumer that copies
              ;; this output elsewhere inherits broken links.
              (let ((out (assoc-ref outputs "out")))
                (for-each (lambda (f)
                            (unless (string-prefix? "/" (readlink f))
                              (let ((target (canonicalize-path f)))
                                (delete-file f)
                                (symlink target f))))
                          (find-files out
                                      (lambda (f s)
                                        (eq? 'symlink (stat:type s)))))))))))
    (inputs (list node-minipass-7.1.3 node-lru-cache-11.5.2))
    (home-page "https://github.com/isaacs/path-scurry#readme")
    (synopsis "walk paths fast and efficiently")
    (description "walk paths fast and efficiently")
    (license license:blue-oak1.0.0)))

(define-public node-glob-13.0.6
  (package
    (name "node-glob")
    (version "13.0.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/glob/-/glob-13.0.6.tgz")
       (sha256
        (base32 "0w49ggh984wkrj0myy3gbzh5hnmjljxisg9wydf91qn4m57ih3q2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's `prepare' script runs `tshy && bash
          ;; scripts/build.sh', which is not runnable here and makes
          ;; every dependent package fail to install this one.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "esbuild"
                                                  "memfs"
                                                  "mkdirp"
                                                  "prettier"
                                                  "rimraf"
                                                  "tap"
                                                  "tshy"
                                                  "typedoc")))))
          ;; Skip '--install-links' and bin-links: npm's bundled
          ;; tar/minizlib crashes with a silently-swallowed "ZlibError:
          ;; zlib: stream error" while copying inputs via
          ;; '--install-links' on this node-24.18.0 toolchain.
          ;; Symlinking instead avoids the crash and is equivalent for
          ;; a build-time dependency; '--no-bin-links' avoids a related
          ;; EROFS chmod failure for any input with a "bin" script.
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "node") "/bin/npm")
                      "--offline" "--ignore-scripts" "--no-bin-links"
                      "--no-audit" "install")
              ;; npm's symlinks (created because '--install-links' is
              ;; skipped above) are relative to wherever the build
              ;; happens to sit right now.  This package still gets
              ;; tar'd up and reinstalled at a different depth by the
              ;; 'repack/'install phases below, which would silently
              ;; leave the relative text pointing at the wrong place
              ;; (e.g. into /tmp instead of /gnu/store) once any later
              ;; consumer copies this tree elsewhere.  Absolutize now,
              ;; while the relative targets are still resolvable.
              (for-each (lambda (f)
                          (unless (string-prefix? "/" (readlink f))
                            (let ((target (canonicalize-path f)))
                              (delete-file f)
                              (symlink target f))))
                        (find-files "node_modules"
                                    (lambda (f s)
                                      (eq? 'symlink (stat:type s)))))))
          (replace 'install
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "node") "/bin/npm")
                      "--prefix" (assoc-ref outputs "out")
                      "--global" "--offline" "--loglevel" "info"
                      "--production" "--no-bin-links"
                      "install" "../package.tgz")
              ;; See the matching comment in 'configure above: this
              ;; second npm invocation (extracting into the final
              ;; store output) creates its own fresh relative symlinks,
              ;; independent of whatever 'configure already fixed.
              ;; Absolutize them too, or any later consumer that copies
              ;; this output elsewhere inherits broken links.
              (let ((out (assoc-ref outputs "out")))
                (for-each (lambda (f)
                            (unless (string-prefix? "/" (readlink f))
                              (let ((target (canonicalize-path f)))
                                (delete-file f)
                                (symlink target f))))
                          (find-files out
                                      (lambda (f s)
                                        (eq? 'symlink (stat:type s)))))))))))
    (inputs (list node-path-scurry-2.0.2 node-minipass-7.1.3
                  node-minimatch-10.2.6))
    (home-page "https://github.com/isaacs/node-glob#readme")
    (synopsis
     "the most correct and second fastest glob implementation in JavaScript")
    (description
     "the most correct and second fastest glob implementation in JavaScript")
    (license license:blue-oak1.0.0)))

(define-public node-jiti-2.7.0
  (package
    (name "node-jiti")
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/jiti/-/jiti-2.7.0.tgz")
       (sha256
        (base32 "1ji8rzdyqd7w8r1hk4j4q9phba9dzgv0v86n8b6fgh29zdjx7l4d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/helper-module-imports"
                                                  "@babel/helper-module-transforms"
                                                  "@babel/helper-plugin-utils"
                                                  "@babel/helper-simple-access"
                                                  "@babel/plugin-proposal-decorators"
                                                  "@babel/plugin-syntax-class-properties"
                                                  "@babel/plugin-syntax-import-assertions"
                                                  "@babel/plugin-syntax-jsx"
                                                  "@babel/plugin-transform-explicit-resource-management"
                                                  "@babel/plugin-transform-export-namespace-from"
                                                  "@babel/plugin-transform-react-jsx"
                                                  "@babel/plugin-transform-typescript"
                                                  "@babel/preset-typescript"
                                                  "@babel/template"
                                                  "@babel/traverse"
                                                  "@babel/types"
                                                  "@rspack/cli"
                                                  "@rspack/core"
                                                  "@types/babel__core"
                                                  "@types/babel__helper-module-imports"
                                                  "@types/babel__helper-plugin-utils"
                                                  "@types/babel__template"
                                                  "@types/babel__traverse"
                                                  "@types/node"
                                                  "@typescript/native-preview"
                                                  "@vitest/coverage-v8"
                                                  "acorn"
                                                  "babel-plugin-parameter-decorator"
                                                  "changelogen"
                                                  "config"
                                                  "consola"
                                                  "defu"
                                                  "destr"
                                                  "escape-string-regexp"
                                                  "eslint"
                                                  "eslint-config-unjs"
                                                  "estree-walker"
                                                  "etag"
                                                  "fast-glob"
                                                  "get-tsconfig"
                                                  "is-installed-globally"
                                                  "mime"
                                                  "mitata"
                                                  "mlly"
                                                  "moment-timezone"
                                                  "nano-jsx"
                                                  "pathe"
                                                  "pkg-types"
                                                  "preact"
                                                  "preact-render-to-string"
                                                  "prettier"
                                                  "react"
                                                  "react-dom"
                                                  "reflect-metadata"
                                                  "rolldown"
                                                  "solid-js"
                                                  "std-env"
                                                  "terser-webpack-plugin"
                                                  "tinyexec"
                                                  "ts-loader"
                                                  "typescript"
                                                  "vitest"
                                                  "vue"
                                                  "yoctocolors"
                                                  "zod"))))))))
    (home-page "https://github.com/unjs/jiti#readme")
    (synopsis "Runtime typescript and ESM support for Node.js")
    (description "Runtime typescript and ESM support for Node.js")
    (license license:expat)))

(define-public node-chalk-5.6.2
  (package
    (name "node-chalk")
    (version "5.6.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/chalk/-/chalk-5.6.2.tgz")
       (sha256
        (base32 "1zagawvlzqw1xwp9hzs0bh1dh9w297aj53qcnsfkpal3lhapg5cl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "xo"
                                                  "ava"
                                                  "tsd"
                                                  "execa"
                                                  "matcha"
                                                  "log-update"
                                                  "yoctodelay"
                                                  "@types/node"
                                                  "color-convert"))))))))
    (home-page "https://github.com/chalk/chalk#readme")
    (synopsis "Terminal string styling done right")
    (description "Terminal string styling done right")
    (license license:expat)))

(define-public node-chalk-6.0.0
  (package
    (name "node-chalk")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/chalk/-/chalk-6.0.0.tgz")
       (sha256
        (base32 "168kk0f5hcqirig59wp3zink5af72v7igv6r4qw8s2d6p0kcxl45"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies
                            '("@types/node" "ansi-styles" "ava"
                              "c8" "color-convert" "execa"
                              "log-update" "matcha" "typescript"
                              "xo" "yoctodelay"))))))))
    (home-page "https://github.com/chalk/chalk#readme")
    (synopsis "Terminal string styling done right")
    (description "Terminal string styling done right")
    (license license:expat)))

(define-public node-semver-7.8.5
  (package
    (name "node-semver")
    (version "7.8.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/semver/-/semver-7.8.5.tgz")
       (sha256
        (base32 "0pbx2afqpl5na8j25kgz496g6wrfaggrasrkj745fz8d63a4al6q"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "benchmark"
                                                  "@npmcli/template-oss"
                                                  "@npmcli/eslint-config"))))))))
    (home-page "https://github.com/npm/node-semver#readme")
    (synopsis "The semantic version parser used by npm.")
    (description "The semantic version parser used by npm.")
    (license license:isc)))

(define-public node-undici-8.10.2
  (package
    (name "node-undici")
    (version "8.10.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/undici/-/undici-8.10.2.tgz")
       (sha256
        (base32 "1ail8vmszm0fwsba0gwihzxbc9pscpim0ybjlr32d3fp6ap3h1kl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's `prepare' script runs `husky', a git-hooks
          ;; installer that is not runnable here and makes every
          ;; dependent package fail to install this one.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("c8" "ws"
                                                  "tsd"
                                                  "borp"
                                                  "jest"
                                                  "husky"
                                                  "proxy"
                                                  "eslint"
                                                  "esbuild"
                                                  "cross-env"
                                                  "dns-packet"
                                                  "fast-check"
                                                  "node-forge"
                                                  "typescript"
                                                  "@types/node"
                                                  "neostandard"
                                                  "jsondiffpatch"
                                                  "@fastify/busboy"
                                                  "abort-controller"
                                                  "@matteo.collina/tspl"
                                                  "@sinonjs/fake-timers"
                                                  "@metcoder95/https-pem"))))))))
    (home-page "https://undici.nodejs.org")
    (synopsis "An HTTP/1.1 client, written from scratch for Node.js")
    (description "An HTTP/1.1 client, written from scratch for Node.js")
    (license license:expat)))

(define-public node-brace-expansion-5.0.9
  (package
    (name "node-brace-expansion")
    (version "5.0.9")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/brace-expansion/-/brace-expansion-5.0.9.tgz")
       (sha256
        (base32 "1byvabmks50gs4l1w563hjwi2xxmnz2lvnwn1klvwp6jvlgh01jx"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's `prepare' script runs `tshy', which is not
          ;; available here and makes every dependent package fail
          ;; to install this one.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "tshy"
                                                  "mkdirp"
                                                  "typedoc"
                                                  "prettier"
                                                  "@types/node"
                                                  "@types/brace-expansion"))))))))
    (inputs (list node-balanced-match-4.0.4))
    (home-page "https://github.com/juliangruber/brace-expansion#readme")
    (synopsis "Brace expansion as known from sh/bash")
    (description "Brace expansion as known from sh/bash")
    (license license:expat)))

(define-public node-minimatch-10.2.6
  (package
    (name "node-minimatch")
    (version "10.2.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/minimatch/-/minimatch-10.2.6.tgz")
       (sha256
        (base32 "1y2m0dvd682xw3phaacnlxb1i54h2257pr17axk2k0m2fj02qgas"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's `prepare' script runs `tshy', which is not
          ;; available here and makes every dependent package fail
          ;; to install this one.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "tshy"
                                                  "mkdirp"
                                                  "oxlint"
                                                  "typedoc"
                                                  "prettier"
                                                  "@types/node"
                                                  "oxlint-tsgolint"))))))))
    (inputs (list node-brace-expansion-5.0.9))
    (home-page "https://github.com/isaacs/minimatch#readme")
    (synopsis "a glob matcher in javascript")
    (description "a glob matcher in javascript")
    (license license:blue-oak1.0.0)))

(define-public node-grok-mermaid-0.2.3
  (package
    (name "node-grok-mermaid")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/grok-mermaid/-/grok-mermaid-0.2.3.tgz")
       (sha256
        (base32 "124c1lpl9q2wciip0fnpjphfrmiq32qa0vfbka3gwq9jymij1l21"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/bun"
                                                  "@biomejs/biome"
                                                  "@typescript/native-preview"))))))))
    (home-page "https://github.com/xl0/grok-mermaid#readme")
    (synopsis
     "Render Mermaid diagrams as Unicode box-drawing art for terminals")
    (description
     "Render Mermaid diagrams as Unicode box-drawing art for terminals")
    (license license:asl2.0)))

(define-public node-lru-cache-11.5.2
  (package
    (name "node-lru-cache")
    (version "11.5.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/lru-cache/-/lru-cache-11.5.2.tgz")
       (sha256
        (base32 "08kaa791ycz2ac32ipism4wyqywq1vyrrlzb79h6iwb4zjm8wv74"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's `prepare' script runs `tshy && bash
          ;; scripts/build.sh', which is not runnable here and makes
          ;; every dependent package fail to install this one.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("benchmark" "esbuild"
                                                  "marked"
                                                  "mkdirp"
                                                  "oxlint"
                                                  "oxlint-tsgolint"
                                                  "prettier"
                                                  "tap"
                                                  "tshy"
                                                  "typedoc"))))))))
    (home-page "https://github.com/isaacs/node-lru-cache#readme")
    (synopsis "A cache object that deletes the least-recently-used items.")
    (description "A cache object that deletes the least-recently-used items.")
    (license license:blue-oak1.0.0)))

(define-public node-hosted-git-info-9.0.3
  (package
    (name "node-hosted-git-info")
    (version "9.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/hosted-git-info/-/hosted-git-info-9.0.3.tgz")
       (sha256
        (base32 "0wxnrvdfn0sm0kfg3ama55xii7rsf3ak9jy2hb264qizb7x1rfmm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@npmcli/template-oss"
                                                  "@npmcli/eslint-config")))))
          ;; Skip '--install-links' and bin-links: npm's bundled
          ;; tar/minizlib crashes with a silently-swallowed "ZlibError:
          ;; zlib: stream error" while copying inputs via
          ;; '--install-links' on this node-24.18.0 toolchain.
          ;; Symlinking instead avoids the crash and is equivalent for
          ;; a build-time dependency; '--no-bin-links' avoids a related
          ;; EROFS chmod failure for any input with a "bin" script.
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "node") "/bin/npm")
                      "--offline" "--ignore-scripts" "--no-bin-links"
                      "--no-audit" "install")
              ;; npm's symlinks (created because '--install-links' is
              ;; skipped above) are relative to wherever the build
              ;; happens to sit right now.  This package still gets
              ;; tar'd up and reinstalled at a different depth by the
              ;; 'repack/'install phases below, which would silently
              ;; leave the relative text pointing at the wrong place
              ;; (e.g. into /tmp instead of /gnu/store) once any later
              ;; consumer copies this tree elsewhere.  Absolutize now,
              ;; while the relative targets are still resolvable.
              (for-each (lambda (f)
                          (unless (string-prefix? "/" (readlink f))
                            (let ((target (canonicalize-path f)))
                              (delete-file f)
                              (symlink target f))))
                        (find-files "node_modules"
                                    (lambda (f s)
                                      (eq? 'symlink (stat:type s)))))))
          (replace 'install
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "node") "/bin/npm")
                      "--prefix" (assoc-ref outputs "out")
                      "--global" "--offline" "--loglevel" "info"
                      "--production" "--no-bin-links"
                      "install" "../package.tgz")
              ;; See the matching comment in 'configure above: this
              ;; second npm invocation (extracting into the final
              ;; store output) creates its own fresh relative symlinks,
              ;; independent of whatever 'configure already fixed.
              ;; Absolutize them too, or any later consumer that copies
              ;; this output elsewhere inherits broken links.
              (let ((out (assoc-ref outputs "out")))
                (for-each (lambda (f)
                            (unless (string-prefix? "/" (readlink f))
                              (let ((target (canonicalize-path f)))
                                (delete-file f)
                                (symlink target f))))
                          (find-files out
                                      (lambda (f s)
                                        (eq? 'symlink (stat:type s)))))))))))
    (inputs (list node-lru-cache-11.5.2))
    (home-page "https://github.com/npm/hosted-git-info")
    (synopsis
     "Provides metadata and conversions from repository urls for GitHub, Bitbucket and GitLab")
    (description
     "Provides metadata and conversions from repository urls for GitHub, Bitbucket and GitLab")
    (license license:isc)))

(define-public node-marked-18.0.5
  (package
    (name "node-marked")
    (version "18.0.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/marked/-/marked-18.0.5.tgz")
       (sha256
        (base32 "1hnsg6i2n71ha8p62a3hr8cnh4w9h2nn4pk6qpgfdh19iipsa3kx"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tslib" "eslint"
                                                  "rimraf"
                                                  "cheerio"
                                                  "esbuild"
                                                  "recheck"
                                                  "titleize"
                                                  "cross-env"
                                                  "commonmark"
                                                  "marked-man"
                                                  "typescript"
                                                  "markdown-it"
                                                  "highlight.js"
                                                  "marked-highlight"
                                                  "semantic-release"
                                                  "@markedjs/testutils"
                                                  "dts-bundle-generator"
                                                  "@arethetypeswrong/cli"
                                                  "@semantic-release/git"
                                                  "@semantic-release/npm"
                                                  "@markedjs/eslint-config"
                                                  "@semantic-release/github"
                                                  "esbuild-plugin-umd-wrapper"
                                                  "@semantic-release/commit-analyzer"
                                                  "@semantic-release/release-notes-generator"))))))))
    (home-page "https://marked.js.org")
    (synopsis "A markdown parser built for speed")
    (description "A markdown parser built for speed")
    (license license:expat)))

(define-public node-marked-18.0.11
  (package
    (name "node-marked")
    (version "18.0.11")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/marked/-/marked-18.0.11.tgz")
       (sha256
        (base32 "155rcvljxqk6s957g70fjvwyxxz9zrlzl64im3hp67jmqxv59xff"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies
                            '("@arethetypeswrong/cli"
                              "@markedjs/eslint-config"
                              "@markedjs/testutils"
                              "@semantic-release/commit-analyzer"
                              "@semantic-release/git"
                              "@semantic-release/github"
                              "@semantic-release/npm"
                              "@semantic-release/release-notes-generator"
                              "cheerio"
                              "commonmark"
                              "cross-env"
                              "dts-bundle-generator"
                              "esbuild"
                              "esbuild-plugin-umd-wrapper"
                              "eslint"
                              "highlight.js"
                              "markdown-it"
                              "marked-highlight"
                              "marked-man"
                              "recheck"
                              "rimraf"
                              "semantic-release"
                              "titleize"
                              "tslib"
                              "typescript"))))))))
    (home-page "https://marked.js.org")
    (synopsis "A markdown parser built for speed")
    (description "A markdown parser built for speed")
    (license license:expat)))

(define-public node-get-east-asian-width-1.6.0
  (package
    (name "node-get-east-asian-width")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/get-east-asian-width/-/get-east-asian-width-1.6.0.tgz")
       (sha256
        (base32 "1mz6xqcd840s6aj191nw2f23lzg99fv2bfmlx1xg4kjs5y7gala4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ava" "outdent"
                                                  "simplify-ranges"
                                                  "typescript" "xo"))))))))
    (home-page "https://github.com/sindresorhus/get-east-asian-width#readme")
    (synopsis "Determine the East Asian Width of a Unicode character")
    (description "Determine the East Asian Width of a Unicode character")
    (license license:expat)))

(define-public node-earendil-works-chord-0.87.1
  (package
    (name "node-earendil-works-chord")
    (version "0.87.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/chord/-/chord-0.87.1.tgz")
       (sha256
        (base32 "1x29wwn1ks9r493hps8csmav6mz0cdp65xlahmzmpp2qr08jj6sd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("shx" "vitest"))))))))
    (inputs (list node-esbuild-0.28.2))
    (home-page "https://github.com/earendil-works/pi#readme")
    (synopsis
     "Code orchestration and bundling utilities for pi, built on esbuild")
    (description
     "Code orchestration and bundling utilities for pi, built on esbuild.")
    (license license:expat)))

(define-public node-esbuild-0.28.2
  (package
    (name "node-esbuild")
    (version "0.28.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/esbuild/-/esbuild-0.28.2.tgz")
       (sha256
        (base32 "124sjlciwz2zhjkw6ivbdd4ipi5yqxwsb8kvwx8dqyjw4d6gjig0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-before 'configure 'force-ignore-scripts
            (lambda _
              ;; Ensure npm lifecycle scripts never run during the build
              ;; (the `postinstall' script runs install.js, which
              ;; verifies/downloads platform binaries).
              (setenv "npm_config_ignore_scripts" "true")))
          ;; Upstream's `postinstall' script runs install.js; strip it
              ;; from the repacked tarball too, so no consumer that
              ;; installs this item with scripts enabled re-runs it.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.postinstall")
                                          #:strict? #f)))))))
    (home-page "https://github.com/evanw/esbuild#readme")
    (synopsis "An extremely fast JavaScript and CSS bundler and minifier.")
    (description "An extremely fast JavaScript and CSS bundler and minifier.")
    (license license:expat)))

(define-public node-earendil-works-pi-tui-0.87.1
  (package
    (name "node-earendil-works-pi-tui")
    (version "0.87.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.87.1.tgz")
       (sha256
        (base32 "0f5ymw1jgsd5x5klq961wymw4pwpypqcfd7077x3w04n9461a8v7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; The npm tarball ships prebuilt native `.node` binaries
          ;; (native/linux/prebuilds/*/linux-platform-x11.node) with an
          ;; empty DT_RUNPATH, so the standard RUNPATH validation fails
          ;; on their libxcb dependency.  libxcb is in `inputs` so the
          ;; library is present in the runtime closure; the dynamic
          ;; linker resolves it via LD_LIBRARY_PATH at dlopen time.
          (delete 'validate-runpath)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chalk" "@xterm/headless"))))))))
    (inputs (list node-get-east-asian-width-1.6.0 node-marked-18.0.11 libxcb))
    (home-page "https://github.com/earendil-works/pi#readme")
    (synopsis
     "Terminal User Interface library with differential rendering for efficient text-based applications")
    (description
     "Terminal User Interface library with differential rendering for efficient text-based applications")
    (license license:expat)))

(define-public node-earendil-works-pi-client-0.87.1
  (package
    (name "node-earendil-works-pi-client")
    (version "0.87.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-client/-/pi-client-0.87.1.tgz")
       (sha256
        (base32 "18kahvn3494m8ymsxs4y0s9sh893a72snl3n9x182fgpcg23pff1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("shx" "vitest"))))))))
    (inputs (list node-earendil-works-chord-0.87.1
                  node-earendil-works-pi-protocol-0.87.1))
    (home-page "https://github.com/earendil-works/pi#readme")
    (synopsis
     "Transport-neutral client for remote pi sessions over framed CBOR bytes")
    (description
     "Transport-neutral client for remote pi sessions over framed CBOR bytes")
    (license license:expat)))

(define-public node-silvia-odwyer-photon-node-0.3.4
  (package
    (name "node-silvia-odwyer-photon-node")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@silvia-odwyer/photon-node/-/photon-node-0.3.4.tgz")
       (sha256
        (base32 "02n55fjxr7nj92whzzyja8pvjisjk0lnkn9d94y8xhyj3if028ss"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://silvia-odwyer.github.io/photon/")
    (synopsis
     "High-performance image processing library for native use and the web")
    (description
     "High-performance image processing library for native use and the web")
    (license license:asl2.0)))

(define-public node-earendil-works-pi-protocol-0.87.1
  (package
    (name "node-earendil-works-pi-protocol")
    (version "0.87.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-protocol/-/pi-protocol-0.87.1.tgz")
       (sha256
        (base32 "11arzzaqzx3glz3sa25dnlzqhh0a5wz9j7i14idn911kdrn5wp8c"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("shx" "vitest"))))))))
    (inputs (list node-earendil-works-chord-0.87.1 node-typebox-1.3.27))
    (home-page "https://github.com/earendil-works/pi#readme")
    (synopsis "Transport-neutral CBOR protocol for remote pi sessions")
    (description "Transport-neutral CBOR protocol for remote pi sessions")
    (license license:expat)))

(define-public node-diff-8.0.4
  (package
    (name "node-diff")
    (version "8.0.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/diff/-/diff-8.0.4.tgz")
       (sha256
        (base32 "1mlkjmimccf2yw8wrbqgpn0940m5p9g9y8966d9wyhw2smx34lgg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "tsd"
                                                  "chai"
                                                  "karma"
                                                  "mocha"
                                                  "eslint"
                                                  "rollup"
                                                  "globals"
                                                  "webpack"
                                                  "cross-env"
                                                  "uglify-js"
                                                  "@eslint/js"
                                                  "typescript"
                                                  "@babel/core"
                                                  "karma-mocha"
                                                  "babel-loader"
                                                  "karma-webpack"
                                                  "@colors/colors"
                                                  "@babel/register"
                                                  "@babel/preset-env"
                                                  "typescript-eslint"
                                                  "webpack-dev-server"
                                                  "karma-mocha-reporter"
                                                  "@arethetypeswrong/cli"
                                                  "babel-plugin-istanbul"
                                                  "karma-sourcemap-loader"))))))))
    (home-page "https://www.npmjs.com/package/node-diff")
    (synopsis "A JavaScript text diff implementation.")
    (description "A JavaScript text diff implementation.")
    (license license:bsd-3)))

(define-public node-yaml-2.9.0
  (package
    (name "node-yaml")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/yaml/-/yaml-2.9.0.tgz")
       (sha256
        (base32 "14ggs4m6rb3wm5mi9s2n6kkgz9h9pymlb81b4zh019qvrc2a53q0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/core"
                                                  "@babel/plugin-transform-typescript"
                                                  "@babel/preset-env"
                                                  "@eslint/js"
                                                  "@rollup/plugin-babel"
                                                  "@rollup/plugin-replace"
                                                  "@rollup/plugin-typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "babel-jest"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "fast-check"
                                                  "jest"
                                                  "jest-resolve"
                                                  "jest-ts-webcompat-resolver"
                                                  "prettier"
                                                  "rollup"
                                                  "tslib"
                                                  "typescript"
                                                  "typescript-eslint"))))))))
    (home-page "https://eemeli.org/yaml/")
    (synopsis "JavaScript parser and stringifier for YAML")
    (description "JavaScript parser and stringifier for YAML")
    (license license:isc)))

(define-public node-ignore-7.0.8
  (package
    (name "node-ignore")
    (version "7.0.8")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ignore/-/ignore-7.0.8.tgz")
       (sha256
        (base32 "0g1w3m9vs05xaq69xfsvwmmqpb7my5vkwanyfnf3zm70vs14hv25"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tap" "tmp"
                                                  "debug"
                                                  "eslint"
                                                  "mkdirp"
                                                  "rimraf"
                                                  "pre-suf"
                                                  "ts-node"
                                                  "@babel/cli"
                                                  "spawn-sync"
                                                  "typescript"
                                                  "@babel/core"
                                                  "@babel/preset-env"
                                                  "eslint-config-ostai"
                                                  "eslint-plugin-import"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (home-page "https://github.com/kaelzhang/node-ignore#readme")
    (synopsis
     "Ignore is a manager and filter for .gitignore rules, the one used by eslint, gitbook and many others.")
    (description
     "Ignore is a manager and filter for .gitignore rules, the one used by eslint, gitbook and many others.")
    (license license:expat)))

(define-public node-openai-6.40.0
  (package
    (name "node-openai")
    (version "6.40.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/openai/-/openai-6.40.0.tgz")
       (sha256
        (base32 "16q54kyb1nbylq7vgl1bns9bq7j3xi9i40l2gbkih9qa0ryyjpn2"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ws" "zod"))))))))
    (home-page "https://github.com/openai/openai-node#readme")
    (synopsis "The official TypeScript library for the OpenAI API")
    (description "The official TypeScript library for the OpenAI API")
    (license license:asl2.0)))

(define-public node-typebox-1.3.27
  (package
    (name "node-typebox")
    (version "1.3.27")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/typebox/-/typebox-1.3.27.tgz")
       (sha256
        (base32 "18yx379gsc6vqdmq80innpazfpi09ph8bcc4vrklbx387p6qna2s"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/sinclairzx81/typebox#readme")
    (synopsis
     "Json Schema Type Builder with Static Type Resolution for TypeScript")
    (description
     "Json Schema Type Builder with Static Type Resolution for TypeScript")
    (license license:expat)))

(define-public node-partial-json-0.1.7
  (package
    (name "node-partial-json")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/partial-json/-/partial-json-0.1.7.tgz")
       (sha256
        (base32 "08k35xv5dhx2k0mlamp1yl5qzyfrjrvw6d2gl8ngn9fwdznzxsih"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@vitest/coverage-istanbul"
                                                  "@vitest/ui" "typescript"
                                                  "vitest"))))))))
    (home-page "https://promplate.dev/partial-json-parser")
    (synopsis "Parse partial JSON generated by LLM")
    (description "Parse partial JSON generated by LLM")
    (license license:expat)))

(define-public node-agent-base-7.1.4
  (package
    (name "node-agent-base")
    (version "7.1.4")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/agent-base/-/agent-base-7.1.4.tgz")
       (sha256
        (base32 "0zmmkk3xhnkwb6djnvri7zyxllsdz5aa5w83x7afi959d0badm3x"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ws" "jest"
                                                  "ts-jest"
                                                  "tsconfig"
                                                  "@types/ws"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@types/debug"
                                                  "async-listen"
                                                  "@types/semver"))))))))
    (home-page "https://github.com/TooTallNate/proxy-agents#readme")
    (synopsis "Turn a function into an `http.Agent` instance")
    (description "Turn a function into an `http.Agent` instance")
    (license license:expat)))

(define-public node-http-proxy-agent-7.0.2
  (package
    (name "node-http-proxy-agent")
    (version "7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/http-proxy-agent/-/http-proxy-agent-7.0.2.tgz")
       (sha256
        (base32 "00kgi96l0vs04g2vl2xw3g53saxb4n9za2x23pbaiyrbm7x76pvq"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "proxy"
                                                  "ts-jest"
                                                  "tsconfig"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@types/debug"
                                                  "async-listen"))))))))
    (inputs (list node-agent-base-7.1.4 node-debug-4.4.3))
    (home-page "https://github.com/TooTallNate/proxy-agents#readme")
    (synopsis "An HTTP(s) proxy `http.Agent` implementation for HTTP")
    (description "An HTTP(s) proxy `http.Agent` implementation for HTTP")
    (license license:expat)))

(define-public node-agent-base-9.0.0
  (package
    (name "node-agent-base")
    (version "9.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/agent-base/-/agent-base-9.0.0.tgz")
       (sha256
        (base32 "17x8rcirms5s8d21m7ldrfzkn3nvx3c6prhxk54npy2gjb7gwq93"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies
                            '("@types/debug" "@types/node" "@types/semver"
                              "@types/ws" "async-listen" "typescript"
                              "ws" "tsconfig"))))))))
    (home-page "https://github.com/TooTallNate/proxy-agents#readme")
    (synopsis "Turn a callable into an http.Agent")
    (description "Turn a callable into an http.Agent.")
    (license license:expat)))

(define-public node-proxy-agent-negotiate-1.1.0
  (package
    (name "node-proxy-agent-negotiate")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/proxy-agent-negotiate/-/proxy-agent-negotiate-1.1.0.tgz")
       (sha256
        (base32 "15qrra5km7sjly0mjs8q6r32w19sdn3ij779mzzv1a41sgaj7v3f"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies
                            '("@types/node" "typescript" "tsconfig"
                              ;; Optional peerDependency; npm's offline
                              ;; install fails with ENOTCACHED when it
                              ;; cannot resolve it, so strip it.
                              "kerberos"))))))))
    (home-page "https://github.com/TooTallNate/proxy-agents#readme")
    (synopsis "Negotiate the proxy agent to use for a given URL")
    (description "Negotiate the proxy agent to use for a given URL.")
    (license license:expat)))

(define-public node-http-proxy-agent-9.1.0
  (package
    (name "node-http-proxy-agent")
    (version "9.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/http-proxy-agent/-/http-proxy-agent-9.1.0.tgz")
       (sha256
        (base32 "0vd7jypljz7ak2zy5hlrgagq8qvxys3pvc6nhkg9pgpa6z2z3329"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies
                            '("@types/debug" "@types/node" "async-listen"
                              "typescript" "tsconfig" "proxy"))))))))
    (inputs (list node-debug-4.4.3 node-agent-base-9.0.0
                  node-proxy-agent-negotiate-1.1.0))
    (home-page "https://github.com/TooTallNate/proxy-agents#readme")
    (synopsis "An HTTP(s) proxy `http.Agent` implementation for HTTP")
    (description "An HTTP(s) proxy `http.Agent` implementation for HTTP.")
    (license license:expat)))

(define-public node-https-proxy-agent-9.1.0
  (package
    (name "node-https-proxy-agent")
    (version "9.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/https-proxy-agent/-/https-proxy-agent-9.1.0.tgz")
       (sha256
        (base32 "1va0x7fvaa15xfnxk3zcbb5f8iymrdfkgklnhd76m48qsaw8cpzh"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies
                            '("@types/async-retry" "@types/debug"
                              "@types/node" "async-listen" "async-retry"
                              "typescript" "proxy" "tsconfig"))))))))
    (inputs (list node-debug-4.4.3 node-agent-base-9.0.0
                  node-proxy-agent-negotiate-1.1.0))
    (home-page "https://github.com/TooTallNate/proxy-agents#readme")
    (synopsis "An HTTP(s) proxy `https.Agent` implementation for HTTPS")
    (description "An HTTP(s) proxy `https.Agent` implementation for HTTPS.")
    (license license:expat)))

(define-public node-zod-4.4.3
  (package
    (name "node-zod")
    (version "4.4.3")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/zod/-/zod-4.4.3.tgz")
       (sha256
        (base32 "17171zbchqs56621d99kxgs2cg215yp879450rhh1m9zadzz2f7f"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://zod.dev")
    (synopsis
     "TypeScript-first schema declaration and validation library with static type inference")
    (description
     "TypeScript-first schema declaration and validation library with static type inference")
    (license license:expat)))

(define-public node-babel-runtime-7.29.7
  (package
    (name "node-babel-runtime")
    (version "7.29.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@babel/runtime/-/runtime-7.29.7.tgz")
       (sha256
        (base32 "19csnq2xy2ny8kr03vfjcq35z1l46b8kiiswc93lv9m10bainzsd"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://babel.dev/docs/en/next/babel-runtime")
    (synopsis "babel's modular runtime helpers")
    (description "babel's modular runtime helpers")
    (license license:expat)))

(define-public node-ts-algebra-2.0.0
  (package
    (name "node-ts-algebra")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ts-algebra/-/ts-algebra-2.0.0.tgz")
       (sha256
        (base32 "0p669fivm6k85ip9n54rv6bh70lcalrv43l6jhp9bdqyv27rdhzl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@trivago/prettier-plugin-sort-imports"
                                                  "@types/node"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "@zerollup/ts-transform-paths"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "eslint-import-resolver-typescript"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-jsdoc"
                                                  "eslint-plugin-prefer-arrow"
                                                  "eslint-plugin-prettier"
                                                  "eslint-plugin-unused-imports"
                                                  "prettier"
                                                  "rollup"
                                                  "rollup-plugin-dts"
                                                  "rollup-plugin-import-map"
                                                  "ts-node"
                                                  "ts-toolbelt"
                                                  "ts-unused-exports"
                                                  "ttypescript"
                                                  "typescript"))))))))
    (home-page "https://github.com/ThomasAribart/ts-algebra#readme")
    (synopsis "Types on steroids ð")
    (description "Types on steroids ð")
    (license license:expat)))

(define-public node-json-schema-to-ts-3.1.1
  (package
    (name "node-json-schema-to-ts")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/json-schema-to-ts/-/json-schema-to-ts-3.1.1.tgz")
       (sha256
        (base32 "0r639hff6d5z17lzkzqb0c8p1blpl97ssi7nx59qgjis8m74xwzn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@babel/cli" "@babel/core"
                                                  "@babel/plugin-transform-runtime"
                                                  "@babel/preset-env"
                                                  "@babel/preset-typescript"
                                                  "@rollup/plugin-typescript"
                                                  "@trivago/prettier-plugin-sort-imports"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "@zerollup/ts-transform-paths"
                                                  "ajv"
                                                  "babel-plugin-module-resolver"
                                                  "dependency-cruiser"
                                                  "eslint"
                                                  "eslint-config-prettier"
                                                  "eslint-import-resolver-typescript"
                                                  "eslint-plugin-import"
                                                  "eslint-plugin-jest"
                                                  "eslint-plugin-jsdoc"
                                                  "eslint-plugin-prefer-arrow"
                                                  "eslint-plugin-prettier"
                                                  "eslint-plugin-unused-imports"
                                                  "jest"
                                                  "prettier"
                                                  "rollup"
                                                  "rollup-plugin-dts"
                                                  "rollup-plugin-import-map"
                                                  "rollup-plugin-typescript-paths"
                                                  "ts-jest"
                                                  "ts-node"
                                                  "ts-toolbelt"
                                                  "ts-unused-exports"
                                                  "tsc-alias"
                                                  "typescript"))))))))
    (inputs (list node-ts-algebra-2.0.0 node-babel-runtime-7.29.7))
    (home-page "https://github.com/ThomasAribart/json-schema-to-ts#readme")
    (synopsis "Infer typescript types from your JSON schemas!")
    (description "Infer typescript types from your JSON schemas!")
    (license license:expat)))

(define-public node-anthropic-ai-sdk-0.91.1
  (package
    (name "node-anthropic-ai-sdk")
    (version "0.91.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@anthropic-ai/sdk/-/sdk-0.91.1.tgz")
       (sha256
        (base32 "1hpv308b2yvcn35abq8nsb5zc30bcmxdxshgr3xhv60py86dj75w"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("zod"))))))))
    (inputs (list node-json-schema-to-ts-3.1.1 node-zod-4.4.3))
    (home-page "https://www.npmjs.com/package/node-anthropic-ai-sdk")
    (synopsis "The official TypeScript library for the Anthropic API")
    (description "The official TypeScript library for the Anthropic API")
    (license license:expat)))

(define-public node-anthropic-ai-sdk-0.124.0
  (package
    (name "node-anthropic-ai-sdk")
    (version "0.124.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@anthropic-ai/sdk/-/sdk-0.124.0.tgz")
       (sha256
        (base32 "1cmfv97wkqb7q1jhjld54hfd0icrnl1vcblq9sj4qkpwb138knd9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-optional-peer
            (lambda _
              (modify-json (delete-dependencies
                            ;; Optional peerDependency; npm's offline
                            ;; install fails with ENOTCACHED when it
                            ;; cannot resolve it, so strip it.
                            '("zod"))))))))
    (inputs (list node-standardwebhooks-1.0.0 node-json-schema-to-ts-3.1.1))
    (home-page "https://www.npmjs.com/package/node-anthropic-ai-sdk")
    (synopsis "The official TypeScript library for the Anthropic API")
    (description "The official TypeScript library for the Anthropic API")
    (license license:expat)))

(define-public node-opentelemetry-api-1.9.0
  (package
    (name "node-opentelemetry-api")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/@opentelemetry/api/-/api-1.9.0.tgz")
       (sha256
        (base32 "12yslgc9dpvx2kcmj687mw8wg8qn940wccr2acjapd47zcdk58qr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("nyc" "dpdm"
                                                  "karma"
                                                  "lerna"
                                                  "memfs"
                                                  "mocha"
                                                  "sinon"
                                                  "codecov"
                                                  "unionfs"
                                                  "webpack"
                                                  "ts-mocha"
                                                  "cross-var"
                                                  "ts-loader"
                                                  "typescript"
                                                  "@types/node"
                                                  "karma-mocha"
                                                  "@types/mocha"
                                                  "@types/sinon"
                                                  "karma-webpack"
                                                  "@types/webpack"
                                                  "karma-coverage"
                                                  "@types/webpack-env"
                                                  "karma-spec-reporter"
                                                  "babel-plugin-istanbul"
                                                  "karma-chrome-launcher"
                                                  "karma-mocha-webworker"))))))))
    (home-page
     "https://github.com/open-telemetry/opentelemetry-js/tree/main/api")
    (synopsis "Public API for OpenTelemetry")
    (description "Public API for OpenTelemetry")
    (license license:asl2.0)))

(define-public node-earendil-works-pi-ai-0.87.1
  (package
    (name "node-earendil-works-pi-ai")
    (version "0.87.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.87.1.tgz")
       (sha256
        (base32 "180wff64wkxyv15h9hw312bm24lvlgv9mfzfdgw6a9nc4wpl7d1m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("canvas" "vitest"
                                                  "@types/node"
                                                  ;; Cloud-provider SDKs
                                                  ;; that pi-ai only
                                                  ;; ever reaches via
                                                  ;; dynamic import() at
                                                  ;; the point a given
                                                  ;; provider is
                                                  ;; actually used
                                                  ;; (dist/api/*.lazy.js).
                                                  ;; Omitted from this
                                                  ;; closure entirely;
                                                  ;; only invoking
                                                  ;; Bedrock or Gemini
                                                  ;; models will fail.
                                                  "@aws-sdk/client-bedrock-runtime"
                                                  "@smithy/node-http-handler"
                                                  "@google/genai"))))))))
    (inputs (list node-earendil-works-pi-telemetry-0.87.1
                  node-anthropic-ai-sdk-0.124.0
                  node-http-proxy-agent-9.1.0
                  node-https-proxy-agent-9.1.0
                  node-partial-json-0.1.7
                  node-typebox-1.3.27
                  node-openai-6.40.0))
    (home-page "https://github.com/earendil-works/pi#readme")
    (synopsis
     "Unified LLM API with automatic model discovery and provider configuration")
    (description
     "Unified LLM API with automatic model discovery and provider configuration")
    (license license:expat)))

(define-public node-earendil-works-pi-telemetry-0.87.1
  (package
    (name "node-earendil-works-pi-telemetry")
    (version "0.87.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-telemetry/-/pi-telemetry-0.87.1.tgz")
       (sha256
        (base32 "1x6sqnpzk2dpmc8lp1n9cs8yjdfvji2zy7y274djzw91bzg6pvvv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/node" "vitest"))))))))
    (home-page "https://github.com/earendil-works/pi#readme")
    (synopsis
     "Vendor-neutral telemetry contracts and typed schema utilities for pi")
    (description
     "Vendor-neutral telemetry contracts and typed schema utilities for pi")
    (license license:expat)))

(define-public node-earendil-works-pi-agent-core-0.87.1
  (package
    (name "node-earendil-works-pi-agent-core")
    (version "0.87.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.87.1.tgz")
       (sha256
        (base32 "1i0jih4927cc9wj0slmq2qg8ymkk7qvjhrwkpgy4xbl2lz7nfqk4"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("vitest" "typescript"
                                                  "@types/node"
                                                  "@vitest/coverage-v8"))))))))
    (inputs (list node-earendil-works-chord-0.87.1
                  node-earendil-works-pi-ai-0.87.1
                  node-earendil-works-pi-telemetry-0.87.1
                  node-diff-8.0.4
                  node-ignore-7.0.8
                  node-typebox-1.3.27
                  node-yaml-2.9.0))
    (home-page "https://github.com/earendil-works/pi#readme")
    (synopsis
     "General-purpose agent with transport abstraction, state management, and attachment support")
    (description
     "General-purpose agent with transport abstraction, state management, and attachment support")
    (license license:expat)))

(define-public pi-coding-agent
  (package
    (name "pi-coding-agent")
    (version "0.87.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-coding-agent/-/pi-coding-agent-0.87.1.tgz")
       (sha256
        (base32 "03sl4z2yrhwq39kw758986rnq1fk4kfciwybw5j69jg7c4yfw8ql"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("shx" "vitest"
                                                  "@types/ms"
                                                  "typescript"
                                                  "@types/diff"
                                                  "@types/node"
                                                  "@types/semver"
                                                  "@types/cross-spawn"
                                                  "@types/hosted-git-info"
                                                  "@types/proper-lockfile"
                                                  ;; 0.87.0 added these
                                                  ;; two devDependencies:
                                                  ;; pi-server is a
                                                  ;; development-only
                                                  ;; server for the pi
                                                  ;; editor integration
                                                  ;; (never imported by
                                                  ;; the CLI), and marked
                                                  ;; is a test/doc tool
                                                  ;; here -- pi-tui's
                                                  ;; runtime marked
                                                  ;; dependency is
                                                  ;; satisfied inside
                                                  ;; pi-tui's own store
                                                  ;; item.
                                                  "@earendil-works/pi-server"
                                                  "marked"
                                                  ;; Rust NAPI native
                                                  ;; addon (prebuilt
                                                  ;; per-OS/arch
                                                  ;; binaries) for OS
                                                  ;; clipboard access.
                                                  ;; Dropped: pi runs
                                                  ;; fine without it,
                                                  ;; just no
                                                  ;; copy/paste
                                                  ;; integration in
                                                  ;; the TUI.
                                                  "@mariozechner/clipboard")))))
          ;; Skip '--install-links' and bin-links: npm's bundled
          ;; tar/minizlib crashes with a silently-swallowed "ZlibError:
          ;; zlib: stream error" while copying inputs via
          ;; '--install-links' on this node-24.18.0 toolchain.
          ;; Symlinking instead avoids the crash and is equivalent for
          ;; a build-time dependency; '--no-bin-links' avoids a related
          ;; EROFS chmod failure for any input with a "bin" script.
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "node") "/bin/npm")
                      "--offline" "--ignore-scripts" "--no-bin-links"
                      "--no-audit" "install")
              ;; npm's symlinks (created because '--install-links' is
              ;; skipped above) are relative to wherever the build
              ;; happens to sit right now.  This package still gets
              ;; tar'd up and reinstalled at a different depth by the
              ;; 'repack/'install phases below, which would silently
              ;; leave the relative text pointing at the wrong place
              ;; (e.g. into /tmp instead of /gnu/store) once any later
              ;; consumer copies this tree elsewhere.  Absolutize now,
              ;; while the relative targets are still resolvable.
              (for-each (lambda (f)
                          (unless (string-prefix? "/" (readlink f))
                            (let ((target (canonicalize-path f)))
                              (delete-file f)
                              (symlink target f))))
                        (find-files "node_modules"
                                    (lambda (f s)
                                      (eq? 'symlink (stat:type s)))))))
          (replace 'install
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "node") "/bin/npm")
                      "--prefix" (assoc-ref outputs "out")
                      "--global" "--offline" "--loglevel" "info"
                      "--production" "--no-bin-links"
                      "install" "../package.tgz")
              ;; See the matching comment in 'configure above: this
              ;; second npm invocation (extracting into the final
              ;; store output) creates its own fresh relative symlinks,
              ;; independent of whatever 'configure already fixed.
              ;; Absolutize them too, or any later consumer that copies
              ;; this output elsewhere inherits broken links.
              (let ((out (assoc-ref outputs "out")))
                (for-each (lambda (f)
                            (unless (string-prefix? "/" (readlink f))
                              (let ((target (canonicalize-path f)))
                                (delete-file f)
                                (symlink target f))))
                          (find-files out
                                      (lambda (f s)
                                        (eq? 'symlink (stat:type s))))))))
          ;; '--no-bin-links' above (needed to dodge the EROFS crash
          ;; for *dependency* bin scripts symlinked from the read-only
          ;; store) also suppresses bin-linking for this package's own
          ;; "pi" executable -- but dist/cli.js here is our own fresh,
          ;; writable output, not a symlink into another store item, so
          ;; chmod/symlink are both safe to do by hand.
          (add-after 'install 'link-own-bin
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (cli (string-append
                           out
                           "/lib/node_modules/@earendil-works/pi-coding-agent/dist/cli.js")))
                (chmod cli #o755)
                (mkdir-p (string-append out "/bin"))
                (symlink cli (string-append out "/bin/pi"))))))))
    (inputs (list node-earendil-works-chord-0.87.1
                  node-earendil-works-pi-agent-core-0.87.1
                  node-earendil-works-pi-protocol-0.87.1
                  node-silvia-odwyer-photon-node-0.3.4
                  node-earendil-works-pi-client-0.87.1
                  node-earendil-works-pi-tui-0.87.1
                  node-earendil-works-pi-ai-0.87.1
                  node-proper-lockfile
                  node-hosted-git-info-9.0.3
                  node-highlight-js-10.7.3
                  node-grok-mermaid-0.2.3
                  node-cross-spawn-7.0.6
                  node-minimatch-10.2.6
                  node-typebox-1.3.27
                  node-undici-8.10.2
                  node-semver-7.8.5
                  node-ignore-7.0.8
                  node-chalk-6.0.0
                  node-yaml-2.9.0
                  node-jiti-2.7.0
                  node-diff-8.0.4))
    (home-page "https://pi.dev")
    (synopsis "Minimal, extensible AI coding-agent CLI")
    (description
     "@code{pi} is a minimal, extensible coding-agent harness with
read/bash/edit/write tools, session management, and pluggable model
providers (OpenAI, Anthropic, and any OpenAI-compatible endpoint).  It
runs as an interactive terminal UI or non-interactively for scripting.")
    (license license:expat)))

;; ------------------------------------------------------------------
;; pi-mcp-adapter (https://github.com/nicobailon/pi-mcp-adapter) -- the
;; de facto standard MCP client extension for pi (732K npm downloads/mo
;; vs. the next-closest alternative's 11K, and itself a dependency of
;; pi-mcp-router).  Exposes MCP servers to pi through one proxy tool
;; instead of dumping every server's full tool schema into context.
;;
;; Its own package.json ships no compiled dist/ for the extension entry
;; point ("exports"."." -> "./index.ts") -- pi's own extension loader
;; (dist/core/extensions/loader.js) transpiles extensions with a bundled
;; jiti at load time, which is why pi-coding-agent already carries
;; node-jiti-2.7.0 as one of its own inputs above.  Discovery walks
;; <agent-dir>/extensions/<name>/ for a package.json with a "pi.extensions"
;; field (dist/core/extensions/loader.js:resolveExtensionEntries) -- see
;; (peteches home modules pi)'s EXTENSIONS field, which symlinks this
;; package's node_modules/pi-mcp-adapter output there.
;;
;; Two of its declared `dependencies' are dropped from the closure below,
;; matching the @mariozechner/clipboard precedent in pi-coding-agent
;; itself: both are `require()'d lazily inside a function body (not a
;; static top-level import, which ESM would need resolvable at load time
;; regardless of whether the code path ever runs), and both already carry
;; their own try/catch fallback-error message for exactly this "package
;; missing" case in the adapter's own source (mcp-auth.ts, mcp-bearer-
;; store.ts):
;;   - @napi-rs/keyring: Rust NAPI native addon (prebuilt per-OS/arch
;;     binaries) for OS-keyring-backed OAuth token storage. Dropped: MCP
;;     connections work fine without it; only OAuth bearer tokens fail to
;;     persist in the OS keyring (no OAuth-requiring server is configured
;;     for any account here -- see EXTENSIONS/MCP-SERVERS below).
;;   - recheck: a ReDoS regex-safety checker shipping prebuilt per-
;;     platform native binaries (or a JAR) as optional deps. Dropped:
;;     used only for validating regex patterns in one advisory code path
;;     (proxy-modes.ts), not for MCP connectivity itself.
(define-public node-jose-6.2.10
  (package
    (name "node-jose")
    (version "6.2.10")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/jose/-/jose-6.2.10.tgz")
       (sha256
        (base32 "0dm12anhs1vw4h4k5crgirb0l72409nrbv6p9qcff8hias0il23a"))))
    (build-system node-build-system)
    (arguments (list #:tests? #f
                      #:phases #~(modify-phases %standard-phases
                                   (delete 'build))))
    (home-page "https://github.com/panva/jose#readme")
    (synopsis "JWA, JWS, JWE, JWT, JWK for Node.js and other runtimes")
    (description "JWA, JWS, JWE, JWT, JWK for Node.js and other runtimes")
    (license license:expat)))

(define-public node-eventsource-parser-4.1.0
  (package
    (name "node-eventsource-parser")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/eventsource-parser/-/eventsource-parser-4.1.0.tgz")
       (sha256
        (base32 "07r4nz15lsxs2g5vfmsb0m7spssl1fpcdavwvb1mffq6bj635klm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("knip" "oxfmt"
                                                  "mitata"
                                                  "oxlint"
                                                  "terser"
                                                  "vitest"
                                                  "esbuild"
                                                  "typescript"
                                                  "@types/node"
                                                  "@changesets/cli"
                                                  "eventsource-encoder"
                                                  "@changesets/changelog-github"))))))))
    (home-page "https://github.com/rexxars/eventsource-parser#readme")
    (synopsis "Streaming, source-agnostic EventSource/eventsource parser")
    (description "Streaming, source-agnostic EventSource/eventsource parser")
    (license license:expat)))

(define-public node-eventsource-5.1.1
  (package
    (name "node-eventsource")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/eventsource/-/eventsource-5.1.1.tgz")
       (sha256
        (base32 "1a1dcm56aran5s95f01fbhlaxqwhj7zr1z66qhwyx32kmpy28k5l"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's `prepare' script runs `npm run build', whose
          ;; `prebuild' step shells out to `scripts/clean.ts' -- a dev-only
          ;; file the published tarball doesn't ship (only dist/ does).
          ;; Harmless when this package builds standalone (npm skips
          ;; `prepare' for a registry-resolved dependency reference), but a
          ;; consumer using node-build-system's default 'install phase
          ;; (which passes `--install-links') re-triggers it here and
          ;; fails on the missing file. Same fix as node-minimatch-10.2.6
          ;; and friends above.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("oxfmt" "oxlint"
                                                  "vitest"
                                                  "happy-dom"
                                                  "playwright"
                                                  "typescript"
                                                  "@types/node"
                                                  "@changesets/cli"
                                                  "@vitest/browser"
                                                  "@tsconfig/strictest"
                                                  "eventsource-encoder"
                                                  "@vitest/browser-playwright"
                                                  "@changesets/changelog-github"
                                                  "@cloudflare/vitest-pool-workers"))))))))
    (inputs (list node-eventsource-parser-4.1.0))
    (home-page "https://github.com/EventSource/eventsource#readme")
    (synopsis "W3C-compliant EventSource client for Node.js")
    (description "W3C-compliant EventSource client for Node.js")
    (license license:expat)))

(define-public node-pkce-challenge-5.0.1
  (package
    (name "node-pkce-challenge")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/pkce-challenge/-/pkce-challenge-5.0.1.tgz")
       (sha256
        (base32 "0w7a7gzxrn5widngl5w358kfi68njp121ig77p8n4mf0bfpbpz6i"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("jest" "diverge"
                                                  "esbuild"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"))))))))
    (home-page "https://github.com/crouchcd/pkce-challenge#readme")
    (synopsis "RFC 7636 PKCE code verifier and challenge pair generator")
    (description "RFC 7636 PKCE code verifier and challenge pair generator")
    (license license:expat)))

(define-public node-standard-schema-spec-1.1.0
  (package
    (name "node-standard-schema-spec")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@standard-schema/spec/-/spec-1.1.0.tgz")
       (sha256
        (base32 "1byfgh3b6ngdj4vba2jw48bk9wl6gjqc7y2hshcba2i8prl75jx7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tsup" "typescript"))))))))
    (home-page "https://github.com/standard-schema/standard-schema#readme")
    (synopsis "Standard interface for TypeScript validation libraries")
    (description "Standard interface for TypeScript validation libraries")
    (license license:expat)))

(define-public node-modelcontextprotocol-core-2.0.0
  (package
    (name "node-modelcontextprotocol-core")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@modelcontextprotocol/core/-/core-2.0.0.tgz")
       (sha256
        (base32 "1rh5mk4var6mvwsnmqifxzyysr7ld3x51fxyh51x7jhs4y6knhz9"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("eslint" "tsdown"
                                                  "vitest"
                                                  "prettier"
                                                  "@eslint/js"
                                                  "typescript"
                                                  "eslint-plugin-n"
                                                  "typescript-eslint"
                                                  "eslint-config-prettier"
                                                  "@typescript/native-preview"
                                                  "@modelcontextprotocol/tsconfig"
                                                  "@modelcontextprotocol/eslint-config"
                                                  "@modelcontextprotocol/vitest-config"))))))))
    (inputs (list node-zod-4.4.3))
    (home-page "https://github.com/modelcontextprotocol/typescript-sdk#readme")
    (synopsis "Core primitives shared by the MCP TypeScript SDK packages")
    (description "Core primitives shared by the MCP TypeScript SDK packages")
    (license license:expat)))

(define-public node-modelcontextprotocol-client-2.0.0
  (package
    (name "node-modelcontextprotocol-client")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@modelcontextprotocol/client/-/client-2.0.0.tgz")
       (sha256
        (base32 "0cm7pwkcqkdba9d2lccgc1vlwd15xa0j5as544k6x85l9410niyb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ajv" "eslint"
                                                  "tsdown"
                                                  "vitest"
                                                  "prettier"
                                                  "@eslint/js"
                                                  "typescript"
                                                  "ajv-formats"
                                                  "eslint-plugin-n"
                                                  "typescript-eslint"
                                                  "@types/cross-spawn"
                                                  "@types/eventsource"
                                                  "@types/content-type"
                                                  "@cfworker/json-schema"
                                                  "eslint-config-prettier"
                                                  "@typescript/native-preview"
                                                  "@modelcontextprotocol/tsconfig"
                                                  "@modelcontextprotocol/test-helpers"
                                                  "@modelcontextprotocol/core-internal"
                                                  "@modelcontextprotocol/eslint-config"
                                                  "@modelcontextprotocol/vitest-config"))))))))
    (inputs (list node-modelcontextprotocol-core-2.0.0
                  node-zod-4.4.3
                  node-jose-6.2.10
                  node-cross-spawn-7.0.6
                  node-eventsource-5.1.1
                  node-pkce-challenge-5.0.1
                  node-eventsource-parser-4.1.0))
    (home-page "https://github.com/modelcontextprotocol/typescript-sdk#readme")
    (synopsis "MCP client transports (stdio, SSE, streamable HTTP)")
    (description "MCP client transports (stdio, SSE, streamable HTTP)")
    (license license:expat)))

(define-public node-modelcontextprotocol-ext-apps-1.7.5
  (package
    (name "node-modelcontextprotocol-ext-apps")
    (version "1.7.5")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@modelcontextprotocol/ext-apps/-/ext-apps-1.7.5.tgz")
       (sha256
        (base32 "1dhkd3mmvjxkg75j8gq8s55hn2f91w4iw92x43s520pb6rfibi0w"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's `prepare' script runs `npm run build && husky' --
          ;; a full schema-generation/bundling pipeline (tsx, bun, husky)
          ;; that isn't available in this offline sandbox and isn't
          ;; needed anyway: the published tarball already ships built
          ;; dist/. Harmless standalone; a consumer's own top-level
          ;; install (e.g. pi-mcp-adapter's) re-triggers it as a nested
          ;; dependency and fails on the missing `tsx' binary. Same fix
          ;; as node-eventsource-5.1.1 above.
          (add-before 'repack 'disable-lifecycle-scripts
            (lambda _
              (modify-json (delete-fields '("scripts.prepare"
                                            "scripts.postinstall")
                                          #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("bun" "tsx"
                                                  "zod"
                                                  "cors"
                                                  "husky"
                                                  "react"
                                                  "sharp"
                                                  "cheerio"
                                                  "esbuild"
                                                  "express"
                                                  "nodemon"
                                                  "typedoc"
                                                  "prettier"
                                                  "cross-env"
                                                  "react-dom"
                                                  "ts-to-zod"
                                                  "@types/bun"
                                                  "playwright"
                                                  "typescript"
                                                  "@types/node"
                                                  "@types/react"
                                                  "caniuse-lite"
                                                  "concurrently"
                                                  "playwright-core"
                                                  "@playwright/test"
                                                  "@types/react-dom"
                                                  "electron-to-chromium"
                                                  "typedoc-github-theme"
                                                  "@modelcontextprotocol/sdk"
                                                  "@boneskull/typedoc-plugin-mermaid"))))))))
    (inputs (list node-standard-schema-spec-1.1.0))
    (home-page "https://github.com/modelcontextprotocol/ext-apps#readme")
    (synopsis "MCP UI/App extension types and browser app-bridge runtime")
    (description "MCP UI/App extension types and browser app-bridge runtime")
    (license license:expat)))

(define-public node-ajv-formats-3.0.1
  (package
    (name "node-ajv-formats")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ajv-formats/-/ajv-formats-3.0.1.tgz")
       (sha256
        (base32 "1idca2hn65drqp1bc4v696bqvnv3x08nj1lrj791yf37sc7rimpl"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              ;; NOTE: "ajv" is deliberately NOT in this list. It appears
              ;; under devDependencies (for ajv-formats' own test suite)
              ;; but is ALSO a real `dependencies' entry -- deleting it
              ;; here would strip that too (delete-dependencies matches by
              ;; name across every dependency object, not just dev), which
              ;; then means the final `npm install --global' step never
              ;; even tries to resolve `ajv' from NODE_PATH's
              ;; node-ajv-8.18.0, leaving a broken `require("ajv")' at
              ;; runtime for anything importing ajv-formats.
              (modify-json (delete-dependencies '("jest"
                                                  "husky"
                                                  "eslint"
                                                  "ts-jest"
                                                  "prettier"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "lint-staged"
                                                  "json-schema-test"
                                                  "@ajv-validator/config"
                                                  "eslint-config-prettier"
                                                  "@typescript-eslint/parser"
                                                  "@typescript-eslint/eslint-plugin"))))))))
    (inputs (list node-ajv-8.18.0))
    (home-page "https://github.com/ajv-validator/ajv-formats#readme")
    (synopsis "Format validators for Ajv JSON Schema Validator")
    (description "Format validators for Ajv JSON Schema Validator")
    (license license:expat)))

(define-public node-define-lazy-prop-3.0.0
  (package
    (name "node-define-lazy-prop")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/define-lazy-prop/-/define-lazy-prop-3.0.0.tgz")
       (sha256
        (base32 "1da99k4vnnn9bxpgjniai8248bcf4w9cwyn7p3wlzii9l9kzxsdv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd"))))))))
    (home-page "https://github.com/sindresorhus/define-lazy-prop#readme")
    (synopsis "Define a lazily evaluated property on an object")
    (description "Define a lazily evaluated property on an object")
    (license license:expat)))

(define-public node-is-docker-3.0.0
  (package
    (name "node-is-docker")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/is-docker/-/is-docker-3.0.0.tgz")
       (sha256
        (base32 "1vnxw8y4p31nx66rbgxhd40jc5zx8akmcf7fpl3gy7n84l5hn8qs"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd" "sinon"))))))))
    (home-page "https://github.com/sindresorhus/is-docker#readme")
    (synopsis "Check if the process is running inside a Docker container")
    (description "Check if the process is running inside a Docker container")
    (license license:expat)))

(define-public node-is-inside-container-1.0.0
  (package
    (name "node-is-inside-container")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/is-inside-container/-/is-inside-container-1.0.0.tgz")
       (sha256
        (base32 "0yz0fbbkypqsx5d5cls1ik8928zkqhwl2kcp4iv7bi2g8fw9bpnv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd" "esmock"))))))))
    (inputs (list node-is-docker-3.0.0))
    (home-page "https://github.com/sindresorhus/is-inside-container#readme")
    (synopsis "Check if the process is running inside a container")
    (description "Check if the process is running inside a container")
    (license license:expat)))

(define-public node-is-wsl-3.1.0
  (package
    (name "node-is-wsl")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/is-wsl/-/is-wsl-3.1.0.tgz")
       (sha256
        (base32 "1ch2zg7f3dqv2c142lkbygj85j9xiq336kcm509l26k4pikswg6w"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd" "esmock"))))))))
    (inputs (list node-is-inside-container-1.0.0))
    (home-page "https://github.com/sindresorhus/is-wsl#readme")
    (synopsis "Check if the process is running inside Windows Subsystem for Linux")
    (description "Check if the process is running inside Windows Subsystem for Linux")
    (license license:expat)))

(define-public node-wsl-utils-0.1.0
  (package
    (name "node-wsl-utils")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/wsl-utils/-/wsl-utils-0.1.0.tgz")
       (sha256
        (base32 "1m4dv580c981pkwlb01vsg4j7gzwswc2n8vriljaa0nmp6gbdjyj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "typescript"))))))))
    (inputs (list node-is-wsl-3.1.0))
    (home-page "https://github.com/sindresorhus/wsl-utils#readme")
    (synopsis "Useful utilities for WSL (Windows Subsystem for Linux)")
    (description "Useful utilities for WSL (Windows Subsystem for Linux)")
    (license license:expat)))

(define-public node-run-applescript-7.0.0
  (package
    (name "node-run-applescript")
    (version "7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/run-applescript/-/run-applescript-7.0.0.tgz")
       (sha256
        (base32 "1sbmpymakf3npd0psjg382m3l30g184p2jqx14lkad7ciypbs5k7"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd"))))))))
    (home-page "https://github.com/sindresorhus/run-applescript#readme")
    (synopsis "Run AppleScript (macOS only; unused on Linux)")
    (description
     "Run AppleScript.  Only ever reached on macOS by @code{bundle-name}'s
platform branch; installed here purely because it is a static
`dependencies' entry, not because anything on Linux calls it.")
    (license license:expat)))

(define-public node-bundle-name-4.1.0
  (package
    (name "node-bundle-name")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/bundle-name/-/bundle-name-4.1.0.tgz")
       (sha256
        (base32 "0489x4n9f6z8v4qpycgakqp30j58xsgi69lrl96bw7698119vr1m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava"))))))))
    (inputs (list node-run-applescript-7.0.0))
    (home-page "https://github.com/sindresorhus/bundle-name#readme")
    (synopsis "Get the bundle name of an application")
    (description "Get the bundle name of an application")
    (license license:expat)))

(define-public node-default-browser-id-5.0.0
  (package
    (name "node-default-browser-id")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/default-browser-id/-/default-browser-id-5.0.0.tgz")
       (sha256
        (base32 "0kpdxc9s6z6bsda3j77njysr6iq8aamml4hm80gngl7gx6d55i2z"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava"))))))))
    (home-page "https://github.com/sindresorhus/default-browser-id#readme")
    (synopsis "Get the bundle identifier of the default browser (macOS only)")
    (description "Get the bundle identifier of the default browser (macOS only)")
    (license license:expat)))

(define-public node-default-browser-5.5.1
  (package
    (name "node-default-browser")
    (version "5.5.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/default-browser/-/default-browser-5.5.1.tgz")
       (sha256
        (base32 "09myy4lzx9mr19yr5vl3c5kazvmahpnz3mnxf2l4ddv5a0c9f692"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd"))))))))
    (inputs (list node-bundle-name-4.1.0 node-default-browser-id-5.0.0))
    (home-page "https://github.com/sindresorhus/default-browser#readme")
    (synopsis "Get the default browser")
    (description "Get the default browser")
    (license license:expat)))

(define-public node-open-10.2.0
  (package
    (name "node-open")
    (version "10.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/open/-/open-10.2.0.tgz")
       (sha256
        (base32 "1savhag7kknqskrsmbqhsrzvxlr5x341rvg0r2jq49l0f490vdmm"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd" "@types/node"))))))))
    (inputs (list node-wsl-utils-0.1.0
                  node-default-browser-5.5.1
                  node-define-lazy-prop-3.0.0
                  node-is-inside-container-1.0.0))
    (home-page "https://github.com/sindresorhus/open#readme")
    (synopsis "Open a file, URL, or executable with the user's preferred app")
    (description "Open a file, URL, or executable with the user's preferred app")
    (license license:expat)))

(define-public node-smol-toml-1.8.0
  (package
    (name "node-smol-toml")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/smol-toml/-/smol-toml-1.8.0.tgz")
       (sha256
        (base32 "14kqsc27p0zjmqf37mfjwks0xxmaxggjw3722gy7gdydj6z9bj8z"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          ;; Upstream's package.json declares a hard `devEngines.
          ;; packageManager' requirement (pnpm >=11.21.0, onFail: "error")
          ;; that makes npm itself refuse to run *any* command against
          ;; this package -- unrelated to scripts/deps, so
          ;; --ignore-scripts doesn't help. Strip the whole field; it's a
          ;; contributor-workflow guard, not something this package needs
          ;; at runtime.
          (add-after 'patch-dependencies 'disable-dev-engines-check
            (lambda _
              (modify-json (delete-fields '("devEngines") #:strict? #f))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("vite" "mitata"
                                                  "vitest"
                                                  "rolldown"
                                                  "typescript"
                                                  "@types/node"
                                                  "@mitata/counters"
                                                  "@tsconfig/node-ts"
                                                  "pin-github-action"
                                                  "@tsconfig/node-lts"
                                                  "@tsconfig/strictest"))))))))
    (home-page "https://github.com/squirrelchat/smol-toml#readme")
    (synopsis "Fast, small, and spec-compliant TOML parser/serializer")
    (description "Fast, small, and spec-compliant TOML parser/serializer")
    (license license:bsd-3)))

(define-public node-strip-json-comments-5.0.3
  (package
    (name "node-strip-json-comments")
    (version "5.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/strip-json-comments/-/strip-json-comments-5.0.3.tgz")
       (sha256
        (base32 "0fnfk264qwb1wxhjmxxq59kz811lplz8bj74y8k60nqa0spbhhjr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("xo" "ava" "tsd" "matcha"))))))))
    (home-page "https://github.com/sindresorhus/strip-json-comments#readme")
    (synopsis "Strip comments from JSON (ESM)")
    (description "Strip comments from JSON (ESM)")
    (license license:expat)))

;; pi-mcp-adapter 2.36.0 requires zod ^4.5.4 (both directly and via
;; @modelcontextprotocol/ext-tasks); the previous node-zod-4.4.3 in its
;; inputs was too old for that range.
(define-public node-zod-4.6.5
  (package
    (name "node-zod")
    (version "4.6.5")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/zod/-/zod-4.6.5.tgz")
       (sha256
        (base32 "1z1n1fsh2lrmmqpf7lm81p5r1qq6mi1sqnf2mz2c23g37m9hr357"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://github.com/colinhacks/zod#readme")
    (synopsis "TypeScript schema validation library")
    (description "TypeScript schema validation library")
    (license license:expat)))

(define-public node-undici-6.28.1
  (package
    (name "node-undici")
    (version "6.28.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/undici/-/undici-6.28.1.tgz")
       (sha256
        (base32 "0x4vnsjm4jn1i9p6mdmpvmxd08hs0jvi16zyqzd47zy0r6m930g1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'remove-dev-dependencies
            (lambda _
              (modify-json (delete-dev-dependencies)
                           (delete-fields '("scripts"))))))))
    (home-page "https://github.com/nodejs/undici#readme")
    (synopsis "HTTP/1.1 client for Node.js")
    (description "Standalone HTTP/1.1 client, a low-level fetch implementation.")
    (license license:isc)))

(define-public node-typesafe-ai-sdk-0.6.0
  (package
    (name "node-typesafe-ai-sdk")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@typesafe-ai/sdk/-/sdk-0.6.0.tgz")
       (sha256
        (base32 "13ipcmirz9xmixp4yy1vx82f74qk025865ab53aqxi8ir4blbnfc"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies
                            '("@arethetypeswrong/cli" "@biomejs/biome"
                              "@types/node" "@vitest/coverage-v8"
                              "git-cliff" "jsr" "publint" "tsdown"
                              "tsx" "typescript" "vitest"))))))))
    (home-page "https://www.npmjs.com/package/@typesafe-ai/sdk")
    (synopsis "Type-safe AI SDK client")
    (description "Type-safe client for AI provider APIs, used by pi-mcp-adapter's JEV client.")
    (license license:expat)))

;; New runtime dependency of pi-mcp-adapter 2.36.0 (@modelcontextprotocol/
;; ext-tasks ^0.1.0).  Its only runtime dependency is zod ^4.5.4 (provided by
;; node-zod-4.6.5); the @modelcontextprotocol/client peer dependency is already
;; in pi-mcp-adapter's inputs.  No install-lifecycle scripts, so it packs
;; cleanly from the registry tarball.
(define-public node-modelcontextprotocol-ext-tasks-0.1.0
  (package
    (name "node-modelcontextprotocol-ext-tasks")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@modelcontextprotocol/ext-tasks/-/ext-tasks-0.1.0.tgz")
       (sha256
        (base32 "0mh3yz9aja861r1wwqk4s0hkrsw0aw65y9lqmfr1wmyrdlk8k3bj"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies
                            '("@eslint/js" "eslint" "eslint-plugin-jsdoc"
                              "fast-check" "globals" "prettier"
                              "typescript-eslint" "@modelcontextprotocol/client"))))))))
    (inputs (list node-zod-4.6.5))
    (home-page "https://github.com/modelcontextprotocol/ext-tasks#readme")
    (synopsis "MCP tasks extension types and runtime")
    (description "MCP tasks extension types and runtime")
    (license license:asl2.0)))

;; Override of node-modelcontextprotocol-ext-tasks-0.1.0 that
;; bundles @modelcontextprotocol/client into the item's own
;; node_modules, making the ext-tasks /client subpath self-contained.
;; Why: pi-mcp-adapter's
;; node_modules/@modelcontextprotocol/ext-tasks is a symlink to the
;; plain item's store path; pi's jiti extension loader resolves it to
;; that real path before loading, so the /client subpath's transitive
;; `import { Client } from "@modelcontextprotocol/client"' can only
;; resolve from within the item itself.  Upstream declares client an
;; optional peerDependency, so the plain item (zod-only inputs) cannot
;; satisfy it, and the sibling client symlink in pi-mcp-adapter's own
;; node_modules is unreachable from the real path.
;; NODE_OPTIONS=--preserve-symlinks does not help: jiti's bundled mlly
;; resolver realpaths every module entry unconditionally (verified
;; against the jiti bundled with pi-coding-agent 0.87.1).
;; Adding client to `dependencies' before `patch-dependencies' makes
;; the build rewrite it to a file: reference to the client input,
;; which the standard node-build-system phases then install (as a
;; real copy, via --install-links) into the item's node_modules.
;; The client package is self-contained (its own deps are real copies
;; in its node_modules), so its transitive imports resolve too.
(define-public node-modelcontextprotocol-ext-tasks-0.1.0-with-client
  (package/inherit node-modelcontextprotocol-ext-tasks-0.1.0
    (inputs (list node-zod-4.6.5
                  node-modelcontextprotocol-client-2.0.0))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-before 'patch-dependencies 'add-client-dependency
            (lambda _
              (modify-json
               (lambda (pkg-meta)
                 (assoc-set! pkg-meta "dependencies"
                             (cons '("@modelcontextprotocol/client" . "0.0.0")
                                   (assoc-ref pkg-meta "dependencies")))))))
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies
                            '("@eslint/js" "eslint" "eslint-plugin-jsdoc"
                              "fast-check" "globals" "prettier"
                              "typescript-eslint"))))))))))

(define-public pi-mcp-adapter
  (package
    (name "pi-mcp-adapter")
    (version "2.36.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/pi-mcp-adapter/-/pi-mcp-adapter-2.36.0.tgz")
       (sha256
        (base32 "1w19nvb68a0p70ilnfjrinnhmz9m9hijw2jdcmxrnkivkkxsignb"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies
                            '("tsx" "vitest" "typebox"
                              "@types/bun" "typescript" "@types/node"
                              "@types/open" "@types/cross-spawn"
                              "@earendil-works/pi-ai"
                              "@earendil-works/pi-tui"
                              "@earendil-works/pi-coding-agent"
                              "@modelcontextprotocol/conformance"
                              ;; See the module comment above this
                              ;; package for why these two (regular
                              ;; `dependencies', not dev) are dropped.
                              "@napi-rs/keyring" "recheck")))))
          ;; Same npm/store-symlink dance as pi-coding-agent above --
          ;; see its 'configure/'install phases for the full rationale.
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "node") "/bin/npm")
                      "--offline" "--ignore-scripts" "--no-bin-links"
                      "--no-audit" "install")
              (for-each (lambda (f)
                          (unless (string-prefix? "/" (readlink f))
                            (let ((target (canonicalize-path f)))
                              (delete-file f)
                              (symlink target f))))
                        (find-files "node_modules"
                                    (lambda (f s)
                                      (eq? 'symlink (stat:type s)))))))
          (replace 'install
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (invoke (string-append (assoc-ref inputs "node") "/bin/npm")
                      "--prefix" (assoc-ref outputs "out")
                      "--global" "--offline" "--loglevel" "info"
                      "--production" "--no-bin-links"
                      "install" "../package.tgz")
              (let ((out (assoc-ref outputs "out")))
                (for-each (lambda (f)
                            (unless (string-prefix? "/" (readlink f))
                              (let ((target (canonicalize-path f)))
                                (delete-file f)
                                (symlink target f))))
                          (find-files out
                                      (lambda (f s)
                                        (eq? 'symlink (stat:type s)))))))))))
    (inputs (list node-ajv-8.18.0
                  node-ajv-formats-3.0.1
                  node-cross-spawn-7.0.6
                  node-modelcontextprotocol-client-2.0.0
                  node-modelcontextprotocol-core-2.0.0
                  node-modelcontextprotocol-ext-apps-1.7.5
                  node-open-10.2.0
                  node-smol-toml-1.8.0
                  node-strip-json-comments-5.0.3
                  node-modelcontextprotocol-ext-tasks-0.1.0-with-client
                  node-zod-4.6.5
                  node-undici-6.28.1
                  node-typesafe-ai-sdk-0.6.0))
    (home-page "https://github.com/nicobailon/pi-mcp-adapter#readme")
    (synopsis "MCP client extension for the pi coding-agent CLI")
    (description
     "Adds Model Context Protocol client support to @code{pi} as a single
low-token proxy tool (@code{mcp({search, tool, args})}) instead of
dumping every connected MCP server's full tool schema into context.
Servers are lazy by default: they only connect once a tool from that
server is actually called.  Reads the same @file{.mcp.json}/
@file{~/.config/mcp/mcp.json} files most other MCP-capable hosts use,
plus @file{<pi agent dir>/mcp.json} for pi-specific overrides.")
    (license license:expat)))
