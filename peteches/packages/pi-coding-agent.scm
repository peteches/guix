(define-module (peteches packages pi-coding-agent)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module (guix-science packages rstudio-node)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages node-xyz))

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

(define-public node-semver-7.8.0
  (package
    (name "node-semver")
    (version "7.8.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/semver/-/semver-7.8.0.tgz")
       (sha256
        (base32 "017wsvynr31d9zgw0p0jnng5kvf25465jnap9r136g4cm0r0rw7l"))))
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

(define-public node-undici-8.9.0
  (package
    (name "node-undici")
    (version "8.9.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/undici/-/undici-8.9.0.tgz")
       (sha256
        (base32 "176n8ls7jgfvc58ivfq8hhyicaf24mm0c22j6ay08bimx6rsnm7m"))))
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

(define-public node-minimatch-10.2.5
  (package
    (name "node-minimatch")
    (version "10.2.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/minimatch/-/minimatch-10.2.5.tgz")
       (sha256
        (base32 "1rd99j1d6x4lfb5ajnda6d16m7agx81x16qgwvxc7imz2mls9km6"))))
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

(define-public node-grok-mermaid-0.2.2
  (package
    (name "node-grok-mermaid")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/grok-mermaid/-/grok-mermaid-0.2.2.tgz")
       (sha256
        (base32 "0d19jjp06g4xcf8b24nhfnr25p18lxb9ksyaz2sg1r9vm4v85yb1"))))
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

(define-public node-earendil-works-pi-tui-0.84.2
  (package
    (name "node-earendil-works-pi-tui")
    (version "0.84.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-tui/-/pi-tui-0.84.2.tgz")
       (sha256
        (base32 "0glrvz01zk0r9kgn00vsnnmnvivz4y24k2qv6kyp959ahmnw5gis"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chalk" "@xterm/headless"))))))))
    (inputs (list node-get-east-asian-width-1.6.0 node-marked-18.0.5))
    (home-page "https://github.com/earendil-works/pi#readme")
    (synopsis
     "Terminal User Interface library with differential rendering for efficient text-based applications")
    (description
     "Terminal User Interface library with differential rendering for efficient text-based applications")
    (license license:expat)))

(define-public node-earendil-works-pi-client-0.84.2
  (package
    (name "node-earendil-works-pi-client")
    (version "0.84.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-client/-/pi-client-0.84.2.tgz")
       (sha256
        (base32 "0v7nspgqjn1zq7gy53s2dsr5mapd60p5r03h6rs8bssnanl60yyi"))))
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
    (inputs (list node-earendil-works-pi-protocol-0.84.2))
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

(define-public node-earendil-works-pi-protocol-0.84.2
  (package
    (name "node-earendil-works-pi-protocol")
    (version "0.84.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-protocol/-/pi-protocol-0.84.2.tgz")
       (sha256
        (base32 "1pidcriarncpvkgls5q58zfrvzbjjv6k2rav4gr8kdnpigzczz6y"))))
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
    (inputs (list node-typebox-1.3.7))
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

(define-public node-ignore-7.0.5
  (package
    (name "node-ignore")
    (version "7.0.5")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/ignore/-/ignore-7.0.5.tgz")
       (sha256
        (base32 "1062hjm3bgg9013nvl33mmz3cchvcjhndq6gj61sgzazvqwnqbg8"))))
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

(define-public node-typebox-1.3.7
  (package
    (name "node-typebox")
    (version "1.3.7")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/typebox/-/typebox-1.3.7.tgz")
       (sha256
        (base32 "0fl6l3ylrdgbgvdss4c5s3l5i0kcbadr8a73kk53cjg6c0jr9l5i"))))
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

(define-public node-earendil-works-pi-ai-0.84.2
  (package
    (name "node-earendil-works-pi-ai")
    (version "0.84.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-ai/-/pi-ai-0.84.2.tgz")
       (sha256
        (base32 "0hfgv921j2jg3hhv26zgsa4yygp25smsgn3cb7n2xsxhfrd7hqh2"))))
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
    (inputs (list node-earendil-works-pi-telemetry-0.84.2
                  node-opentelemetry-api-1.9.0
                  node-https-proxy-agent
                  node-anthropic-ai-sdk-0.91.1
                  node-http-proxy-agent-7.0.2
                  node-partial-json-0.1.7
                  node-typebox-1.3.7
                  node-openai-6.40.0))
    (home-page "https://github.com/earendil-works/pi#readme")
    (synopsis
     "Unified LLM API with automatic model discovery and provider configuration")
    (description
     "Unified LLM API with automatic model discovery and provider configuration")
    (license license:expat)))

(define-public node-earendil-works-pi-telemetry-0.84.2
  (package
    (name "node-earendil-works-pi-telemetry")
    (version "0.84.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-telemetry/-/pi-telemetry-0.84.2.tgz")
       (sha256
        (base32 "0w25g2j6mzzxjpbnxjwmn359lbkj029gb1cgv3082z5g91zaq2zx"))))
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

(define-public node-earendil-works-pi-agent-core-0.84.2
  (package
    (name "node-earendil-works-pi-agent-core")
    (version "0.84.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-agent-core/-/pi-agent-core-0.84.2.tgz")
       (sha256
        (base32 "1kxxsgx4pfr2nhgy9nx78gfdwwjxl698dlhmv5lzy2bcdwn5snsn"))))
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
    (inputs (list node-earendil-works-pi-telemetry-0.84.2
                  node-earendil-works-pi-ai-0.84.2
                  node-typebox-1.3.7
                  node-ignore-7.0.5
                  node-yaml-2.9.0
                  node-diff-8.0.4))
    (home-page "https://github.com/earendil-works/pi#readme")
    (synopsis
     "General-purpose agent with transport abstraction, state management, and attachment support")
    (description
     "General-purpose agent with transport abstraction, state management, and attachment support")
    (license license:expat)))

(define-public pi-coding-agent
  (package
    (name "pi-coding-agent")
    (version "0.84.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@earendil-works/pi-coding-agent/-/pi-coding-agent-0.84.2.tgz")
       (sha256
        (base32 "1ylglvqwga8scrb4f7vx79ay6dbl8amk7gy7fh0iy30sgg6rkf4m"))))
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
    (inputs (list node-earendil-works-pi-agent-core-0.84.2
                  node-earendil-works-pi-protocol-0.84.2
                  node-silvia-odwyer-photon-node-0.3.4
                  node-earendil-works-pi-client-0.84.2
                  node-earendil-works-pi-tui-0.84.2
                  node-earendil-works-pi-ai-0.84.2
                  node-proper-lockfile
                  node-hosted-git-info-9.0.3
                  node-highlight-js-10.7.3
                  node-grok-mermaid-0.2.2
                  node-cross-spawn-7.0.6
                  node-minimatch-10.2.5
                  node-typebox-1.3.7
                  node-undici-8.9.0
                  node-semver-7.8.0
                  node-ignore-7.0.5
                  node-chalk-5.6.2
                  node-yaml-2.9.0
                  node-jiti-2.7.0
                  node-glob-13.0.6
                  node-diff-8.0.4))
    (home-page "https://pi.dev")
    (synopsis "Minimal, extensible AI coding-agent CLI")
    (description
     "@code{pi} is a minimal, extensible coding-agent harness with
read/bash/edit/write tools, session management, and pluggable model
providers (OpenAI, Anthropic, and any OpenAI-compatible endpoint).  It
runs as an interactive terminal UI or non-interactively for scripting.")
    (license license:expat)))
