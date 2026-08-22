;; peteches/packages/dev-mcp-servers.scm — MCP servers for the ygo account's
;; dev-database toolchain.
;;
;; Two npm packages, generated via `guix import npm-binary -r <pkg>@<version>'
;; and hand-curated, following the same pattern as comfyui-mcp.scm:
;;
;; - @modelcontextprotocol/server-postgres — registered twice in
;;   %ygo-mcp-servers (claude-workstation-ygo.scm), once per dev database,
;;   since the connection string is a positional CLI arg, not something the
;;   package itself can multiplex.
;; - time-mcp — registered once, with --local-timezone.
;;
;; Both pin @modelcontextprotocol/sdk very differently: server-postgres pins
;; an exact old "1.0.1" (not a caret range) — upstream hasn't touched this
;; package since the SDK's early days, so its dependency subtree is kept
;; fully self-contained here rather than risking a newer SDK against
;; whatever narrow API surface a year-old release actually calls. time-mcp
;; pins "^1.25.3", comfortably satisfied by the already-packaged
;; node-modelcontextprotocol-sdk-1.29.0 (claude-agent-acp-deps.scm) — reusing it
;; skips ~35 packages that only exist to support that SDK version's
;; Express/Hono-based Streamable HTTP transport, which neither server here
;; uses (both are stdio).
;;
;; A handful of leaf/mid-tier packages below are exact version+hash matches
;; for ones already defined in seerr-deps.scm, so they're imported from
;; there instead of redefined.

(define-module (peteches packages dev-mcp-servers)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system node)
  #:use-module ((gnu packages databases) #:select (postgresql))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages base) #:select (which))
  #:use-module ((peteches packages seerr-deps)
                #:select (node-bindings-1.5.0
                          node-nan-2.26.2
                          node-raw-body-3.0.2
                          node-content-type-1.0.5
                          node-pg-pool-3.14.0
                          node-pg-types-2.2.0
                          node-pg-cloudflare-1.4.0
                          node-xtend-4.0.2
                          node-dayjs-1.11.21))
  #:use-module ((peteches packages claude-agent-acp-deps)
                #:select (node-modelcontextprotocol-sdk-1.29.0
                          node-zod-4.4.3)))

;; ------------------------------
;; time-mcp
;; ------------------------------

(define-public node-time-mcp
  (package
    (name "node-time-mcp")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/time-mcp/-/time-mcp-1.0.6.tgz")
       (sha256
        (base32 "1xnv3i8il1416b0691h4vydd7l61i0qdimk0ja9nwvqbz61g1pjr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@eslint/js" "@types/node"
                                                  "@typescript-eslint/eslint-plugin"
                                                  "@typescript-eslint/parser"
                                                  "eslint"
                                                  "tsup"
                                                  "tsx"
                                                  "typescript"
                                                  "typescript-eslint"))))))))
    (inputs (list node-zod-4.4.3 node-dayjs-1.11.21
                  node-modelcontextprotocol-sdk-1.29.0))
    (home-page "https://github.com/yokingma/time-mcp#readme")
    (synopsis "MCP server giving LLMs time awareness")
    (description
     "@code{time-mcp} is a Model Context Protocol server exposing current
time, timezone conversion and relative-time tools to MCP clients.  Set
@option{--local-timezone} to pin the timezone it reports as local.")
    (license license:expat)))

;; ------------------------------
;; @modelcontextprotocol/server-postgres
;; ------------------------------
;;
;; Self-contained pg 8.23.0 subtree (only its still-missing pieces —
;; pg-pool/pg-types/pg-cloudflare/bindings/nan are exact matches already in
;; seerr-deps.scm and reused above), plus the exact old SDK 1.0.1 this
;; package pins.

;; Native addon: binding.gyp locates libpq via `pg_config' (found by
;; shelling out to `which pg_config', with a /usr/bin-only fallback `find'
;; that can never see anything in the store) and links `-lpq' against
;; whatever `pg_config --libdir' reports. Neither is on PATH by default in
;; a Guix build environment, and node-gyp separately needs Python — same
;; two gaps comfyui-mcp.scm's better-sqlite3/sharp phases work around, here
;; for a real external library (Guix's `postgresql') rather than a vendored
;; one. libpq's wire protocol is stable across server versions, so the
;; default `postgresql' package is fine to build against even though the
;; ygo VM's dev database runs postgresql-17.
(define-public node-libpq-1.11.0
  (package
    (name "node-libpq")
    (version "1.11.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/libpq/-/libpq-1.11.0.tgz")
       (sha256
        (base32 "10923axxrz99if6mm3zi3g3xsdvb2xa3qbq2dvqcwx214nm424gn"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("async" "buffer-from"
                                                  "lodash" "mocha" "okay")))))
          (add-before 'install 'set-node-gyp-toolchain
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((python (search-input-file inputs "/bin/python3"))
                    (pg-config-dir (dirname (search-input-file inputs
                                             "/bin/pg_config"))))
                (setenv "PATH"
                        (string-append pg-config-dir ":" (getenv "PATH")))
                (setenv "PYTHON" python)
                (setenv "npm_config_python" python))
              (setenv "CC" "gcc")
              (setenv "CXX" "g++")))
          ;; libpq's package.json has no "install"/"preinstall" script, so
          ;; npm's own *implicit* default (run `node-gyp rebuild' whenever
          ;; binding.gyp is present and neither is declared) never gets
          ;; written down explicitly — which means node-build-system's
          ;; standard `avoid-node-gyp-rebuild' phase, which only neutralises
          ;; an install script it can literally see in package.json, has
          ;; nothing to rewrite. Every *dependent* package's own npm
          ;; install of this one as a nested dependency (pg-native below)
          ;; then re-triggers that implicit rebuild from scratch, in a
          ;; sandbox with no Python/pg_config on PATH. Same fix as
          ;; better-sqlite3/sharp in comfyui-mcp.scm: write the no-op
          ;; install script ourselves.
          (add-after 'install 'no-rebuild-for-dependents
            (lambda* (#:key outputs #:allow-other-keys)
              (modify-json #:file (search-input-file outputs
                                   "/lib/node_modules/libpq/package.json")
                           (lambda (meta)
                             (assoc-set! meta "scripts"
                                         (assoc-set! (or (assoc-ref meta
                                                                    "scripts")
                                                         '()) "install"
                                          "echo Guix: addon already built")))))))))
    (native-inputs (list python which))
    (inputs (list postgresql node-nan-2.26.2 node-bindings-1.5.0))
    (home-page "https://github.com/brianc/node-libpq#readme")
    (synopsis "Low-level native bindings to PostgreSQL libpq")
    (description "Low-level native bindings to PostgreSQL libpq")
    (license license:expat)))

(define-public node-pg-native-3.9.0
  (package
    (name "node-pg-native")
    (version "3.9.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pg-native/-/pg-native-3.9.0.tgz")
       (sha256
        (base32 "0qmb8k2623afnqmphjn0wya2ipy99i7mdgg4d7509bsiy6ymz91m"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("async" "concat-stream"
                                                  "generic-pool"
                                                  "lodash"
                                                  "mocha"
                                                  "node-gyp"
                                                  "okay"
                                                  "semver"))))))))
    (inputs (list node-pg-types-2.2.0 node-libpq-1.11.0))
    (home-page
     "https://github.com/brianc/node-postgres/tree/master/packages/pg-native")
    (synopsis "A slightly nicer interface to Postgres over node-libpq")
    (description "A slightly nicer interface to Postgres over node-libpq")
    (license license:expat)))

(define-public node-split2-4.2.0
  (package
    (name "node-split2")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/split2/-/split2-4.2.0.tgz")
       (sha256
        (base32 "0m5z4zgfpwh4b65a457m3ymr4zbm35kbdbx8rmnzzi9g0iziw1p0"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("binary-split"
                                                  "callback-stream"
                                                  "fastbench"
                                                  "nyc"
                                                  "pre-commit"
                                                  "standard"
                                                  "tape"))))))))
    (home-page "https://github.com/mcollina/split2#readme")
    (synopsis "Split a text stream into a line stream")
    (description "Split a text stream into a line stream, using Stream 3")
    (license license:isc)))

(define-public node-pgpass-1.0.6
  (package
    (name "node-pgpass")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pgpass/-/pgpass-1.0.6.tgz")
       (sha256
        (base32 "1wlzwsg25qv2f3m8ivqnnr3qz5774vlzj2l766sk418sjfsjs67k"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("pg" "nyc"
                                                  "tmp"
                                                  "mocha"
                                                  "which"
                                                  "jshint"
                                                  "resumer"
                                                  "pg-escape"
                                                  "pg-native"))))))))
    (inputs (list node-split2-4.2.0))
    (home-page "https://github.com/hoegaarden/pgpass#readme")
    (synopsis "Module for reading .pgpass")
    (description "Module for reading .pgpass")
    (license license:expat)))

(define-public node-pg-int8-1.0.1
  (package
    (name "node-pg-int8")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pg-int8/-/pg-int8-1.0.1.tgz")
       (sha256
        (base32 "14jy6q61f4cqx582lkbi2135g1b31736zfzd87l18s59fj4x847h"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@charmander/eslint-config-base"
                                                  "tap"))))))))
    (home-page "https://github.com/charmander/pg-int8#readme")
    (synopsis "64-bit big-endian signed integer-to-string conversion")
    (description "64-bit big-endian signed integer-to-string conversion")
    (license license:isc)))

(define-public node-postgres-date-2.1.0
  (package
    (name "node-postgres-date")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/postgres-date/-/postgres-date-2.1.0.tgz")
       (sha256
        (base32 "1816cqmkm3p9s52wgxkxcvjysb1sq981pkiqw96693aqcmkl6j65"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("standard" "tape"))))))))
    (home-page "https://github.com/bendrucker/postgres-date#readme")
    (synopsis "Postgres date column parser")
    (description "Postgres date column parser")
    (license license:expat)))

(define-public node-postgres-array-3.0.4
  (package
    (name "node-postgres-array")
    (version "3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/postgres-array/-/postgres-array-3.0.4.tgz")
       (sha256
        (base32 "1xpp9zfshp624vqr5nsvlfj0xkbnr8l2pvv1y1y4xj68hwsxh3wv"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tape" "standard"))))))))
    (home-page "https://github.com/bendrucker/postgres-array#readme")
    (synopsis "Parse postgres array columns")
    (description "Parse postgres array columns")
    (license license:expat)))

(define-public node-postgres-bytea-3.0.0
  (package
    (name "node-postgres-bytea")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/postgres-bytea/-/postgres-bytea-3.0.0.tgz")
       (sha256
        (base32 "08m4x2j4azp0l7bxk56plghfdqxzqnc1zkkmvivcfnna3niw4b2d"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tape" "standard"))))))))
    (home-page "https://github.com/bendrucker/postgres-bytea#readme")
    (synopsis "Postgres bytea parser")
    (description "Postgres bytea parser")
    (license license:expat)))

(define-public node-postgres-interval-4.1.0
  (package
    (name "node-postgres-interval")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/postgres-interval/-/postgres-interval-4.1.0.tgz")
       (sha256
        (base32 "1ivzxn9b9idbynqm9h5jgjx090v8n7ps08wvz5v9ggyfgg7qq3a3"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("tape" "standard"))))))))
    (inputs (list node-xtend-4.0.2))
    (home-page "https://github.com/bendrucker/postgres-interval#readme")
    (synopsis "Parse Postgres interval columns")
    (description "Parse Postgres interval columns")
    (license license:expat)))

;; node-pg-types-2.2.0 itself is reused from seerr-deps.scm (see #:select
;; above) — but that copy is wired to seerr's own postgres-{date,array,
;; bytea,interval}/pg-int8 sub-versions, which happen to be identical to
;; the ones defined here, so pg-native above (needing pg-types) is fine
;; either way. The five node-postgres-* and node-pg-int8 packages above
;; exist here only because node-pg-protocol/-connection-string below (not
;; reused — see file header) sit one level up the tree from them via
;; pg-types; keeping them local avoids reaching back into seerr-deps.scm's
;; internals for packages it doesn't export.

(define-public node-pg-protocol-1.16.0
  (package
    (name "node-pg-protocol")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pg-protocol/-/pg-protocol-1.16.0.tgz")
       (sha256
        (base32 "1z6xnz5anky990ih08w8pab411gyqg04rm5dzbwxy188z17igz9p"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("chai" "mocha"
                                                  "ts-node"
                                                  "typescript"
                                                  "@types/chai"
                                                  "@types/node"
                                                  "@types/mocha"))))))))
    (home-page "https://github.com/brianc/node-postgres#readme")
    (synopsis "Postgres client/server binary protocol, in TypeScript")
    (description
     "The postgres client/server binary protocol, implemented in TypeScript.")
    (license license:expat)))

(define-public node-pg-connection-string-2.14.0
  (package
    (name "node-pg-connection-string")
    (version "2.14.0")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/pg-connection-string/-/pg-connection-string-2.14.0.tgz")
       (sha256
        (base32 "0861vz0pvf884rp2kq9pwi6xhhdvvlsd7a2i3gsiflhc3gh8nkii"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("@types/pg" "chai"
                                                  "coveralls"
                                                  "istanbul"
                                                  "mocha"
                                                  "nyc"
                                                  "tsx"
                                                  "typescript"))))))))
    (home-page
     "https://github.com/brianc/node-postgres/tree/master/packages/pg-connection-string")
    (synopsis "Functions for dealing with a PostgreSQL connection string")
    (description "Functions for dealing with a PostgreSQL connection string")
    (license license:expat)))

(define-public node-pg-8.23.0
  (package
    (name "node-pg")
    (version "8.23.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/pg/-/pg-8.23.0.tgz")
       (sha256
        (base32 "1b7l22px26ih5xxbl3inm4507ksln0hna7d2bkymkgjdd417knyr"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("co" "async"
                                                  "vitest"
                                                  "bluebird"
                                                  "wrangler"
                                                  "typescript"
                                                  "pg-copy-streams"
                                                  "@cloudflare/workers-types"
                                                  "@cloudflare/vitest-pool-workers"
                                                  "pg-native"))))))))
    (inputs (list node-pg-connection-string-2.14.0
                  node-pg-cloudflare-1.4.0
                  node-pg-protocol-1.16.0
                  node-pg-types-2.2.0
                  node-pg-pool-3.14.0
                  node-pgpass-1.0.6
                  node-pg-native-3.9.0))
    (home-page "https://github.com/brianc/node-postgres")
    (synopsis "PostgreSQL client - pure JavaScript & libpq with the same API")
    (description
     "PostgreSQL client - pure javascript & libpq with the same API.")
    (license license:expat)))

;; sdk-1.0.1 pins zod's 3.x line, distinct from the 4.4.3 the newer SDK
;; (reused above for time-mcp) needs.
(define-public node-zod-3.25.76
  (package
    (name "node-zod")
    (version "3.25.76")
    (source
     (origin
       (method url-fetch)
       (uri "https://registry.npmjs.org/zod/-/zod-3.25.76.tgz")
       (sha256
        (base32 "0xw3m1qdqbqam3fhxiv8ag9l9kampywwx4gfcjmis36xy02il7wy"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build))))
    (home-page "https://zod.dev")
    (synopsis "TypeScript-first schema declaration and validation")
    (description
     "TypeScript-first schema declaration and validation library with
static type inference.")
    (license license:expat)))

(define-public node-modelcontextprotocol-sdk-1.0.1
  (package
    (name "node-modelcontextprotocol-sdk")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@modelcontextprotocol/sdk/-/sdk-1.0.1.tgz")
       (sha256
        (base32 "1sn6i482ldi7fzf0rflrih2jvx260p7aiaqql088fk0rqw6dbpa1"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("ws" "tsx"
                                                  "jest"
                                                  "eslint"
                                                  "express"
                                                  "ts-jest"
                                                  "@types/ws"
                                                  "@eslint/js"
                                                  "typescript"
                                                  "@types/jest"
                                                  "@types/node"
                                                  "eventsource"
                                                  "@types/express"
                                                  "@types/eslint__js"
                                                  "typescript-eslint"
                                                  "@types/eventsource"
                                                  "@types/content-type"))))))))
    (inputs (list node-content-type-1.0.5 node-raw-body-3.0.2 node-zod-3.25.76))
    (home-page "https://modelcontextprotocol.io")
    (synopsis "Model Context Protocol implementation for TypeScript (v1.0.1)")
    (description "Model Context Protocol implementation for TypeScript.")
    (license license:expat)))

(define-public node-modelcontextprotocol-server-postgres
  (package
    (name "node-modelcontextprotocol-server-postgres")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri
        "https://registry.npmjs.org/@modelcontextprotocol/server-postgres/-/server-postgres-0.6.2.tgz")
       (sha256
        (base32 "0vx2bfm1i6vps4q37y0gs28vhhv0xrj8hjpv94cy6fmi04520mdg"))))
    (build-system node-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)
          (add-after 'patch-dependencies 'delete-dev-dependencies
            (lambda _
              (modify-json (delete-dependencies '("shx" "@types/pg"
                                                  "typescript"))))))))
    (inputs (list node-modelcontextprotocol-sdk-1.0.1 node-pg-8.23.0))
    (home-page "https://modelcontextprotocol.io")
    (synopsis "MCP server for interacting with PostgreSQL databases")
    (description
     "A Model Context Protocol server that provides read-only access to
PostgreSQL databases: it lets MCP clients inspect table schemas and run
read-only SQL queries, each executed inside a @code{READ ONLY} transaction.
The target database is given as a connection-string CLI argument.")
    (license license:expat)))
