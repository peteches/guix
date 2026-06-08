(define-module (peteches packages claude-agent-acp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module (guix-science packages rstudio-node)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages base))

(define-public node-agentclientprotocol-sdk-0.24.0
  (package
   (name "node-agentclientprotocol-sdk")
   (version "0.24.0")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/@agentclientprotocol/sdk/-/sdk-0.24.0.tgz")
     (sha256
      (base32 "1127gpsvqj8dmcn27lnqmwr4yr31zpv3l0q0ykga8rab26qw9bx4"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("tsx" "zod"
								       "eslint"
								       "vitest"
								       "globals"
								       "typedoc"
								       "prettier"
								       "@eslint/js"
								       "typescript"
								       "@types/node"
								       "http-server"
								       "concurrently"
								       "@hey-api/openapi-ts"
								       "typedoc-github-theme"
								       "eslint-config-prettier"
								       "@typescript-eslint/parser"
								       "@typescript-eslint/eslint-plugin"))))))))
   (home-page "https://github.com/agentclientprotocol/typescript-sdk#readme")
   (synopsis
    "The Agent Client Protocol (ACP) is a protocol that standardizes communication between *code editors* (interactive programs for viewing and editing source code) and *coding agents* (programs that use generative AI to autonomously modify code).")
   (description
    "The Agent Client Protocol (ACP) is a protocol that standardizes communication between *code editors* (interactive programs for viewing and editing source code) and *coding agents* (programs that use generative AI to autonomously modify code).")
   (license license:asl2.0)))

(define-public node-stablelib-base64-1.0.1
  (package
   (name "node-stablelib-base64")
   (version "1.0.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/@stablelib/base64/-/base64-1.0.1.tgz")
     (sha256
      (base32 "0sjrdadiyy1xyh6lp9pdlbxk8qdx6lgxn9bqdi1qfgqd5qfp2c01"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@stablelib/benchmark"))))))))
   (home-page
    "https://github.com/StableLib/stablelib/tree/master/packages/base64")
   (synopsis "Base64 encoding and decoding")
   (description "Base64 encoding and decoding")
   (license license:expat)))

(define-public node-fast-sha256-1.3.0
  (package
   (name "node-fast-sha256")
   (version "1.3.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/fast-sha256/-/fast-sha256-1.3.0.tgz")
     (sha256
      (base32 "1xl45kfg22wr0qnzybw05aahbwdlcl7lsz8f4nqwzjadb6i7x2ag"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("faucet" "tape" "tslint"
								       "typescript" "uglify-js"))))))))
   (home-page "https://github.com/dchest/fast-sha256-js#readme")
   (synopsis
    "SHA-256, HMAC and PBKDF2 implementation with typed arrays for modern browsers and Node.js")
   (description
    "SHA-256, HMAC and PBKDF2 implementation with typed arrays for modern browsers and Node.js")
   (license license:unlicense)))

(define-public node-standardwebhooks-1.0.0
  (package
   (name "node-standardwebhooks")
   (version "1.0.0")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/standardwebhooks/-/standardwebhooks-1.0.0.tgz")
     (sha256
      (base32 "1gfv22b5wm5pklx1j3xxjyjk5v4vaw06zpw89jgx6g9hicdqwpck"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@stablelib/utf8"
								       "@types/jest"
								       "@typescript-eslint/eslint-plugin"
								       "@typescript-eslint/parser"
								       "@typescript-eslint/typescript-estree"
								       "eslint"
								       "jest"
								       "prettier"
								       "ts-jest"
								       "typescript"))))))))
   (inputs (list node-fast-sha256-1.3.0 node-stablelib-base64-1.0.1))
   (home-page
    "https://github.com/standard-webhooks/standard-webhooks/tree/main/libraries/javascript")
   (synopsis "Standard Webhooks for TypeScript")
   (description "Standard Webhooks for TypeScript")
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

(define-public node-anthropic-ai-sdk-0.102.0
  (package
   (name "node-anthropic-ai-sdk")
   (version "0.102.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/@anthropic-ai/sdk/-/sdk-0.102.0.tgz")
     (sha256
      (base32 "0jrkrv9wh3sm98nl3h04pjdk7zd1bnljhifwqw3gxzr6ysnw2ppi"))))
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
   (inputs (list node-json-schema-to-ts-3.1.1 node-standardwebhooks-1.0.0
                 node-zod-4.4.3))
   (home-page "https://github.com/anthropics/anthropic-sdk-typescript#readme")
   (synopsis "The official TypeScript library for the Anthropic API")
   (description "The official TypeScript library for the Anthropic API")
   (license license:expat)))

(define-public node-cfworker-json-schema-4.1.1
  (package
   (name "node-cfworker-json-schema")
   (version "4.1.1")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/@cfworker/json-schema/-/json-schema-4.1.1.tgz")
     (sha256
      (base32 "00q2c41ji1v1jar17yvd5hz8h9kpbvxnx2m73xkz01858mmhga7k"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@types/chai" "@types/mocha"
								       "chai"
								       "json-schema-test-suite"
								       "esbuild"
								       "mocha"
								       "typescript"
								       "wrangler"))))))))
   (home-page
    "https://github.com/cfworker/cfworker/tree/master/packages/json-schema/README.md")
   (synopsis
    "A JSON schema validator that will run on Cloudflare workers. Supports drafts 4, 7, 2019-09, and 2020-12.")
   (description
    "A JSON schema validator that will run on Cloudflare workers. Supports drafts 4, 7, 2019-09, and 2020-12.")
   (license license:expat)))

(define-public node-cors-2.8.6
  (package
   (name "node-cors")
   (version "2.8.6")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/cors/-/cors-2.8.6.tgz")
     (sha256
      (base32 "1zy1q5b9ny116yfmsr8vr1c9srklrns558iq28780h3i74j2291j"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("after" "eslint" "express"
								       "mocha" "nyc" "supertest"))))))))
   (inputs (list node-vary-1.1.2 node-object-assign-4.1.7))
   (home-page "https://github.com/expressjs/cors#readme")
   (synopsis "Node.js CORS middleware")
   (description "Node.js CORS middleware")
   (license license:expat)))

(define-public node-jose-6.2.3
  (package
   (name "node-jose")
   (version "6.2.3")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/jose/-/jose-6.2.3.tgz")
     (sha256
      (base32 "19amggxlhcyffqqakbkq1f1yc2617smjjshv75jhciqqkzh8ni64"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build))))
   (home-page "https://github.com/panva/jose")
   (synopsis
    "JWA, JWS, JWE, JWT, JWK, JWKS for Node.js, Browser, Cloudflare Workers, Deno, Bun, and other Web-interoperable runtimes")
   (description
    "JWA, JWS, JWE, JWT, JWK, JWKS for Node.js, Browser, Cloudflare Workers, Deno, Bun, and other Web-interoperable runtimes")
   (license license:expat)))

(define-public node-fast-uri-3.1.2
  (package
   (name "node-fast-uri")
   (version "3.1.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/fast-uri/-/fast-uri-3.1.2.tgz")
     (sha256
      (base32 "0hy16g7i9f5yw6spsl90fh2gygl7h17kr9hvg28sp3gz07ly7zlk"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("ajv" "eslint" "neostandard"
								       "playwright-test" "tape"
								       "tsd"))))))))
   (home-page "https://github.com/fastify/fast-uri")
   (synopsis "Dependency-free RFC 3986 URI toolbox")
   (description "Dependency-free RFC 3986 URI toolbox")
   (license license:bsd-3)))

(define-public node-ajv-8.20.0
  (package
   (name "node-ajv")
   (version "8.20.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/ajv/-/ajv-8.20.0.tgz")
     (sha256
      (base32 "1cz7yr42yf4kb0znhwyxslqrxqcr2j5z0538zdgcrf5vjflb7w5j"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@ajv-validator/config"
								       "@rollup/plugin-commonjs"
								       "@rollup/plugin-json"
								       "@rollup/plugin-node-resolve"
								       "@rollup/plugin-typescript"
								       "@types/chai"
								       "@types/mocha"
								       "@types/node"
								       "@types/require-from-string"
								       "@typescript-eslint/eslint-plugin"
								       "@typescript-eslint/parser"
								       "ajv-formats"
								       "browserify"
								       "chai"
								       "cross-env"
								       "dayjs"
								       "dayjs-plugin-utc"
								       "eslint"
								       "eslint-config-prettier"
								       "glob"
								       "husky"
								       "jimp"
								       "js-beautify"
								       "json-schema-test"
								       "karma"
								       "karma-chrome-launcher"
								       "karma-mocha"
								       "lint-staged"
								       "mocha"
								       "module-from-string"
								       "node-fetch"
								       "nyc"
								       "prettier"
								       "re2"
								       "rollup"
								       "rollup-plugin-terser"
								       "ts-node"
								       "tsify"
								       "typescript"
								       "uri-js"))))))))
   (inputs (list node-require-from-string-2.0.2
                 node-json-schema-traverse-1.0.0 node-fast-uri-3.1.2
                 node-fast-deep-equal-3.1.3))
   (home-page "https://ajv.js.org")
   (synopsis "Another JSON Schema Validator")
   (description "Another JSON Schema Validator")
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
				   (modify-json (delete-dependencies '("@ajv-validator/config"
								       "@types/jest"
								       "@types/node"
								       "@typescript-eslint/eslint-plugin"
								       "@typescript-eslint/parser"
								       "ajv"
								       "eslint"
								       "eslint-config-prettier"
								       "husky"
								       "jest"
								       "json-schema-test"
								       "lint-staged"
								       "prettier"
								       "ts-jest"
								       "typescript"))))))))
   (inputs (list node-ajv-8.20.0))
   (home-page "https://github.com/ajv-validator/ajv-formats#readme")
   (synopsis "Format validation for Ajv v7+")
   (description "Format validation for Ajv v7+")
   (license license:expat)))

(define-public node-eventsource-3.0.7
  (package
   (name "node-eventsource")
   (version "3.0.7")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/eventsource/-/eventsource-3.0.7.tgz")
     (sha256
      (base32 "14hw12k1s7h7bdh5x7sdlx4ic9p4dw6mb7ppbafb6nbf36xx8qkw"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("tsx" "sinon"
								       "eslint"
								       "rimraf"
								       "undici"
								       "esbuild"
								       "prettier"
								       "playwright"
								       "typescript"
								       "@types/sinon"
								       "semantic-release"
								       "@sanity/pkg-utils"
								       "@tsconfig/strictest"
								       "eventsource-encoder"
								       "eslint-config-sanity"
								       "eslint-config-prettier"
								       "rollup-plugin-visualizer"
								       "@typescript-eslint/parser"
								       "@sanity/semantic-release-preset"
								       "@typescript-eslint/eslint-plugin"))))))))
   (inputs (list node-eventsource-parser-3.1.0))
   (home-page "https://github.com/EventSource/eventsource#readme")
   (synopsis
    "WhatWG/W3C compliant EventSource client for Node.js and browsers")
   (description
    "WhatWG/W3C compliant EventSource client for Node.js and browsers")
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
				   (modify-json (delete-dependencies '("jest" "diverge" "esbuild"
								       "typescript" "@types/jest"
								       "@types/node"))))))))
   (home-page "https://github.com/crouchcd/pkce-challenge#readme")
   (synopsis
    "Generate or verify a Proof Key for Code Exchange (PKCE) challenge pair")
   (description
    "Generate or verify a Proof Key for Code Exchange (PKCE) challenge pair")
   (license license:expat)))

(define-public node-hono-4.12.23
  (package
   (name "node-hono")
   (version "4.12.23")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/hono/-/hono-4.12.23.tgz")
     (sha256
      (base32 "0xfw293rjjpk4p5maagm000yxw50rybmssn4w5y21vjqn51s87ls"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@hono/eslint-config"
								       "@hono/node-server"
								       "@types/glob"
								       "@types/jsdom"
								       "@types/node"
								       "@types/ws"
								       "@typescript/native-preview"
								       "@vitest/coverage-v8"
								       "arg"
								       "bun-types"
								       "editorconfig-checker"
								       "esbuild"
								       "eslint"
								       "glob"
								       "jsdom"
								       "msw"
								       "np"
								       "oxc-parser"
								       "pkg-pr-new"
								       "prettier"
								       "publint"
								       "typescript"
								       "undici"
								       "vite-plugin-fastly-js-compute"
								       "vitest"
								       "wrangler"
								       "ws"
								       "zod"))))))))
   (home-page "https://hono.dev")
   (synopsis "Web framework built on Web Standards")
   (description "Web framework built on Web Standards")
   (license license:expat)))

(define-public node-hono-node-server-1.19.14
  (package
   (name "node-hono-node-server")
   (version "1.19.14")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/@hono/node-server/-/node-server-1.19.14.tgz")
     (sha256
      (base32 "1bdrlpbjymv8n1zbjgrryivkhbbs4rp5hkpf0ffvin4ndws6z58b"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("np" "hono"
								       "jest"
								       "tsup"
								       "eslint"
								       "publint"
								       "ts-jest"
								       "prettier"
								       "supertest"
								       "typescript"
								       "@types/jest"
								       "@types/node"
								       "@types/supertest"
								       "@whatwg-node/fetch"
								       "@hono/eslint-config"))))))))
   (home-page "https://github.com/honojs/node-server")
   (synopsis "Node.js Adapter for Hono")
   (description "Node.js Adapter for Hono")
   (license license:expat)))

(define-public node-json-schema-typed-8.0.2
  (package
   (name "node-json-schema-typed")
   (version "8.0.2")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/json-schema-typed/-/json-schema-typed-8.0.2.tgz")
     (sha256
      (base32 "1jbbs2nxga2rfg4hxcipg0zn7j5d3r9rjziz7c3w7x7wwhk43p77"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build))))
   (home-page
    "https://github.com/RemyRylan/json-schema-typed/tree/main/dist/node")
   (synopsis
    "JSON Schema TypeScript definitions with complete inline documentation.")
   (description
    "JSON Schema TypeScript definitions with complete inline documentation.")
   (license license:bsd-2)))

(define-public node-eventsource-parser-3.1.0
  (package
   (name "node-eventsource-parser")
   (version "3.1.0")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/eventsource-parser/-/eventsource-parser-3.1.0.tgz")
     (sha256
      (base32 "0fh5qpdgyxd28yrna59999dwq5h3xpgvpj5w2zm7ch1ix7h4ra7c"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@sanity/pkg-utils"
								       "@sanity/semantic-release-preset"
								       "@sanity/tsconfig"
								       "@types/node"
								       "eventsource-encoder"
								       "knip"
								       "mitata"
								       "oxfmt"
								       "oxlint"
								       "rimraf"
								       "rollup-plugin-visualizer"
								       "semantic-release"
								       "terser"
								       "typescript"
								       "vitest"))))))))
   (home-page "https://github.com/rexxars/eventsource-parser#readme")
   (synopsis
    "Streaming, source-agnostic EventSource/Server-Sent Events parser")
   (description
    "Streaming, source-agnostic EventSource/Server-Sent Events parser")
   (license license:expat)))

(define-public node-cookie-0.7.2
  (package
   (name "node-cookie")
   (version "0.7.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/cookie/-/cookie-0.7.2.tgz")
     (sha256
      (base32 "084ymsdgqj3jc00gh39cbfbmh1vval1wy2ifd88hlqqw4pw61cbn"))))
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
								       "benchmark"
								       "top-sites"
								       "safe-buffer"
								       "beautify-benchmark"
								       "eslint-plugin-markdown"))))))))
   (home-page "https://github.com/jshttp/cookie#readme")
   (synopsis "HTTP server cookie parsing and serialization")
   (description "HTTP server cookie parsing and serialization")
   (license license:expat)))

(define-public node-is-promise-4.0.0
  (package
   (name "node-is-promise")
   (version "4.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/is-promise/-/is-promise-4.0.0.tgz")
     (sha256
      (base32 "19s5njn24k6ra9c4skkzjhjfaq0d1izkxxicfsw07ykn70br2f45"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build))))
   (home-page "https://github.com/then/is-promise#readme")
   (synopsis "Test whether an object looks like a promises-a+ promise")
   (description "Test whether an object looks like a promises-a+ promise")
   (license license:expat)))

(define-public node-path-to-regexp-8.4.2
  (package
   (name "node-path-to-regexp")
   (version "8.4.2")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/path-to-regexp/-/path-to-regexp-8.4.2.tgz")
     (sha256
      (base32 "0y5xr95pidzbnbi524y08d3gnapr9p2q0yyczrya58mhaff2lwg8"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@borderless/ts-scripts"
								       "@size-limit/preset-small-lib"
								       "@types/node"
								       "@types/semver"
								       "@vitest/coverage-v8"
								       "recheck"
								       "size-limit"
								       "typescript"
								       "vitest"))))))))
   (home-page "https://github.com/pillarjs/path-to-regexp#readme")
   (synopsis "Express style path to RegExp utility")
   (description "Express style path to RegExp utility")
   (license license:expat)))

(define-public node-router-2.2.0
  (package
   (name "node-router")
   (version "2.2.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/router/-/router-2.2.0.tgz")
     (sha256
      (base32 "0qclnvx6zzlrqj03wz8lyh9rzwickiz8c7czm3vig5csncvsyi5i"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("finalhandler" "mocha" "nyc"
								       "run-series" "standard"
								       "supertest"))))))))
   (inputs (list node-path-to-regexp-8.4.2 node-parseurl-1.3.3
                 node-is-promise-4.0.0 node-depd-2.0.0 node-debug-4.4.3))
   (home-page "https://github.com/pillarjs/router#readme")
   (synopsis "Simple middleware-style router")
   (description "Simple middleware-style router")
   (license license:expat)))

(define-public node-negotiator-1.0.0
  (package
   (name "node-negotiator")
   (version "1.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/negotiator/-/negotiator-1.0.0.tgz")
     (sha256
      (base32 "015w5p5p4sb02cd9zq20mp7l32jspq206p6d4g355b603ppdz8mm"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("nyc" "mocha" "eslint"
								       "eslint-plugin-markdown"))))))))
   (home-page "https://github.com/jshttp/negotiator#readme")
   (synopsis "HTTP content negotiation")
   (description "HTTP content negotiation")
   (license license:expat)))

(define-public node-accepts-2.0.0
  (package
   (name "node-accepts")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/accepts/-/accepts-2.0.0.tgz")
     (sha256
      (base32 "0hi56wcavwsv8s4mpvks7gywmjdiqcqa0a91vga8rpw8gmgr2g8p"))))
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
								       "deep-equal"
								       "eslint-plugin-node"
								       "eslint-plugin-import"
								       "eslint-plugin-promise"
								       "eslint-config-standard"
								       "eslint-plugin-markdown"
								       "eslint-plugin-standard"))))))))
   (inputs (list node-negotiator-1.0.0 node-mime-types-3.0.2))
   (home-page "https://github.com/jshttp/accepts#readme")
   (synopsis "Higher-level content negotiation")
   (description "Higher-level content negotiation")
   (license license:expat)))

(define-public node-qs-6.15.2
  (package
   (name "node-qs")
   (version "6.15.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/qs/-/qs-6.15.2.tgz")
     (sha256
      (base32 "15xqhlhplzsvwyrgfrxb3bc20k054i9d3h99rmdiy632mjrx96g6"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@browserify/envify"
								       "@browserify/uglifyify"
								       "@ljharb/eslint-config"
								       "browserify"
								       "bundle-collapser"
								       "common-shakeify"
								       "eclint"
								       "es-value-fixtures"
								       "eslint"
								       "evalmd"
								       "for-each"
								       "glob"
								       "has-bigints"
								       "has-override-mistake"
								       "has-property-descriptors"
								       "has-proto"
								       "has-symbols"
								       "iconv-lite"
								       "in-publish"
								       "jackspeak"
								       "jiti"
								       "mkdirp"
								       "mock-property"
								       "module-deps"
								       "npmignore"
								       "nyc"
								       "object-inspect"
								       "qs-iconv"
								       "safe-publish-latest"
								       "safer-buffer"
								       "tape"
								       "unassertify"))))))))
   (inputs (list node-side-channel-1.1.0))
   (home-page "https://github.com/ljharb/qs")
   (synopsis
    "A querystring parser that supports nesting and arrays, with a depth limit")
   (description
    "A querystring parser that supports nesting and arrays, with a depth limit")
   (license license:bsd-3)))

(define-public node-content-type-2.0.0
  (package
   (name "node-content-type")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/content-type/-/content-type-2.0.0.tgz")
     (sha256
      (base32 "01mwl6jp5rqkwinh99cl76qlnbcz4w0qxx9ra0n2rppl8jfnim52"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@borderless/ts-scripts"
								       "@vitest/coverage-v8"
								       "typescript" "vitest"))))))))
   (home-page "https://github.com/jshttp/content-type#readme")
   (synopsis "Create and parse HTTP Content-Type header")
   (description "Create and parse HTTP Content-Type header")
   (license license:expat)))

(define-public node-media-typer-1.1.0
  (package
   (name "node-media-typer")
   (version "1.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/media-typer/-/media-typer-1.1.0.tgz")
     (sha256
      (base32 "1ghrgjcv59qna3h37himz6p7qsby9vki3gjrnv7r5z0y3lg57p5m"))))
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
								       "eslint-plugin-node"
								       "eslint-plugin-import"
								       "eslint-plugin-promise"
								       "eslint-config-standard"
								       "eslint-plugin-markdown"
								       "eslint-plugin-standard"))))))))
   (home-page "https://github.com/jshttp/media-typer#readme")
   (synopsis "Simple RFC 6838 media type parser and formatter")
   (description "Simple RFC 6838 media type parser and formatter")
   (license license:expat)))

(define-public node-type-is-2.1.0
  (package
   (name "node-type-is")
   (version "2.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/type-is/-/type-is-2.1.0.tgz")
     (sha256
      (base32 "1z02ncgm0xqw2q9g4k20jbac129hk1dfxz5l5h68wj6dd66hhlws"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("eslint"
								       "eslint-config-standard"
								       "eslint-plugin-import"
								       "eslint-plugin-markdown"
								       "eslint-plugin-node"
								       "eslint-plugin-promise"
								       "eslint-plugin-standard"
								       "mocha"
								       "nyc"))))))))
   (inputs (list node-mime-types-3.0.2 node-media-typer-1.1.0
                 node-content-type-2.0.0))
   (home-page "https://github.com/jshttp/type-is#readme")
   (synopsis "Infer the content-type of a request.")
   (description "Infer the content-type of a request.")
   (license license:expat)))

(define-public node-raw-body-3.0.2
  (package
   (name "node-raw-body")
   (version "3.0.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/raw-body/-/raw-body-3.0.2.tgz")
     (sha256
      (base32 "1nqrhjp2v55z7rd13f1qlgnk6vgc4hmd9c8awnxmipina012mpk6"))))
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
								       "bluebird"
								       "neostandard"
								       "safe-buffer"
								       "readable-stream"
								       "@stylistic/eslint-plugin"
								       "@stylistic/eslint-plugin-js"))))))))
   (inputs (list node-http-errors-2.0.1 node-iconv-lite-0.7.2
                 node-unpipe-1.0.0 node-bytes-3.1.2))
   (home-page "https://github.com/stream-utils/raw-body#readme")
   (synopsis "Get and validate the raw body of a readable stream.")
   (description "Get and validate the raw body of a readable stream.")
   (license license:expat)))

(define-public node-iconv-lite-0.7.2
  (package
   (name "node-iconv-lite")
   (version "0.7.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.7.2.tgz")
     (sha256
      (base32 "0gm0im7640vi4j29ssh9zf1ynfrffkjm4mig9np3b4hzwxlngm31"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("nyc" "async"
								       "errto"
								       "iconv"
								       "mocha"
								       "unorm"
								       "eslint"
								       "semver"
								       "request"
								       "bench-node"
								       "typescript"
								       "@types/node"
								       "expect-type"
								       "neostandard"
								       "@arethetypeswrong/cli"
								       "@stylistic/eslint-plugin"
								       "@stylistic/eslint-plugin-js"))))))))
   (inputs (list node-safer-buffer-2.1.2))
   (home-page "https://github.com/pillarjs/iconv-lite")
   (synopsis "Convert character encodings in pure javascript.")
   (description "Convert character encodings in pure javascript.")
   (license license:expat)))

(define-public node-body-parser-2.2.2
  (package
   (name "node-body-parser")
   (version "2.2.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/body-parser/-/body-parser-2.2.2.tgz")
     (sha256
      (base32 "02x3fgd42cf09bdadwb84k7jb31b0nq0m8kkb70aqpc0gqs9g3f2"))))
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
								       "supertest"
								       "eslint-plugin-node"
								       "eslint-plugin-import"
								       "eslint-plugin-promise"
								       "eslint-config-standard"
								       "eslint-plugin-markdown"
								       "eslint-plugin-standard"))))))))
   (inputs (list node-content-type-1.0.5
                 node-on-finished-2.4.1
                 node-http-errors-2.0.1
                 node-iconv-lite-0.7.2
                 node-raw-body-3.0.2
                 node-type-is-2.1.0
                 node-debug-4.4.3
                 node-bytes-3.1.2
                 node-qs-6.15.2))
   (home-page "https://github.com/expressjs/body-parser#readme")
   (synopsis "Node.js body parsing middleware")
   (description "Node.js body parsing middleware")
   (license license:expat)))

(define-public node-finalhandler-2.1.1
  (package
   (name "node-finalhandler")
   (version "2.1.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/finalhandler/-/finalhandler-2.1.1.tgz")
     (sha256
      (base32 "0wmqm14f4xd6gf6s6117sn0xgvx935bdcrxyidcv6856a7y9p512"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("eslint"
								       "eslint-config-standard"
								       "eslint-plugin-import"
								       "eslint-plugin-markdown"
								       "eslint-plugin-node"
								       "eslint-plugin-promise"
								       "eslint-plugin-standard"
								       "mocha"
								       "nyc"
								       "supertest"))))))))
   (inputs (list node-statuses-2.0.2
                 node-parseurl-1.3.3
                 node-on-finished-2.4.1
                 node-escape-html-1.0.3
                 node-encodeurl-2.0.0
                 node-debug-4.4.3))
   (home-page "https://github.com/pillarjs/finalhandler#readme")
   (synopsis "Node.js final http responder")
   (description "Node.js final http responder")
   (license license:expat)))

(define-public node-fresh-2.0.0
  (package
   (name "node-fresh")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/fresh/-/fresh-2.0.0.tgz")
     (sha256
      (base32 "1mvs4wihlr6bw05h7q12771qrkwrssm26bk80ysv4qjzn1x3j25d"))))
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
								       "benchmark"
								       "beautify-benchmark"
								       "eslint-plugin-node"
								       "eslint-plugin-import"
								       "eslint-plugin-promise"
								       "eslint-config-standard"
								       "eslint-plugin-markdown"
								       "eslint-plugin-standard"))))))))
   (home-page "https://github.com/jshttp/fresh#readme")
   (synopsis "HTTP response freshness testing")
   (description "HTTP response freshness testing")
   (license license:expat)))

(define-public node-mime-types-3.0.2
  (package
   (name "node-mime-types")
   (version "3.0.2")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/mime-types/-/mime-types-3.0.2.tgz")
     (sha256
      (base32 "1b6j7px7npv0gli3v249m5a1rc2m8x3qxxpva23zy0y3af1x579g"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("eslint"
								       "eslint-config-standard"
								       "eslint-plugin-import"
								       "eslint-plugin-markdown"
								       "eslint-plugin-node"
								       "eslint-plugin-promise"
								       "eslint-plugin-standard"
								       "mocha"
								       "nyc"))))))))
   (inputs (list node-mime-db-1.54.0))
   (home-page "https://github.com/jshttp/mime-types#readme")
   (synopsis "The ultimate javascript content-type utility.")
   (description "The ultimate javascript content-type utility.")
   (license license:expat)))

(define-public node-send-1.2.1
  (package
   (name "node-send")
   (version "1.2.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/send/-/send-1.2.1.tgz")
     (sha256
      (base32 "0wn46l21fl21yf5abnx3yr6fr2kq4cxm7ggf5g5xs8yx2srly9gs"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("after" "eslint"
								       "eslint-config-standard"
								       "eslint-plugin-import"
								       "eslint-plugin-markdown"
								       "eslint-plugin-node"
								       "eslint-plugin-promise"
								       "eslint-plugin-standard"
								       "mocha"
								       "nyc"
								       "supertest"))))))))
   (inputs (list node-statuses-2.0.2
                 node-range-parser-1.2.1
                 node-on-finished-2.4.1
                 node-ms
                 node-mime-types-3.0.2
                 node-http-errors-2.0.1
                 node-fresh-2.0.0
                 node-etag-1.8.1
                 node-escape-html-1.0.3
                 node-encodeurl-2.0.0
                 node-debug-4.4.3))
   (home-page "https://github.com/pillarjs/send#readme")
   (synopsis
    "Better streaming static file server with Range and conditional-GET support")
   (description
    "Better streaming static file server with Range and conditional-GET support")
   (license license:expat)))

(define-public node-serve-static-2.2.1
  (package
   (name "node-serve-static")
   (version "2.2.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/serve-static/-/serve-static-2.2.1.tgz")
     (sha256
      (base32 "0hszcbfcncifwgxk060jy1andx4b7pzjbvig40cfnwlknlmzgm1n"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("eslint"
								       "eslint-config-standard"
								       "eslint-plugin-import"
								       "eslint-plugin-markdown"
								       "eslint-plugin-node"
								       "eslint-plugin-promise"
								       "eslint-plugin-standard"
								       "mocha"
								       "nyc"
								       "supertest"))))))))
   (inputs (list node-send-1.2.1 node-parseurl-1.3.3 node-escape-html-1.0.3
                 node-encodeurl-2.0.0))
   (home-page "https://github.com/expressjs/serve-static#readme")
   (synopsis "Serve static files")
   (description "Serve static files")
   (license license:expat)))

(define-public node-cookie-signature-1.2.2
  (package
   (name "node-cookie-signature")
   (version "1.2.2")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/cookie-signature/-/cookie-signature-1.2.2.tgz")
     (sha256
      (base32 "1lsk6l4501i1sil49gdwdkaj0nzr5asm5ybx1ppn17i93jpvlasd"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("mocha" "should"))))))))
   (home-page "https://github.com/visionmedia/node-cookie-signature#readme")
   (synopsis "Sign and unsign cookies")
   (description "Sign and unsign cookies")
   (license license:expat)))

(define-public node-merge-descriptors-2.0.0
  (package
   (name "node-merge-descriptors")
   (version "2.0.0")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/merge-descriptors/-/merge-descriptors-2.0.0.tgz")
     (sha256
      (base32 "1khx20ml70ll3k69qsq8p9ybz967mykrks8ak79m46f7bwm3525f"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("ava" "xo"))))))))
   (home-page "https://github.com/sindresorhus/merge-descriptors#readme")
   (synopsis "Merge objects using their property descriptors")
   (description "Merge objects using their property descriptors")
   (license license:expat)))

(define-public node-content-disposition-1.1.0
  (package
   (name "node-content-disposition")
   (version "1.1.0")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/content-disposition/-/content-disposition-1.1.0.tgz")
     (sha256
      (base32 "0g09yjp20gl96bzgyq18bzc1vz5a23y244y10rkah8xim0jik2rg"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("c8" "eslint"
								       "eslint-plugin-node"
								       "eslint-plugin-import"
								       "eslint-plugin-promise"
								       "eslint-config-standard"
								       "eslint-plugin-markdown"
								       "eslint-plugin-standard"))))))))
   (home-page "https://github.com/jshttp/content-disposition#readme")
   (synopsis "Create and parse Content-Disposition header")
   (description "Create and parse Content-Disposition header")
   (license license:expat)))

(define-public node-express-5.2.1
  (package
   (name "node-express")
   (version "5.2.1")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/express/-/express-5.2.1.tgz")
     (sha256
      (base32 "01fhbm4bndc5d6sign60v8mh5xv828hlv74v8x9jchml09na2wqp"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("ejs" "hbs"
								       "nyc"
								       "after"
								       "mocha"
								       "vhost"
								       "eslint"
								       "marked"
								       "morgan"
								       "supertest"
								       "connect-redis"
								       "cookie-parser"
								       "cookie-session"
								       "express-session"
								       "method-override"
								       "pbkdf2-password"))))))))
   (inputs (list node-content-disposition-1.1.0
                 node-merge-descriptors-2.0.0
                 node-cookie-signature-1.2.2
                 node-serve-static-2.2.1
                 node-range-parser-1.2.1
                 node-finalhandler-2.1.1
                 node-content-type-1.0.5
                 node-on-finished-2.4.1
                 node-http-errors-2.0.1
                 node-escape-html-1.0.3
                 node-body-parser-2.2.2
                 node-proxy-addr-2.0.7
                 node-mime-types-3.0.2
                 node-encodeurl-2.0.0
                 node-statuses-2.0.2
                 node-parseurl-1.3.3
                 node-type-is-2.1.0
                 node-accepts-2.0.0
                 node-router-2.2.0
                 node-cookie-0.7.2
                 node-fresh-2.0.0
                 node-debug-4.4.3
                 node-vary-1.1.2
                 node-send-1.2.1
                 node-once
                 node-etag-1.8.1
                 node-depd-2.0.0
                 node-qs-6.15.2))
   (home-page "https://expressjs.com/")
   (synopsis "Fast, unopinionated, minimalist web framework")
   (description "Fast, unopinionated, minimalist web framework")
   (license license:expat)))

(define-public node-ip-address-10.2.0
  (package
   (name "node-ip-address")
   (version "10.2.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://registry.npmjs.org/ip-address/-/ip-address-10.2.0.tgz")
     (sha256
      (base32 "1fn0lzwpagd0dslpzw272g7wvnqwqhib01a43g0pc5ad7z0dvq1p"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@types/chai" "@types/mocha"
								       "@typescript-eslint/eslint-plugin"
								       "@typescript-eslint/parser"
								       "c8"
								       "chai"
								       "eslint"
								       "eslint_d"
								       "eslint-config-airbnb"
								       "eslint-config-prettier"
								       "eslint-plugin-filenames"
								       "eslint-plugin-import"
								       "eslint-plugin-jsx-a11y"
								       "eslint-plugin-prettier"
								       "eslint-plugin-sort-imports-es6-autofix"
								       "mocha"
								       "monocart-coverage-reports"
								       "prettier"
								       "source-map-support"
								       "tsx"
								       "typedoc"
								       "typescript"))))))))
   (home-page "https://github.com/beaugunderson/ip-address#readme")
   (synopsis
    "A library for parsing IPv4 and IPv6 IP addresses in node and the browser.")
   (description
    "A library for parsing IPv4 and IPv6 IP addresses in node and the browser.")
   (license license:expat)))

(define-public node-express-rate-limit-8.5.2
  (package
   (name "node-express-rate-limit")
   (version "8.5.2")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/express-rate-limit/-/express-rate-limit-8.5.2.tgz")
     (sha256
      (base32 "14rs37ck9n7j3kzafp63jz512k3agcvpz3rarg08y4wcg588qdxm"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@biomejs/biome"
								       "@express-rate-limit/prettier"
								       "@express-rate-limit/tsconfig"
								       "@jest/globals"
								       "@types/express"
								       "@types/jest"
								       "@types/node"
								       "@types/supertest"
								       "del-cli"
								       "dts-bundle-generator"
								       "esbuild"
								       "express"
								       "husky"
								       "jest"
								       "lint-staged"
								       "mintlify"
								       "npm-run-all"
								       "prettier"
								       "ratelimit-header-parser"
								       "supertest"
								       "ts-jest"
								       "ts-node"
								       "typescript"))))))))
   (inputs (list node-ip-address-10.2.0 node-express-5.2.1))
   (home-page "https://github.com/express-rate-limit/express-rate-limit")
   (synopsis
    "Basic IP rate-limiting middleware for Express. Use to limit repeated requests to public APIs and/or endpoints such as password reset.")
   (description
    "Basic IP rate-limiting middleware for Express. Use to limit repeated requests to public APIs and/or endpoints such as password reset.")
   (license license:expat)))

(define-public node-zod-to-json-schema-3.25.2
  (package
   (name "node-zod-to-json-schema")
   (version "3.25.2")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/zod-to-json-schema/-/zod-to-json-schema-3.25.2.tgz")
     (sha256
      (base32 "1x4kiprzcpvwmzyw9i1h1m4lqjyxy23z3j8rq7b4zzralfsrm4d4"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@types/json-schema"
								       "@types/node"
								       "ajv"
								       "ajv-errors"
								       "ajv-formats"
								       "fast-diff"
								       "local-ref-resolver"
								       "rimraf"
								       "tsx"
								       "typescript"
								       "zod"))))))))
   (home-page "https://github.com/StefanTerdell/zod-to-json-schema#readme")
   (synopsis "Converts Zod schemas to Json Schemas")
   (description "Converts Zod schemas to Json Schemas")
   (license license:isc)))

(define-public node-modelcontextprotocol-sdk-1.29.0
  (package
   (name "node-modelcontextprotocol-sdk")
   (version "1.29.0")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/@modelcontextprotocol/sdk/-/sdk-1.29.0.tgz")
     (sha256
      (base32 "0v09ddnk77y2gxcazjxjks3bbsnk2zi4ifyqlm2fg2i8r874fl8w"))))
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
								       "eslint"
								       "vitest"
								       "prettier"
								       "@types/ws"
								       "supertest"
								       "@eslint/js"
								       "typescript"
								       "@types/cors"
								       "@types/node"
								       "@types/express"
								       "eslint-plugin-n"
								       "@types/supertest"
								       "typescript-eslint"
								       "@types/cross-spawn"
								       "@types/eventsource"
								       "@types/content-type"
								       "@cfworker/json-schema"
								       "eslint-config-prettier"
								       "@typescript/native-preview"
								       "@types/express-serve-static-core"
								       "@modelcontextprotocol/conformance"
								       "zod"))))))))
   (inputs (list node-zod-to-json-schema-3.25.2
                 node-express-rate-limit-8.5.2
                 node-eventsource-parser-3.1.0
                 node-json-schema-typed-8.0.2
                 node-hono-node-server-1.19.14
                 node-pkce-challenge-5.0.1
                 node-content-type-1.0.5
                 node-eventsource-3.0.7
                 node-cross-spawn-7.0.6
                 node-ajv-formats-3.0.1
                 node-raw-body-3.0.2
                 node-express-5.2.1
                 node-jose-6.2.3
                 node-hono-4.12.23
                 node-cors-2.8.6
                 node-zod-4.4.3
                 node-ajv-8.20.0
                 node-cfworker-json-schema-4.1.1))
   (home-page "https://modelcontextprotocol.io")
   (synopsis "Model Context Protocol implementation for TypeScript")
   (description "Model Context Protocol implementation for TypeScript")
   (license license:expat)))

(define-public node-anthropic-ai-claude-agent-sdk-0.3.165
  (package
   (name "node-anthropic-ai-claude-agent-sdk")
   (version "0.3.165")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/@anthropic-ai/claude-agent-sdk/-/claude-agent-sdk-0.3.165.tgz")
     (sha256
      (base32 "1dfg8ilf9j625z0ihxk1v8zvi4n9439dic87bxdpgqfjn8475lwx"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("zod" "@anthropic-ai/sdk"
								       "@modelcontextprotocol/sdk"))))))))
   (home-page "https://github.com/anthropics/claude-agent-sdk-typescript")
   (synopsis
    "SDK for building AI agents with Claude Code's capabilities. Programmatically interact with Claude to build autonomous agents that can understand codebases, edit files, and execute workflows.")
   (description
    "SDK for building AI agents with Claude Code's capabilities. Programmatically interact with Claude to build autonomous agents that can understand codebases, edit files, and execute workflows.")
   (license #f)))

(define-public node-agentclientprotocol-claude-agent-acp-0.42.0
  (package
   (name "node-agentclientprotocol-claude-agent-acp")
   (version "0.42.0")
   (source
    (origin
     (method url-fetch)
     (uri
      "https://registry.npmjs.org/@agentclientprotocol/claude-agent-acp/-/claude-agent-acp-0.42.0.tgz")
     (sha256
      (base32 "1b9586agc7liml5zy36ayyinpabxf35gpryc32f6f0n38pwypp4b"))))
   (build-system node-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'build)
		      (add-after 'patch-dependencies 'delete-dev-dependencies
				 (lambda _
				   (modify-json (delete-dependencies '("@anthropic-ai/sdk"
								       "@eslint/js"
								       "@types/node"
								       "@typescript-eslint/eslint-plugin"
								       "@typescript-eslint/parser"
								       "eslint"
								       "eslint-config-prettier"
								       "globals"
								       "prettier"
								       "ts-node"
								       "typescript"
								       "vitest"))))))))
   (inputs (list node-zod-4.4.3 node-anthropic-ai-claude-agent-sdk-0.3.165
                 node-agentclientprotocol-sdk-0.24.0))
   (home-page
    "https://github.com/agentclientprotocol/claude-agent-acp#readme")
   (synopsis
    "An ACP-compatible coding agent powered by the Claude Agent SDK (TypeScript)")
   (description
    "An ACP-compatible coding agent powered by the Claude Agent SDK (TypeScript)")
   (license license:asl2.0)))

