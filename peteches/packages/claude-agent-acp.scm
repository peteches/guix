(define-module (peteches packages claude-agent-acp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (peteches packages claude-agent-acp-deps)
  #:export (node-agentclientprotocol-claude-agent-acp-0.42.0))

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
