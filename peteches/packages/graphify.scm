;; graphify.scm --- graphify knowledge-graph code-analysis tool
;;
;; https://github.com/Graphify-Labs/graphify — parses a codebase (source,
;; docs, SQL schemas, configs) into a local queryable knowledge graph using
;; deterministic tree-sitter AST parsing, with every edge traceable back to
;; its source rather than relying on a vector store. Ships as both a CLI
;; (`graphify`) and an MCP server (`graphify-mcp`) for use as a coding-
;; assistant skill.
;;
;; This packages only the core (non-optional) dependency set from
;; pyproject.toml, plus the `mcp` extra's `mcp` package so that
;; `graphify-mcp` works over its default stdio transport. It deliberately
;; leaves out the other half of that extra, `starlette` — graphify's own
;; pyproject.toml floors it at >=1.3.1 for two CVE fixes, but Guix only
;; carries 0.49.1, and `starlette` is only imported (lazily) by
;; graphify/serve.py's HTTP transport path, so simply not offering that
;; transport avoids shipping the vulnerable version rather than patching
;; around the floor.  The `neo4j`, `falkordb`, `pdf`, `watch`, `svg`,
;; `leiden`, `office`, `postgres`, `video`, LLM-backend (`kimi`/`ollama`/
;; `bedrock`/`anthropic`/`gemini`/`openai`), `chinese`, `sql`, `pascal`, `dm`
;; and `terraform` extras are not wired up either. The upstream PyPI
;; project name is "graphifyy" (double-y); this package keeps the
;; project's own "graphify" name.

(define-module (peteches packages graphify)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)   ;pypi-uri
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tree-sitter)
  #:use-module (peteches packages graphify-tree-sitter-deps)
  #:use-module (peteches packages python-deps))

(define-public graphify
  (package
    (name "graphify")
    (version "0.9.48")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "graphifyy" version))
       (sha256
        (base32 "07p4sn63s6i1qmb87y26wd8jnaxnkak1qj9lrc698rj8h21srshl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; The suite (155 files under tests/) exercises optional LLM-backend,
      ;; network-dependent, and multi-host-CLI-install integrations well
      ;; beyond the core dependency set packaged here; skip rather than
      ;; hand-pick a network-free subset.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'drop-tree-sitter-version-constraints
            (lambda _
              ;; Guix pins exactly one version of each tree-sitter-<lang>
              ;; input, so there's no ambiguity for these constraints to
              ;; resolve, but several don't hold against what Guix carries
              ;; (e.g. Fortran 0.5.1 vs >=0.6, Groovy 0.0.1 vs >=0.1,
              ;; Kotlin 0.3.8 vs >=1.0) — and some grammar repos' own
              ;; bindings/python pyproject.toml ships a stale `version`
              ;; unrelated to the grammar's actual version (e.g.
              ;; tree-sitter-powershell's is "0.0.1" against a required
              ;; >=0.26), which no amount of matching Guix's grammar
              ;; version here could satisfy anyway. Strip the constraints
              ;; instead of chasing each mismatch individually.
              (substitute* "pyproject.toml"
                (("(tree-sitter[a-zA-Z0-9_-]*)>=[^\"]*" all name)
                 name)))))))
    (native-inputs (list python-setuptools))
    (propagated-inputs
     (list python-networkx
           python-numpy
           python-rapidfuzz
           ;; For `graphify-mcp` (stdio transport only — see the module
           ;; comment above on why `starlette` isn't propagated too; the
           ;; HTTP transport that needs it is simply unavailable here).
           python-mcp-1.26
           python-tree-sitter
           python-tree-sitter-python
           python-tree-sitter-javascript
           python-tree-sitter-typescript
           python-tree-sitter-go
           python-tree-sitter-rust
           python-tree-sitter-java
           python-tree-sitter-groovy
           python-tree-sitter-c
           python-tree-sitter-cpp
           python-tree-sitter-ruby
           python-tree-sitter-c-sharp
           python-tree-sitter-kotlin
           python-tree-sitter-scala
           python-tree-sitter-php
           python-tree-sitter-swift
           python-tree-sitter-lua
           python-tree-sitter-zig
           python-tree-sitter-powershell
           python-tree-sitter-elixir
           python-tree-sitter-objc
           python-tree-sitter-julia
           python-tree-sitter-verilog
           python-tree-sitter-fortran
           python-tree-sitter-bash
           python-tree-sitter-json))
    (home-page "https://github.com/Graphify-Labs/graphify")
    (synopsis "Turn a codebase into a queryable knowledge graph")
    (description
     "Graphify parses source code, documentation, SQL schemas, and configs
into a local knowledge graph using deterministic tree-sitter AST parsing,
with every edge traceable back to its source rather than relying on a
vector store.  It provides both a @command{graphify} CLI and a
@command{graphify-mcp} MCP server for use as a coding-assistant skill in
Claude Code, Cursor, Codex, Gemini CLI, and others.")
    (license (list license:asl2.0 license:expat))))
