;; graphify-tree-sitter-deps.scm --- Python bindings for graphify's tree-sitter grammars
;;
;; graphify (see graphify.scm) depends on ~28 per-language `tree-sitter-<lang>`
;; PyPI packages, each just a thin Python binding over a C grammar. Guix
;; already carries the C grammars for every language graphify's core
;; (non-optional) dependency set needs, and Guix's own
;; `(gnu packages tree-sitter)` has a private helper,
;; `python-tree-sitter-grammar`, that wraps a grammar package into its Python
;; bindings variant — but that helper is defined with plain `define*`
;; (not `define*-public`), so it is never exported and can't be reused from
;; here. This file re-implements it (matching upstream's implementation) and
;; applies it to the grammar packages graphify needs that don't already have
;; a Python bindings package upstream. bash/c/cpp/c-sharp/elixir/fortran/
;; go/groovy/java/julia/kotlin/lua/objc/php/powershell/ruby/scala/swift/
;; typescript/verilog/zig — html/javascript/json/python/rust already have
;; upstream Python bindings and are used directly from
;; `(gnu packages tree-sitter)`.
;;
;; Two grammar versions are older than the range graphify's pyproject.toml
;; declares (tree-sitter-groovy: Guix has 0.0.1, graphify wants >=0.1,<0.3;
;; tree-sitter-kotlin: Guix has 0.3.8, graphify wants >=1.0,<2.0). Guix has
;; no equivalent of pip's version-range enforcement — whatever grammar
;; version is wired in is simply what gets used — so this still builds, but
;; graphify's Groovy/Kotlin extraction may hit call/type-ref cases those
;; older grammars don't model as richly as the versions upstream tested
;; against.

(define-module (peteches packages graphify-tree-sitter-deps)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages check)
  #:use-module (gnu packages node)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages tree-sitter))

(define* (python-tree-sitter-grammar pkg #:key (tests? #f))
  "Return a package for the Python bindings of the Tree-sitter grammar PKG.
Unlike upstream's private helper of the same name (which defaults to
TESTS? #t), this defaults to #f: most of these single-language grammar
repos ship no bindings/python test suite at all, and pytest treats
\"0 tests collected\" (exit code 5) as a failure, so enabling tests here
would require checking each of the ~20 call sites below individually."
  (package
    (inherit pkg)
    (name (string-append "python-" (package-name pkg)))
    (source (origin (inherit (package-source pkg))
                    (snippet #f) (patches '())))
    (build-system pyproject-build-system)
    (arguments (list #:tests? tests?))
    (native-inputs (append (if tests?
                               (list python-pytest python-tree-sitter)
                               '())
                           (list python-setuptools)))
    (description
     (string-append (package-description pkg)
                    "\n\nThis variant provides Python bindings."))))

(define-public python-tree-sitter-bash
  (python-tree-sitter-grammar tree-sitter-bash))

(define-public python-tree-sitter-c
  (python-tree-sitter-grammar tree-sitter-c))

(define-public python-tree-sitter-cpp
  (python-tree-sitter-grammar tree-sitter-cpp))

(define-public python-tree-sitter-c-sharp
  (python-tree-sitter-grammar tree-sitter-c-sharp))

(define-public python-tree-sitter-elixir
  (python-tree-sitter-grammar tree-sitter-elixir))

(define-public python-tree-sitter-fortran
  (python-tree-sitter-grammar tree-sitter-fortran))

(define-public python-tree-sitter-go
  (python-tree-sitter-grammar tree-sitter-go))

(define-public python-tree-sitter-groovy
  (python-tree-sitter-grammar tree-sitter-groovy))

(define-public python-tree-sitter-java
  (python-tree-sitter-grammar tree-sitter-java))

(define-public python-tree-sitter-julia
  (python-tree-sitter-grammar tree-sitter-julia))

(define-public python-tree-sitter-kotlin
  (python-tree-sitter-grammar tree-sitter-kotlin))

(define-public python-tree-sitter-lua
  (python-tree-sitter-grammar tree-sitter-lua))

(define-public python-tree-sitter-objc
  (python-tree-sitter-grammar tree-sitter-objc))

(define-public python-tree-sitter-php
  (python-tree-sitter-grammar tree-sitter-php))

(define-public python-tree-sitter-powershell
  ;; Two upstream bugs need working around:
  ;; - pyproject.toml declares
  ;;   `optional-dependencies.core = ["tree-sitter=0.24"]` — a single "="
  ;;   is not valid PEP 508, and recent setuptools validates
  ;;   pyproject.toml strictly, failing the build outright without a fix.
  ;; - setup.py's ext_modules sources list never got the external scanner
  ;;   added — it still has the generator's placeholder comment ("if your
  ;;   language uses an external scanner, add it here") despite the
  ;;   PowerShell grammar having one (src/scanner.c) — so the built
  ;;   extension is missing tree_sitter_powershell_external_scanner_create
  ;;   and fails to import.
  (package
    (inherit (python-tree-sitter-grammar tree-sitter-powershell))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-invalid-pep508-dependency
            (lambda _
              (substitute* "pyproject.toml"
                (("\"tree-sitter=0.24\"") "\"tree-sitter==0.24\""))))
          (add-after 'unpack 'add-missing-external-scanner
            (lambda _
              (substitute* "setup.py"
                (("\"src/parser.c\",")
                 "\"src/parser.c\",\n                \"src/scanner.c\",")))))))))

(define-public python-tree-sitter-ruby
  (python-tree-sitter-grammar tree-sitter-ruby))

(define-public python-tree-sitter-scala
  (python-tree-sitter-grammar tree-sitter-scala))

(define-public python-tree-sitter-swift
  ;; Unlike most of the other languages here, this grammar repo doesn't
  ;; check in a pre-generated src/parser.c — it's produced from grammar.js
  ;; by `tree-sitter generate` (which shells out to `node`), a step Guix's
  ;; own tree-sitter-build-system runs for the plain C grammar package but
  ;; that the pristine origin re-used here doesn't include. Regenerate it
  ;; before setup.py's build_ext tries to compile it.
  (package
    (inherit (python-tree-sitter-grammar tree-sitter-swift))
    (native-inputs (list node-lts tree-sitter-cli python-setuptools))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'generate-parser
            (lambda _
              (invoke "tree-sitter" "generate"))))))))

(define-public python-tree-sitter-typescript
  (python-tree-sitter-grammar tree-sitter-typescript))

(define-public python-tree-sitter-verilog
  (python-tree-sitter-grammar tree-sitter-verilog))

(define-public python-tree-sitter-zig
  (python-tree-sitter-grammar tree-sitter-zig))
