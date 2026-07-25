;; python-deps.scm — Python library packages needed for MCP servers
;;
;; Provides the dependency closure for plane-mcp-server (see mcp.scm):
;; fastmcp and its tree (cyclopts, py-key-value-aio, jsonref, ...), the
;; Plane SDK, plus newer versions of python-mcp / python-pyjwt /
;; python-authlib than Guix proper carries.
(define-module (peteches packages python-deps)
  ;; Guix core
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)  ;pypi-uri
  #:use-module (guix build-system pyproject)
  #:use-module (guix utils)

  ;; Deps
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xdisorg)

  ;; Licensing
  #:use-module ((guix licenses)
                #:prefix license:))

;; --- Small leaf libraries ----------------------------------------------------

(define-public python-jsonref
  (package
    (name "python-jsonref")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jsonref" version))
       (sha256
        (base32 "0lm5rqg4xpnvd8bl6m79z5h2pclhapw0m5ffxgxxw3xghlfqxzij"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Upstream uses the legacy pdm-pep517 backend; pdm-backend
          ;; provides the same functionality under a new module name.
          (add-after 'unpack 'use-pdm-backend
            (lambda _
              (substitute* "pyproject.toml"
                (("build-backend = \"pdm\\.pep517\\.api\"")
                 "build-backend = \"pdm.backend\"")))))))
    (native-inputs (list python-pdm-backend))
    (home-page "https://github.com/gazpachoking/jsonref")
    (synopsis "JSON References for Python")
    (description
     "This package provides automatic dereferencing of JSON Reference
objects (@code{$ref}) in Python JSON documents.")
    (license license:expat)))

(define-public python-openapi-pydantic
  (package
    (name "python-openapi-pydantic")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "openapi_pydantic" version))
       (sha256
        (base32 "03gf0lqdq68ab7qv4lbfziabfjc75fwkpf9yp6glaynydfpkas7z"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-poetry-core))
    (propagated-inputs (list python-pydantic))
    (home-page "https://github.com/mike-oakley/openapi-pydantic")
    (synopsis "Pydantic models for the OpenAPI specification")
    (description
     "Pydantic models for OpenAPI 3.0 and 3.1 documents, used to parse,
validate, and generate OpenAPI schemas in Python.")
    (license license:expat)))

(define-public python-rich-rst
  (package
    (name "python-rich-rst")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rich_rst" version))
       (sha256
        (base32 "1gckip6zbbjh5vd9yyrlif4a8z8rynn5ryjrjxv3iwwpjss1glgl"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-docutils python-rich))
    (home-page "https://github.com/wasi-master/rich-rst")
    (synopsis "Render reStructuredText with rich")
    (description
     "A beautiful reStructuredText renderer built on top of the rich
terminal formatting library.")
    (license license:expat)))

(define-public python-uncalled-for
  (package
    (name "python-uncalled-for")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "uncalled_for" version))
       (sha256
        (base32 "18mqbkjmyp2jnvp6bi8sbmwyrqncwq1a67qb0dyg9f72f76xpxc9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; hatch-vcs needs a git checkout; the sdist has none.
          (add-after 'unpack 'pretend-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-hatchling python-hatch-vcs python-setuptools-scm))
    (home-page "https://github.com/chrisguidry/uncalled-for")
    (synopsis "Async dependency injection for Python functions")
    (description
     "Dependency injection for async Python functions, used by fastmcp.")
    (license license:expat)))

(define-public python-cyclopts
  (package
    (name "python-cyclopts")
    (version "4.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cyclopts" version))
       (sha256
        (base32 "1bldb3xsda3ih0sgmimgvmh94ij8328pr4q2jajspvf7kjx6916v"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; hatch-vcs needs a git checkout; the sdist has none.
          (add-after 'unpack 'pretend-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-hatchling python-hatch-vcs python-setuptools-scm))
    (propagated-inputs
     (list python-attrs python-docstring-parser python-rich python-rich-rst))
    (home-page "https://github.com/BrianPugh/cyclopts")
    (synopsis "Intuitive CLIs based on Python type hints")
    (description
     "Cyclopts builds command-line interfaces from Python type hints and
docstrings, similar to Typer but with broader type support.")
    (license license:asl2.0)))

(define-public python-caio
  (package
    (name "python-caio")
    ;; aiofile pins caio~=0.9.0.
    (version "0.9.25")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "caio" version))
       (sha256
        (base32 "047d2dqhcn7mcz9mn2kf6y8mdqjzwr02agxdq2jgbl6ih5zqwj8n"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/mosquito/caio")
    (synopsis "Asynchronous file IO for Linux with POSIX fallback")
    (description
     "Low-level asynchronous file access library with linux libaio and
POSIX AIO implementations plus a pure-Python thread-based fallback.")
    (license license:asl2.0)))

(define-public python-aiofile
  (package
    (name "python-aiofile")
    (version "3.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiofile" version))
       (sha256
        (base32 "1yacbxsz9f0icjddjdlajk76ff5zy0a3bbm4kkja9lj3cqn9348z"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-hatchling))
    (propagated-inputs (list python-caio))
    (home-page "https://github.com/mosquito/aiofile")
    (synopsis "Real asynchronous file operations for asyncio")
    (description
     "File operations for asyncio using caio, avoiding thread-pool based
emulation where the kernel supports asynchronous IO.")
    (license license:asl2.0)))

(define-public python-py-key-value-aio
  (package
    (name "python-py-key-value-aio")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "py_key_value_aio" version))
       (sha256
        (base32 "1ms5xag0l56g8szsskx58hi5lhlv59f8g7jg1ziaapdyd8n3lmn6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Guix has no uv_build backend; hatchling handles the same
          ;; src/ layout once told where the package lives.
          (add-after 'unpack 'use-hatchling
            (lambda _
              (substitute* "pyproject.toml"
                (("requires = \\[\"uv_build[^]]*\\]")
                 "requires = [\"hatchling\"]")
                (("build-backend = \"uv_build\"")
                 "build-backend = \"hatchling.build\""))
              (let ((port (open-file "pyproject.toml" "a")))
                (display "\n[tool.hatch.build.targets.wheel]\n" port)
                (display "packages = [\"src/key_value\"]\n" port)
                (close-port port)))))))
    (native-inputs (list python-hatchling))
    ;; aiofile/anyio, cachetools, and redis cover the filetree, memory,
    ;; and redis extras used by fastmcp and plane-mcp-server.  The
    ;; keyring extra is deliberately left out: Guix's keyring is older
    ;; than the extra's pin and the store is only imported when used.
    (propagated-inputs
     (list python-aiofile
           python-anyio
           python-beartype
           python-cachetools
           python-redis
           python-typing-extensions))
    (home-page "https://github.com/strawgate/py-key-value")
    (synopsis "Async pluggable key-value store interface")
    (description
     "An async key-value store abstraction with pluggable backends
(memory, disk, Redis, and more) used by fastmcp for token and state
storage.")
    (license license:asl2.0)))

(define-public python-exceptiongroup
  ;; Removed from Guix proper (Python >= 3.11 has ExceptionGroup built
  ;; in), but fastmcp uses the backport's `catch' helper unconditionally.
  (package
    (name "python-exceptiongroup")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "exceptiongroup" version))
       (sha256
        (base32 "069j9qfgjha1qgvsh3zpaikyslikx45004632iyhnnq5qqr28hcb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Guix has no flit_scm backend; the sdist already contains the
          ;; generated _version.py, so plain flit_core suffices.
          (add-after 'unpack 'use-flit-core
            (lambda _
              (substitute* "pyproject.toml"
                (("requires = \\[\"flit_scm\"\\]")
                 "requires = [\"flit_core\"]")
                (("build-backend = \"flit_scm:buildapi\"")
                 "build-backend = \"flit_core.buildapi\"")
                (("dynamic = \\[\"version\"\\]")
                 (string-append "version = \"" #$version "\""))))))))
    (native-inputs (list python-flit-core))
    (propagated-inputs (list python-typing-extensions))
    (home-page "https://github.com/agronholm/exceptiongroup")
    (synopsis "Backport of PEP 654 exception groups")
    (description
     "Backport of the Python 3.11 exception group machinery, including
the @code{catch} context manager that fastmcp relies on.")
    (license license:expat)))

;; --- Newer versions of packages Guix already carries -------------------------

(define-public python-pyjwt-2.13
  (package
    (inherit python-pyjwt)
    (name "python-pyjwt")
    (version "2.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyjwt" version))
       (sha256
        (base32 "08rlf2vz3fkr91wkgxnnpc7q3m37fg8a467gx1wqqncira4iqms1"))))
    (arguments (list #:tests? #f))))

(define-public python-joserfc-1.7
  ;; Guix carries joserfc 1.0.1; authlib >= 1.7 needs >= 1.6.
  (package
    (name "python-joserfc")
    (version "1.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "joserfc" version))
       (sha256
        (base32 "1kyg3n02xvzb8cyg9rwz1jbglmcjzkbhpz910sidnfbib315as8i"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; The >=45 pin is newer than Guix's cryptography, but joserfc
          ;; only uses primitives that long predate it.
          (add-after 'unpack 'relax-cryptography-pin
            (lambda _
              (substitute* "pyproject.toml"
                (("cryptography>=45\\.0\\.1") "cryptography>=44")))))))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-cryptography))
    (home-page "https://github.com/authlib/joserfc")
    (synopsis "Implementations of JOSE RFCs in Python")
    (description
     "Python implementation of JWS, JWE, JWK, and JWT (RFC 7515-7519 and
related specifications), maintained by the Authlib project.")
    (license license:bsd-3)))

(define-public python-authlib-1.7
  (package
    (inherit python-authlib)
    (name "python-authlib")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "authlib" version))
       (sha256
        (base32 "0ca2qqkbhr4idb5mvkd84d5h7j35qapw0whkvwxigrylzkz2bsic"))))
    (arguments (list #:tests? #f))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-authlib)
       (append python-joserfc-1.7)))))

(define-public python-mcp-1.26
  (package
    (inherit python-mcp)
    (name "python-mcp")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mcp" version))
       (sha256
        (base32 "0ric2hvi276sjn3x9bwkkychabpcipr7c6kijc6imk7fj7s2wvnv"))))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Guix has no uv-dynamic-versioning hatch plugin; pin the
          ;; version statically instead.
          (add-after 'unpack 'static-version
            (lambda _
              (substitute* "pyproject.toml"
                (("dynamic = \\[\"version\"\\]")
                 (string-append "version = \"" #$version "\""))
                (("requires = \\[\"hatchling\", \"uv-dynamic-versioning\"\\]")
                 "requires = [\"hatchling\"]")
                (("\\[tool\\.hatch\\.version\\]") "")
                (("source = \"uv-dynamic-versioning\"") "")))))))
    ;; Unlike Guix's python-mcp, propagate the actual runtime closure so
    ;; dependents work outside the build container.
    (propagated-inputs
     (list python-anyio
           python-cryptography
           python-httpx
           python-httpx-sse
           python-jsonschema
           python-multipart
           python-pydantic
           python-pydantic-settings
           python-pyjwt-2.13
           python-sse-starlette
           python-starlette
           python-typing-extensions
           python-typing-inspection
           python-uvicorn))
    (native-inputs (list python-hatchling))))

;; --- fastmcp and the Plane stack ---------------------------------------------

(define-public python-fastmcp
  (package
    (name "python-fastmcp")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fastmcp" version))
       (sha256
        (base32 "1vrvj3h0846c73ga0h8fj421zx2cj2cg7h3dqzcx74imzj7hp0yl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'static-version-and-relax-pins
            (lambda _
              (substitute* "pyproject.toml"
                ;; No uv-dynamic-versioning plugin in Guix.
                (("dynamic = \\[\"version\"\\]")
                 (string-append "version = \"" #$version "\""))
                (("requires = \\[\"hatchling\", \"uv-dynamic-versioning[^]]*\\]")
                 "requires = [\"hatchling\"]")
                (("\\[tool\\.hatch\\.version\\]") "")
                (("source = \"uv-dynamic-versioning\"") "")
                ;; Guix versions are slightly behind these pins but
                ;; provide the APIs fastmcp uses.
                (("uvicorn>=0.35") "uvicorn>=0.34")
                (("websockets>=15.0.1") "websockets>=13")
                (("jsonschema-path>=0.3.4") "jsonschema-path>=0.3.2")
                ;; Guix's keyring predates the keyring extra's pin; the
                ;; keyring store is only imported when actually used
                ;; (CLI OAuth token storage).
                (("py-key-value-aio\\[filetree,keyring,memory\\]")
                 "py-key-value-aio[filetree,memory]")))))))
    (native-inputs (list python-hatchling))
    (propagated-inputs
     (list python-authlib-1.7
           python-cyclopts
           python-dotenv
           python-email-validator ;pydantic[email]
           python-exceptiongroup
           python-httpx
           python-jsonref
           python-jsonschema-path
           python-mcp-1.26
           python-openapi-pydantic
           python-opentelemetry-api
           python-packaging
           python-platformdirs
           python-py-key-value-aio
           python-pydantic
           python-pyperclip
           python-pyyaml
           python-rich
           python-uncalled-for
           python-uvicorn
           python-watchfiles
           python-websockets))
    (home-page "https://gofastmcp.com")
    (synopsis "Pythonic framework for building MCP servers and clients")
    (description
     "FastMCP is a framework for building Model Context Protocol servers
and clients in Python, with decorator-based tools, resources, prompts,
authentication, and OpenAPI integration.")
    (license license:asl2.0)))

(define-public python-plane-sdk
  (package
    (name "python-plane-sdk")
    (version "0.2.19")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "plane_sdk" version))
       (sha256
        (base32 "09a2ziiq1sdjgh4kmqiinxqlx6bzprn4hsd8w6ngvxx0q5bvg2vk"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-pydantic python-requests))
    (home-page "https://github.com/makeplane/plane-python-sdk")
    (synopsis "Python SDK for the Plane API")
    (description
     "Official Python client library for the Plane project management
platform's REST API.")
    (license license:expat)))
