;; peteches/packages/ovms.scm — Intel OpenVINO Model Server, from the upstream
;; prebuilt Ubuntu 24.04 tarball.
;;
;; WHAT IT IS AND WHY WE WANT IT.  OVMS is Intel's own production inference
;; server for OpenVINO IR models, and since 2026.2/2026.3 it exposes an
;; OpenAI-compatible HTTP API (`/v3/chat/completions`).  It is model-agnostic:
;; it ships native, *streaming* tool and reasoning parsers for a dozen families
;; and picks the right ones from each model's chat template.  In OVMS 2026.4.1
;; the accepted values are `llama3, hermes3, phi4, mistral, gptoss, qwen3coder,
;; devstral, lfm2, gemma4, onyx, minicpm5` for --tool_parser and `qwen3,
;; gemma4, gptoss, onyx, minicpm5, lfm2, devstral` for --reasoning_parser
;; (obtained by passing each flag a deliberately invalid value and reading the
;; error; both lists are also recorded next to the fields in
;; peteches/services/ovms.scm).
;;
;; THE FLEET CURRENTLY SERVES Qwen3.6-35B-A3B INT4 through it on comfyui's Arc
;; Pro B70, for which OVMS auto-detects `qwen3coder` + `qwen3`.  Before that it
;; served an LFM2.5 export, which is where the `lfm2` parser name below comes
;; from — and that name is a trap worth keeping on the record: `lfm2` is the
;; ONLY LFM-family value OVMS 2026.4.1 accepts, for BOTH flags, even though the
;; upstream source file for the reasoning half is named
;; `src/llm/io_processing/lfm2/lfm25_reasoning_parser.hpp`.  Passing
;; `--reasoning_parser lfm25` makes the server refuse to start.
;;
;; HISTORY: this package was originally justified as the path to deleting
;; `peteches/services/openvino-server.py`, a ~1095-line hand-written OpenAI shim
;; over the openvino-genai Python bindings that hand-implemented LFM2's
;; `<|tool_call_start|>[fn(arg='v')]<|tool_call_end|>` tool-call markup and its
;; `<think>…</think>` blocks, and that could not stream a tool-bearing turn
;; at all (it had to buffer the whole completion before it could tell markup
;; from text).  OVMS's parsers do the same job in C++, with tests, and
;; incrementally.  That shim, its service type and its test probe were deleted
;; on 2026-11-26 when LFM2.5 was retired in favour of Qwen3.6 — the packaging
;; argument turned out to be the smaller half of the win.
;;
;; WHY THE PREBUILT TARBALL AND NOT A SOURCE BUILD.  OVMS only builds through
;; Docker/Podman plus Bazel — `docs/build_from_source.md` says so outright, and
;; Bazel runs *inside* the build container, not on the host.  Guix packages
;; neither `bazel` nor `drogon` (the HTTP frontend OVMS links statically), so
;; there is no route to a `cmake-build-system` package here.  This is the same
;; compromise `peteches/packages/koboldcpp.scm` already makes for the same
;; reason, and it is why this module is a `copy-build-system` + `patchelf`
;; package: unpack a vendor blob, fix up the ELF headers, ship it.
;;
;; IT BUNDLES ITS OWN OPENVINO.  The tarball carries `libopenvino.so.2026.4.1`,
;; `libopenvino_genai.so.2026.4.1.0`, `libopenvino_tokenizers.so`, the CPU/GPU/
;; NPU/AUTO/HETERO plugins, every model frontend, opencv 4.14, TBB, espeak-ng,
;; azure-storage, libgit2 and the OpenCL ICD *loader* in `lib/`, plus the
;; matching CPython packages in `lib/python/`.  It therefore deliberately does
;; **not** reuse this repo's from-source `(peteches packages openvino)` — the
;; versions would have to be pinned in lockstep forever and a rebuild of ours
;; could silently break the server.  Upstream is explicit about it: "Do not
;; install openvino, openvino-tokenizers, or openvino-genai via pip for OVMS
;; usage.  These packages are already included."  The cost is ~507 MB extracted
;; and a second OpenVINO on any box that also has ours; the benefit is that
;; nothing about this package is coupled to our own OpenVINO build.
;;
;; AVX2 IS REQUIRED AT RUNTIME.  The bundled oneDNN CPU plugin is compiled
;; with an AVX2 floor, so the binary `SIGILL`s (exit 132) on a CPU without it.
;; The dev VM this package was built on is a `QEMU Virtual CPU version 2.5+`
;; with sse4_2 and *no* AVX at all, which is why it cannot be smoke-tested
;; there — building it is fine (that only runs patchelf), *running* it is not.
;; Functional testing has to happen on comfyui (i9-14900KS: AVX2 yes, AVX-512
;; no), which is also the intended deployment host.
;;
;; LIBXML2: A SONAME MISMATCH, NOT A MISSING DEPENDENCY.  `bin/ovms` carries a
;; `DT_NEEDED` on `libxml2.so.2`, but Guix ships libxml2 2.14.6 whose soname is
;; `libxml2.so.16` — there is no `libxml2.so.2` to point a RUNPATH at.  It is
;; also vestigial on the executable itself: `bin/ovms` has 1904 undefined
;; dynamic symbols (1889 strong `U`, 15 weak `w`) and *zero* of them match
;; `/xml/`, so nothing in it ever calls into libxml2.  The only bundled
;; consumer is `libazure-storage-common.so.12.14.0` (the Azure blob-storage
;; model-source backend, which this deployment never uses).  So `install`
;; drops a `$out/lib/libxml2.so.2` symlink at Guix's `libxml2.so.16`.  Do NOT
;; count on the loader to flag an ABI mismatch here — that is not what it
;; does: Guix's `libxml2.so.16` carries VERNEED but no VERDEF, and
;; `ld.so --list` on `libazure-storage-common.so.12.14.0` exits 0 emitting
;; only `no version information available` and binds unversioned, silently.
;; The real guard is the `nm -D` sweep documented below: all 25 `xml*`
;; symbols that object needs do resolve against libxml2 2.14.6 (verified),
;; and re-running that sweep is what would catch a future OVMS release
;; calling libxml2 symbols Guix's build does not provide.  It is the one
;; symlink *we create* in this package — the vendor tree ships its own
;; (including e.g. `lib/libxml2.so.16 -> libxml2.so.2` upstream of us), and
;; everything else is resolved through RUNPATH.
;;
;; THE WHOLE TREE GETS PATCHED, NOT JUST bin/ovms.  This is the part that is
;; easy to get wrong.  glibc's `DT_RUNPATH` is *not* transitive: it is consulted
;; only for the direct `DT_NEEDED` entries of the object that carries it, never
;; for the dependencies of those dependencies (`DT_RPATH` was transitive;
;; `DT_RUNPATH` deliberately is not).  The bundled libraries therefore have to
;; find their own external dependencies themselves, and several of them cannot:
;; `libopenvino_genai.so.2026.4.1.0` — the first thing `bin/ovms` links, and the
;; whole point of the exercise — has **no RUNPATH at all** and `DT_NEEDED`s
;; `libtbb.so.12`, which is bundled two inches away in the same directory.
;; Verified empirically: patching only `bin/ovms` makes
;; `ld-linux-x86-64.so.2 --list bin/ovms` die with
;;   libtbb.so.12: cannot open shared object file
;; while patching every ELF object in the tree makes the same command resolve
;; cleanly (exit 0, all 51 shared objects found).  So `patchelf-fix` sets the
;; same RUNPATH on `bin/ovms` *and* on all 69 bundled ELF objects under
;; `$out/lib`.  It is cheap — the phase takes about a second for the whole
;; 507 MB tree — because patchelf only rewrites headers.
;;
;; RUNPATH ORDER: `$out/lib` first, so the bundled OpenVINO always wins over
;; anything else on the box (this is why the package does not collide with
;; `(peteches packages openvino)` even when both are in the same profile), then
;; glibc, gcc's `lib` output, python, curl, openssl, util-linux's `lib` output,
;; zlib and libxml2.  zlib is not on `bin/ovms`'s own `DT_NEEDED` list and is
;; easy to miss: it is needed transitively by `libgit2.so`, `libopencv_core.so`
;; and `libopencv_imgcodecs.so`, and because RUNPATH is not transitive those
;; three have to reach `libz.so.1` from their *own* search path.
;;
;; WHAT IS DELIBERATELY LEFT UNRESOLVED.  Two `DT_NEEDED` entries in the tree
;; cannot be satisfied and are not worth satisfying:
;;   * `libopenvino_jax_frontend.so.2641`, referenced by the four
;;     `lib/python/openvino/frontend/jax/py_jax_frontend.cpython-3*.so`
;;     extension modules — upstream simply does not ship that library in the
;;     tarball, so those modules are dead on arrival in the vendor image too.
;;     We serve IR models; nothing imports them.
;;   * `libze_loader.so.1` (`libopenvino_intel_npu_vm_runtime.so`) and
;;     `libzstd.so.1` (`libopenvino_intel_npu_compiler.so`) — the Level Zero
;;     NPU stack.  OpenVINO `dlopen`s these lazily and tolerates failure, and
;;     no host in this fleet has an NPU.
;; This is why `#:validate-runpath?` is `#f`.  Guix's validator walks every ELF
;; object under `$out/lib` and `$out/bin` and hard-fails on a `DT_NEEDED` entry
;; missing from that object's RUNPATH, so with those two unfixable names in the
;; tree it can never pass.  Disabling it hides nothing we care about, because
;; the closure that does matter was verified on the built output instead:
;; `ld-linux-x86-64.so.2 --list $out/bin/ovms` exits 0 with all 51 shared
;; objects resolved and zero "not found" — the DT_NEEDED/RUNPATH closure of
;; `bin/ovms` is those 51 objects plus `ld-linux-x86-64.so.2` itself, 52
;; names.  An `nm -D` sweep over exactly that 51-object closure finds zero
;; unresolved *strong* undefined symbols; the 53 that do stay unresolved are
;; all weak, so they bind to NULL instead of failing the load (which matters
;; — these objects are BIND_NOW).  The weak families actually present in the
;; swept closure: OpenSSL-1.0-era compat aliases such as `EVP_MD_CTX_create`,
;; libitm `_ITM_*`, TBB server-mode `__TBB_*`, TLS-init guards
;; `_ZGTt*`/`_ZTHN*`, coverage hooks `__gcov_*` and `__gmon_start__`, TBB's
;; `scalable_*`/`tcm*`, `ZSTD_trace_*` and `git_lfs_shutdown_requested`.
;; NOTE what the sweep did NOT cover, deliberately: none of the 52 closure
;; names is under `lib/python`, so the `cpython-313` extension variants sit
;; outside it.  Those modules carry unresolved *strong* `Py*` symbols
;; (`py_openvino_genai.cpython-313-x86_64-linux-gnu.so` alone has 169, with
;; no DT_NEEDED on any libpython).  That is irrelevant rather than alarming
;; — the bundled interpreter is CPython 3.12 and never loads them — but had
;; they been swept, "zero unresolved strong" would have been false.  Do not
;; read that claim as covering `$out/lib/python`.
;;
;; LICENSES.  OVMS itself is Apache-2.0, which in Guix is `asl2.0` (there is no
;; `license:apache2.0` — see the same note in `peteches/packages/openvino.scm`).
;; `share/doc/ovms/thirdparty-licenses/` carries 76 files for the bundled
;; dependencies; two of them make the aggregate more than permissive and are
;; worth knowing about before this is ever redistributed: libgit2 is GPL-2
;; *with a linking exception* (so linking `libgit2.so` into an Apache-2.0
;; server is fine), and ittnotify is dual BSD-3/GPL-2.
;;
;; PYTHON_ON IS REQUIRED, NOT OPTIONAL.  Upstream also publishes a
;; `_python_off` tarball; it cannot serve our models.  OVMS renders chat
;; templates either through its C++ minja engine or through Python's jinja2,
;; and the C++ engine does not implement every template — upstream lists
;; exactly three exceptions: `lfm2`, miniCPMv5 and Qwen3-Coder —
;; and OVMS's own docs state "using `tools` is not supported in configuration
;; without Python".  Two of those three names are directly relevant here: the
;; retired LFM2.5 export needed `lfm2`, and `qwen3coder` is the tool parser
;; OVMS auto-detects for the Qwen3.6 model the fleet serves now.  So
;; `python_off` would lose both the tool-call parser and the chat template,
;; which is the entire reason for packaging this.  The
;; tarball is pinned to `ovms_ubuntu24_<version>_python_on.tar.gz` for that
;; reason, and `%ovms-version` is exported so the service module and the docs
;; can reference one pin instead of restating the string.
;;
;; `lib/python/` IS NOT A SITE-PACKAGES DIRECTORY.  The bundled CPython
;; packages (`openvino/`, `openvino_genai/`, `openvino_tokenizers/`,
;; `pyovms.so`) live at `$out/lib/python`, which no Guix profile machinery will
;; ever look at, and this package deliberately does not try to register them.
;; Whatever runs ovms has to put that directory on `PYTHONPATH` itself — the
;; same arrangement the deleted `peteches/services/openvino.scm` documented for
;; OpenVINO's own Python bindings, which install to `<out>/python` and had to be
;; named explicitly in `PYTHONPATH` because a profile merge never looks there
;; either.  `(peteches packages openvino)` itself is still in the tree but is
;; currently imported by no system config.  The tree
;; is left exactly as upstream shipped it.

(define-module (peteches packages ovms)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)          ;glibc
  #:use-module (gnu packages compression)   ;zlib
  #:use-module (gnu packages curl)          ;curl
  #:use-module (gnu packages elf)           ;patchelf
  #:use-module (gnu packages gcc)           ;gcc (for the lib output: libstdc++)
  #:use-module (gnu packages linux)         ;util-linux (for the lib output: libuuid)
  #:use-module (gnu packages python)        ;python
  #:use-module (gnu packages tls)           ;openssl
  #:use-module (gnu packages xml))          ;libxml2
;; NOTE: deliberately NO `#:export` clause.  `ovms` and `%ovms-version` below are
;; both `define-public`, which already calls `module-export!` on them; listing
;; them in `#:export` as well declares each name twice.  That is not an error
;; (module-export! is idempotent), but it means two places to keep in step and a
;; reader has to check both to know what the module's public surface is.  One
;; mechanism per module: here it is `define-public`, matching how
;; peteches/packages/koboldcpp.scm does it.

;; Single source of truth for the pin, exported so that
;; (peteches services ovms) — which splices it into ovms-service-type's
;; description — and the docs do not restate the version.  Mirrors
;; %openvino-version in peteches/packages/openvino.scm.
(define-public %ovms-version "2026.4.1")

(define-public ovms
  (package
    (name "ovms")
    (version %ovms-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/openvinotoolkit/model_server/releases/"
             "download/v" version
             "/ovms_ubuntu24_" version "_python_on.tar.gz"))
       ;; Naming the file after the upstream asset keeps the store item
       ;; identical to what `guix download' of that URL produces.
       (file-name (string-append "ovms_ubuntu24_" version
                                 "_python_on.tar.gz"))
       (sha256
        (base32 "1vnm7q6xh9i8zrmj1nh1zb7slxmh75lnl6h3s5f3dz09s0lly44j"))))
    (build-system copy-build-system)
    (arguments
     (list
      ;; See the header: two DT_NEEDED names in the vendor tree
      ;; (libopenvino_jax_frontend.so.2641, libze_loader.so.1) cannot be
      ;; satisfied at all, and Guix's validator walks every ELF object under
      ;; $out/lib, so it can never pass.  The object that actually runs is
      ;; checked explicitly instead.
      #:validate-runpath? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Prebuilt, already-stripped vendor binaries: `strip' on a 507 MB
          ;; tree is pure downside (slow, and it can upset the plugins that
          ;; are dlopened by soname).
          (delete 'strip)

          ;; Licenses are installed explicitly below, into a directory we
          ;; choose; the generic phase would guess a source directory
          ;; relative to `..' and drop them somewhere else.
          (delete 'install-license-files)

          ;; KEPT: unlike koboldcpp (a raw single binary) this source is a
          ;; .tar.gz, so `unpack' runs.  Note that Guix's `unpack' ends with
          ;; `(and=> (first-subdirectory ".") chdir)' — it has already chdir'd
          ;; into the archive's single top-level `ovms/', so the tree to copy
          ;; is `.', not `ovms/'.  (Confirmed against the failed build log:
          ;; `patch-source-shebangs' reported `./bin/convert_tokenizer'.)  The
          ;; fallback keeps this working if that ever changes.
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (use-modules (guix build utils))
              (let* ((out (assoc-ref outputs "out"))
                     (doc (string-append out "/share/doc/ovms"))
                     ;; `file-is-directory?' raises on ENOENT rather than
                     ;; returning #f, so guard it: after `unpack''s chdir the
                     ;; normal case is that there is no `ovms' subdirectory at
                     ;; all and the tree to copy is `.'.
                     (src (if (false-if-exception
                               (file-is-directory? "ovms"))
                              "ovms" ".")))
                ;; Lift the *contents* of the unpacked tree to $out, so the
                ;; result is $out/bin, $out/lib, $out/include, $out/share and
                ;; NOT a stray $out/ovms/ nesting level.  copy-recursively
                ;; copies a directory's contents into the destination and
                ;; preserves symlinks (53 vendor symlinks under $out/lib —
                ;; the .so -> .so.N chains the loader depends on; 54 once
                ;; the libxml2.so.2 shim added below is counted) instead of
                ;; following them.
                (copy-recursively src out
                                  #:log (%make-void-port "w")
                                  #:keep-mtime? #t
                                  #:keep-permissions? #t)
                ;; copy-recursively only *reports* i/o errors and returns #f,
                ;; so check the one file that must be there rather than
                ;; letting a silently empty $out reach patchelf.
                (unless (file-exists? (string-append out "/bin/ovms"))
                  (error "install: ~a/bin/ovms is missing — copying from ~s \
did not produce the expected tree" out src))
                ;; The tarball is read-only in places — bin/ovms and five
                ;; others are mode 555 — and patchelf opens its target for
                ;; writing, failing with "open: Permission denied" otherwise.
                ;; Must happen before `patchelf-fix'.
                (invoke "chmod" "-R" "u+w" out)
                (mkdir-p doc)
                (for-each (lambda (entry)
                            (rename-file (string-append out "/" entry)
                                         (string-append doc "/" entry)))
                          '("LICENSE" "metadata.json" "thirdparty-licenses")))))

          (add-after 'install 'patchelf-fix
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (guix build utils))
              (let* ((out       (assoc-ref outputs "out"))
                     (libdir    (string-append out "/lib"))
                     (patchelf  (search-input-file inputs "/bin/patchelf"))
                     (glibc     (assoc-ref inputs "libc"))
                     (interp    (string-append glibc
                                               "/lib/ld-linux-x86-64.so.2"))
                     (libxml2   (assoc-ref inputs "libxml2"))
                     (rpath     (string-join
                                 (list
                                  ;; $out/lib FIRST: the bundled OpenVINO has
                                  ;; to win over any other on the box, and this
                                  ;; also preserves the $ORIGIN/../lib/ intent
                                  ;; the vendor RUNPATH had.
                                  libdir
                                  (string-append glibc "/lib")
                                  (string-append (assoc-ref inputs "gcc:lib")
                                                 "/lib")
                                  (string-append (assoc-ref inputs "python")
                                                 "/lib")
                                  (string-append (assoc-ref inputs "curl")
                                                 "/lib")
                                  (string-append (assoc-ref inputs "openssl")
                                                 "/lib")
                                  (string-append (assoc-ref
                                                  inputs "util-linux:lib")
                                                 "/lib")
                                  (string-append (assoc-ref inputs "zlib")
                                                 "/lib")
                                  (string-append libxml2 "/lib"))
                                 ":")))
                ;; libxml2 soname shim — see the header.  Resolved by glob
                ;; rather than hardcoded to .so.16 so that a libxml2 bump
                ;; re-points it instead of silently dangling.
                ;; (Deliberately no `match' here — a plain let/if reads fine
                ;; for a one-value-or-error case.  NOT because `match' would
                ;; be unavailable: `(ice-9 match)' is in scope in a phase
                ;; body, since (guix build utils) imports it and Guile's
                ;; transformer lookup walks the use chain.  This file
                ;; already relies on another macro from outside the build
                ;; modules — boot-9's `false-if-exception', used in the
                ;; `install' phase above — and builds fine.)
                (let ((found (find-files (string-append libxml2 "/lib")
                                         "^libxml2\\.so\\.[0-9]+$")))
                  (if (= 1 (length found))
                      (symlink (car found)
                               (string-append libdir "/libxml2.so.2"))
                      (error "expected exactly one versioned libxml2 in ~a, \
found ~s" libxml2 found)))

                ;; The entry point: interpreter into glibc, RUNPATH as above.
                (invoke patchelf
                        "--set-interpreter" interp
                        "--set-rpath" rpath
                        (string-append out "/bin/ovms"))

                ;; Every bundled ELF object too.  DT_RUNPATH is not
                ;; transitive, and libopenvino_genai.so.2026.4.1.0 has no
                ;; RUNPATH at all while needing libtbb.so.12 — patching only
                ;; bin/ovms leaves `ld.so --list' failing on exactly that.
                (let ((objects (find-files libdir
                                           (lambda (file stat)
                                             (and (eq? 'regular
                                                       (stat:type stat))
                                                  (elf-file? file)))
                                           #:stat lstat)))
                  (format #t "setting RUNPATH on ~a bundled ELF objects~%"
                          (length objects))
                  (for-each (lambda (object)
                              (invoke patchelf "--set-rpath" rpath object))
                            objects))

                ;; The vendor image leaves exactly one DANGLING symlink
                ;; behind: lib/python/openvino_tokenizers/lib/
                ;; libopenvino_tokenizers.so points at the absolute
                ;; Docker-build path /ovms/lib/libopenvino_tokenizers.so,
                ;; which does not exist here.  The real library is
                ;; $out/lib/libopenvino_tokenizers.so (a regular ~3.8 MB
                ;; file), so re-point the link at it — openvino_tokenizers
                ;; is exactly what the `python_on' choice rests on, and this
                ;; keeps the output free of dangling links.  readlink
                ;; succeeds only if the path is a symlink (it throws on a
                ;; regular file or ENOENT), and the file-exists? guard stops
                ;; us ever creating a NEW dangling link if the vendor layout
                ;; changes under us.
                (let* ((link (string-append
                              out
                              "/lib/python/openvino_tokenizers/lib/"
                              "libopenvino_tokenizers.so"))
                       (target (string-append libdir
                                              "/libopenvino_tokenizers.so")))
                  (when (and (false-if-exception (readlink link))
                             (file-exists? target))
                    (switch-symlinks link target)))

                ;; bin/convert_tokenizer has `#!/usr/bin/python3', which does
                ;; not exist on Guix.  CHOSEN: an explicit `patch-shebang'
                ;; against the store python's bin directory rather than
                ;; relying on $PATH — it is deterministic and does not depend
                ;; on which input happens to come first.  (The standard
                ;; `patch-shebangs' phase still runs afterwards and is a no-op
                ;; by then: it resolves python3 to the same store path.)
                (patch-shebang (string-append out "/bin/convert_tokenizer")
                               (list (string-append (assoc-ref inputs "python")
                                                    "/bin")))))))))
    (native-inputs (list patchelf))
    ;; Labelled pairs read back with assoc-ref, as in koboldcpp.scm.  The two
    ;; multi-output packages need their `lib' output named explicitly:
    ;; util-linux's default output does not contain libuuid.so.1, and gcc's
    ;; default output is not where libstdc++.so.6 lives.
    (inputs (list (list "libc" glibc)
                  (list "gcc:lib" gcc "lib")
                  (list "python" python)
                  (list "curl" curl)
                  (list "openssl" openssl)
                  (list "util-linux:lib" util-linux "lib")
                  (list "zlib" zlib)
                  (list "libxml2" libxml2)))
    ;; The vendor tarball is x86_64 ELF only, and the CPU plugin needs AVX2 at
    ;; runtime (see the header).
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/openvinotoolkit/model_server")
    (synopsis "OpenVINO Model Server — Intel's OpenAI-compatible inference server")
    (description
     "OpenVINO Model Server (OVMS) is Intel's production inference server for
OpenVINO IR models, exposing gRPC and an OpenAI-compatible HTTP API.  It is
model-agnostic and ships native, streaming tool-call and reasoning parsers for
a dozen model families — @code{llama3}, @code{hermes3}, @code{phi4},
@code{mistral}, @code{gptoss}, @code{qwen3coder}, @code{devstral}, @code{lfm2},
@code{gemma4}, @code{onyx} and @code{minicpm5} for tool calls, and
@code{qwen3}, @code{gemma4}, @code{gptoss}, @code{onyx}, @code{minicpm5},
@code{lfm2} and @code{devstral} for reasoning — auto-detected from each model's
chat template, so tool-call markup and reasoning blocks are decoded
incrementally rather than buffered.

This package installs the upstream prebuilt Ubuntu 24.04 @code{python_on}
tarball with its ELF interpreter and RUNPATH rewritten for Guix.  It bundles
its own OpenVINO and openvino-genai and does not use a system or Guix-provided
OpenVINO.  Requires an AVX2-capable CPU at runtime.  The bundled CPython
packages live in @file{lib/python}, which is not a site-packages directory:
callers must add it to @code{PYTHONPATH} themselves, alongside a profile that
provides @code{jinja2}, because the C++ template engine does not implement
every chat template.")
    (license license:asl2.0)))
