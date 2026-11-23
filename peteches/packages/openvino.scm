;; peteches/packages/openvino.scm — OpenVINO runtime + GenAI, for the LFM2.5 trial.
;;
;; Intel's OpenVINO inference runtime (`openvino') and its generative-AI
;; front-end library (`openvino-genai'), built from source so they can be used
;; as Guix packages next to strata on comfyui rather than as a pip wheel in some
;; one-off venv.
;;
;; WHY A RECURSIVE GIT FETCH: OpenVINO vendors ~29 dependencies as Git
;; submodules (oneDNN for the CPU plugin, onnx + protobuf-lite for the ONNX
;; front-end that optimum-intel drives when it converts HF weights to IR,
;; pybind11 for the Python bindings, ipf for the AUTO plugin, flatbuffers,
;; pugixml, snappy, xbyak, …).  Upstream publishes no source tarball with the
;; submodules filled in — GitHub's archive puts empty directories there — so the
;; origin is a `git-reference' with (recursive? #t), which makes Guix run
;; `git submodule update --init --recursive' and then delete every `.git' entry,
;; exactly as (guix build git) does.  The fixed-output hash was therefore
;; computed the same way locally (`fetch --depth 1', checkout, recursive
;; submodule init, strip .git, `guix hash -r'): a ~957 MB tree, so this fetch is
;; not cheap and substitutes are worth having for anything else that needs it.
;;
;; GPU ENABLED — LEVEL ZERO, NOT OPENCL.  This build targets the Arc Pro B70
;; (Battlemage, Xe2) on comfyui, and for that card the door is Level Zero, not
;; OpenCL: 2026.4.1's cmake exposes `GPU_RT_TYPE' (OCL | SYCL | ZE, L0 aliased
;; to ZE) and this build sets it to ZE.  That pulls in three things, all of
;; which are already in the recursive submodule fetch above, so nothing new is
;; downloaded and the source sha256 is unchanged:
;;   * the Level Zero loader (thirdparty/level_zero/level-zero — ENABLE_OV_ZERO_LOADER
;;     is forced ON by `ENABLE_INTEL_GPU AND GPU_RT_TYPE STREQUAL ZE', and with
;;     ENABLE_SYSTEM_LEVEL_ZERO left OFF the vendored loader is compiled in and
;;     installed; at runtime it dlopens the driver's libze_intel_gpu.so.1, which
;;     comfyui's system carries from nonguix's intel-level-zero-gpu — the same
;;     driver strata uses);
;;   * the OpenCL ICD loader + headers (thirdparty/ocl/* — the ZE runtime still
;;     links OpenCL::OpenCL "because of the kernel builder and OneDNN" per
;;     src/plugins/intel_gpu/src/runtime/CMakeLists.txt; with
;;     ENABLE_SYSTEM_OPENCL left OFF the vendored Khronos ICD loader is built,
;;     which is exactly what a sandboxed build needs);
;;   * oneDNN with its GPU backend (thirdparty/onednn_gpu, ENABLE_ONEDNN_FOR_GPU
;;     left at its ON default — the GPU plugin is useless without it).
;; The ZE path has its own kernel builder (src/plugins/intel_gpu/src/runtime/
;; ze/ze_kernel_builder.cpp), so kernel compilation does not round-trip through
;; OpenCL.  The NPU plugin stays off (there is no NPU on this box).  The
;; AUTO/HETERO/MULTI/TEMPLATE composite plugins stay on because they are cheap
;; and are what GenAI pipelines expect.  With the GPU plugin compiled in,
;; `ov.Core().available_devices' on comfyui reports the Arc as GPU alongside
;; CPU — which device a pipeline runs on is now an explicit choice at the
;; service layer (peteches/services/openvino.scm), not a property of the
;; package.
;;
;; FRONTENDS: IR (OpenVINO's own format — what optimum-cli exports) and ONNX are
;; enabled; TensorFlow, PyTorch, JAX, Paddle and TFLite are off.  They are not
;; needed for this trial and each drags in more bundled third-party code.
;;
;; INSTALL LAYOUT: we asked for a GNU layout and did not get one.  OpenVINO's install
;; destinations come from OV_CPACK_* cache variables that its packaging macros only define
;; when a known CPACK_GENERATOR is active; none was, so the passed-through values were
;; ignored and upstream's own SDK-style tree won.  What `guix build' produced (verified on
;; the built output) is:
;;     runtime/lib/intel64/       libopenvino.so* + the CPU/auto/hetero plugins
;;     runtime/include/
;;     runtime/cmake/             OpenVINOConfig.cmake — what openvino-genai's
;;                                -DOpenVINO_DIR has to name, NOT lib/cmake/OpenVINO
;;     python/openvino/           the pybind11 module
;; 123 MB total.  That is why nixpkgs carries install-path patches; rather than fight it we
;; point consumers at the paths upstream actually wrote, and (peteches services openvino)
;; names them explicitly instead of relying on a profile merge.  Python bindings install into
;; lib/python3.<minor>/site-packages/openvino with <minor> read from the very
;; python package passed as an input, so the two cannot drift apart.

(define-module (peteches packages openvino)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages base)            ;gnu-make — this guix keeps it in base, not build-tools
  #:use-module (gnu packages compression)     ;unzip (needed to unpack the gguf-tools .zip)
  #:use-module (gnu packages cmake)           ;cmake
  #:use-module (gnu packages oneapi)          ;onetbb — OpenVINO's exported cmake config demands TBBConfig.cmake
  #:use-module (gnu packages python-xyz)      ;python-numpy — this guix keeps it in python-xyz, not python-science
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)   ;python-setuptools, python-wheel
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages oneapi)         ;onetbb
  #:export (openvino openvino-genai))

;; The OpenVINO release line this trial is pinned to.  openvino-genai and
;; openvino-tokenizers version-lock to the same line upstream (their wheels are
;; published as `~=2026.4.1'), so anything added here has to match.
(define-public %openvino-version "2026.4.1")

(define ram-capped-job-count
  ;; A gexp-valued procedure rather than a plain one: `#$(lambda ...)' is rejected as
  ;; "invalid G-expression input" because raw procedures are not serializable into a
  ;; derivation, whereas #~(lambda ...) composes as gexp data and the call happens on the
  ;; build side.  Hence this body may use nothing but Guile itself — no guix bindings, no
  ;; references back into this module.
  #~(lambda (mib-per-job)
    "How many compile jobs this machine can afford at MIB-PER-JOB MiB resident each,
read from /proc/meminfo's MemAvailable.  Capped at 8 because the builders in this
fleet have between 4 and 24 cores and RAM, not cores, is what has been binding."
    ;; read-line lives in (ice-9 rdelim), which the build environment does NOT import:
    ;; gexps get (guix build utils) and (guix build syscalls) only, and gen 4 of this file
    ;; died on exactly that with "Unbound variable: read-line".
    (use-modules (ice-9 rdelim))
    (let ((avail-kb
           ;; If /proc/meminfo cannot be read (odd container, masked /proc), assume a
           ;; roomy machine rather than refusing to compile at all.
           (or (call-with-input-file "/proc/meminfo"
                 (lambda (port)
                   (let loop ()
                     (let ((line (read-line port)))
                       (cond ((eof-object? line) #f)
                             ((string-prefix? "MemAvailable:" line)
                              ;; Keep the digits only: "MemAvailable:   3854976 kB".
                              (string->number
                               (string-filter char-numeric? line)))
                             (else (loop)))))))
               8000000)))
      (max 1 (min 8 (inexact->exact (floor (/ avail-kb (* mib-per-job 1024)))))))))

(define-public openvino
  (package
    (name "openvino")
    (version %openvino-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openvinotoolkit/openvino.git")
             (commit version)
             (recursive? #t)))
       (file-name (git-file-name name version))
       ;; Recursive submodule checkout; see the module comment for how this was
       ;; computed.  Do not re-derive it from a GitHub archive tarball — those
       ;; leave the submodule directories empty and the build fails deep in
       ;; src/plugins/intel_cpu.
       (sha256
        (base32 "1vbig4h37r7qr8571k89f8ids77mr9dymfp8a9nsxy4p861r2nhb"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                    ; no CTest suite without upstream model fixtures
      ;; RAM, not cores, is what kills this build on a small builder.  Gen 3 died at -j4
      ;; when cc1plus was OOM-killed on oneDNN's cpu_convolution_list.cpp — the hungriest
      ;; translation unit in the CPU plugin, because it instantiates every convolution
      ;; kernel variant.  Guix's own build phase always passes an explicit -j = nproc and
      ;; cmake honours that over CMAKE_BUILD_PARALLEL_LEVEL, so replacing the phase is the
      ;; only way to get a job count that tracks available memory instead of core count.
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (invoke "cmake" "--build" "." "-j"
                      (number->string (#$ram-capped-job-count 900))))))
      ;; -Wno-dev: passing cache variables CMake does not know is a *developer*
      ;; warning here, and OpenVINO turns developer warnings into errors by
      ;; default.  Keep it off so a renamed flag degrades to "unused", not failure.
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
               "-Wno-dev"

               ;; Install destinations: see the module comment.  Without these the
               ;; tree lands half in $out/lib and half at the prefix root.
               "-DOV_CPACK_INCLUDEDIR=include"
               "-DOV_CPACK_OPENVINO_CMAKEDIR=lib/cmake/OpenVINO"
               "-DOV_CPACK_LIBRARYDIR=lib"
               "-DOV_CPACK_RUNTIMEDIR=lib"
               "-DOV_CPACK_ARCHIVEDIR=lib"
               "-DOV_CPACK_PLUGINSDIR=lib"
               (string-append "-DOV_CPACK_PYTHONDIR=lib/python"
                              #$(version-major+minor (package-version python))
                              "/site-packages")

               ;; CPU + GPU (Level Zero).  See the module comment: GPU_RT_TYPE=ZE
               ;; is the Battlemage-correct runtime, and it forces the vendored
               ;; Level Zero loader and the vendored OpenCL ICD loader in from
               ;; the submodule fetch — no system packages, no downloads.
               "-DENABLE_INTEL_CPU=ON"
               "-DENABLE_INTEL_GPU=ON"
               "-DGPU_RT_TYPE=ZE"
               "-DENABLE_INTEL_NPU=OFF"
               "-DENABLE_ONEDNN_FOR_GPU=ON"

               ;; Composite plugins GenAI expects; all CPU-side and cheap.
               "-DENABLE_AUTO=ON"
               "-DENABLE_HETERO=ON"
               "-DENABLE_MULTI=ON"
               "-DENABLE_TEMPLATE=ON"
               "-DENABLE_PROXY=ON"

               ;; Frontends: IR + ONNX only (see module comment).
               "-DENABLE_OV_IR_FRONTEND=ON"
               "-DENABLE_OV_ONNX_FRONTEND=ON"
               "-DENABLE_OV_TF_FRONTEND=OFF"
               "-DENABLE_OV_TF_LITE_FRONTEND=OFF"
               "-DENABLE_OV_JAX_FRONTEND=OFF"
               "-DENABLE_OV_PYTORCH_FRONTEND=OFF"
               "-DENABLE_OV_PADDLE_FRONTEND=OFF"

               ;; Python bindings, driven by CMake with the vendored pybind11
               ;; submodule rather than a system pybind11 (upstream guix has no
               ;; python-pybind11 at this channel pin anyway).  The interpreter is
               ;; referenced lexically as `python' — the same package object used
               ;; for native-inputs and for OV_CPACK_PYTHONDIR below, so the three
               ;; cannot drift.  (this-package-input "python" resolves to #f in a
               ;; nested unquote here, which is what made gen1 of this file fail at
               ;; configure time with "Wrong type (expecting string): #f".)
               "-DENABLE_PYTHON=ON"
               (string-append "-DPython_EXECUTABLE="
                              #$(file-append python "/bin/python3"))

               ;; TBB is the one bundled dependency worth replacing: upstream guix
               ;; has oneTBB (gnu packages oneapi) and OpenVINO's find_package(TBB)
               ;; finds its config file.  onetbb is a hard input below, so this is
               ;; unconditional.
               "-DENABLE_SYSTEM_TBB=ON"

               ;; ...and TBBBind has to be switched off explicitly.  src/cmake/
               ;; install_tbb.cmake first looks for a *dynamic* tbbbind_2_5 next to
               ;; the system libtbb (guix's onetbb ships libtbb/libtbbmalloc but no
               ;; libtbbbind_2_5.so — confirmed against the store dir), and when it
               ;; does not find one it falls back to DOWNLOADING Intel's prebuilt
               ;; static tbbbind_2_5 from storage.openvinotoolkit.org, which cannot
               ;; work in the sandbox: gen 2 of this file died at configure with
               ;; "downloading '.../tbbbind_2_5_static_lin_v4.tgz' failed".  What
               ;; turning it off costs is TBB's hwloc-based hybrid-core topology hint;
               ;; on a 14900KS that only affects how OV spreads threads between P- and
               ;; E-cores, not correctness, and a CPU trial can live with it.  The
               ;; alternative is packaging Intel's prebuilt static blob and passing
               ;; TBBBIND_2_5_ROOT — extra moving parts for a throughput nicety.
               "-DENABLE_TBBBIND_2_5=OFF"

               ;; Keep the build small and deterministic: no docs/samples/linting,
               ;; no tests (they need upstream model fixtures), no LTO (it costs a
               ;; lot of memory on this class of machine and buys nothing for a
               ;; trial).  ENABLE_NCC_STYLE needs Node.js to run upstream's naming
               ;; consistency checker.
               "-DBUILD_TESTING=OFF"
               "-DENABLE_FUNCTIONAL_TESTS=OFF"
               "-DENABLE_SAMPLES=OFF"
               "-DENABLE_DOCS=OFF"
               "-DENABLE_JS=OFF"
               "-DENABLE_LTO=OFF"
               "-DENABLE_NCC_STYLE=OFF"
               "-DENABLE_CPPLINT=OFF"
               "-DENABLE_CLANG_TIDY=OFF"
               "-DENABLE_FASTER_BUILD=OFF"))
     )
    (native-inputs
     (list python python-setuptools python-wheel git-minimal pkg-config))
    ;; onetbb only: the CPU plugin's oneDNN, onnx/protobuf-lite for the ONNX
    ;; front-end and pybind11 for the bindings all come from the vendored
    ;; submodules in the source fetch (see module comment).
    (inputs (list onetbb))
    ;; The python bindings import numpy unconditionally (openvino/utils/data_helpers/
    ;; data_dispatcher.py: "import numpy as np"), so a profile that carries openvino without
    ;; it fails at `import openvino` with ModuleNotFoundError — hit live in a guix shell.
    ;; Propagated rather than patched into the service because the dependency belongs to the
    ;; package, not to one consumer's environment.  requirements.txt alongside the bindings
    ;; also asks for openvino-telemetry>=2023.2.1, which this distro does not package; the
    ;; failed while these bindings were being checked (import openvino, import
    ;; openvino_genai and Core().available_devices all work without it); if a future
    ;; version does require it, it needs its own package here.
    (propagated-inputs (list python-numpy))
    (synopsis "OpenVINO inference runtime (CPU + Level Zero GPU plugins, IR + ONNX front-ends)")
    (description
     "OpenVINO is Intel's toolkit for deploying deep-learning models.  This build
provides the C and C++ inference runtimes, the Python bindings, the CPU plugin
(backed by oneDNN), the Intel GPU plugin with the Level Zero runtime
(GPU_RT_TYPE=ZE — the Battlemage-correct door for the Arc Pro B70), the
AUTO/HETERO/MULTI/TEMPLATE composite plugins and the IR and ONNX model
front-ends, plus the cmake config files downstream consumers such as
openvino-genai need.  The NPU plugin is disabled; there is no NPU on the box
this runs on.")
    (home-page "https://docs.openvinotoolkit.org/")
    (license license:asl2.0)))

;;; ── openvino-genai ───────────────────────────────────────────────────────
;;
;; The GenAI layer (LLMPipeline, tokenizer, chat templates) plus the tokenizers
;; library it builds out of its own submodule.  Two layout facts come straight from
;; reading this project's CMake and they shape everything below:
;;
;;   * Python installs to <prefix>/python/openvino_genai — an ordinary
;;     site-packages-style tree, like the core runtime above, but NOT under
;;     lib/pythonX.Y/site-packages.  That distinction is not cosmetic: guix's
;;     system-profile merge only folds in lib/pythonX.Y/site-packages, so neither OV
;;     python package would be importable from a profile on its own and
;;     (peteches services openvino) names <pkg>/python in PYTHONPATH explicitly.
;;   * Everything else goes to destinations the project hardcodes: libs to
;;     ${LIBRARY_DESTINATION} (pinned to `lib` below), headers to runtime/include,
;;     the exported cmake package to runtime/cmake.  Since install(EXPORT) writes
;;     those as relative paths, the generated OpenVINOGenAIConfig.cmake resolves them
;;     against its own location and needs no patching — which is why this recipe does
;;     not copy nixpkgs' config rewriting: nixpkgs forces everything into $out/lib and
;;     then has to rewrite the config to say so.  Asking cmake for a layout it already
;;     understands beats maintaining a substitute* phase.
;;
;; FetchContent is the interesting part for a sandboxed build.  src/cpp/CMakeLists.txt
;; and src/python/CMakeLists.txt pull five things at *configure* time that are not in
;; the source tree — nlohmann_json v3.11.3, hsnyder/safetensors.h, Lourdle/gguf-tools
;; (for GGUF), google/minja (the chat-template engine) and pybind11 v3.0.1 for the
;; bindings (upstream fetches its own rather than reusing OpenVINO's vendored copy,
;; because the two can conflict).  With no network those downloads are fatal — that is
;; exactly how gen 2 of the core package died on tbbbind — so each becomes a
;; copy-build-system origin here and is handed to cmake through the standard
;; FETCHCONTENT_SOURCE_DIR_* escape hatch.
;;
;; The three archives carry URL_HASH sha256s in that same CMake file, so every hash
;; below was cross-checked against Intel's own expectation before being written down:
;;   nlohmann v3.11.3  upstream 0d8ef5af… == guix 01lllyca… (same bytes)
;;   safetensors.h     upstream 9aaf5961… == guix 0jvxjdmd…
;;   gguflib           upstream 4d6eab50… == guix 0bf20bq0…
;; minja and pybind11 are pinned by tag/commit upstream instead; minja's hash came from
;; a shallow checkout with .git removed (the same convention as the two big recursive
;; sources), pybind11's straight from guix download of the pinned tarball.
;;
;; Licenses were read off the fetched artifacts rather than assumed: safetensors.h is
;; public domain (its LICENSE is the Unlicense text), minja and gguf-tools are MIT
;; (`license:expat' here, since this guix has no `license:mit'), pybind11 is BSD-3.
;; Note there is deliberately no `license:apache2.0' in this file either — the
;; identifier upstream guix uses for Apache-2.0 is asl2.0, which is what OpenVINO and
;; GenAI themselves are labelled with.
;;
;; Deliberate build choices:
;;   ENABLE_XGRAMMAR=OFF — grammar-constrained decoding (structured output /
;;     guaranteed-shaped tool calls) pulls mlc-ai/xgrammar *and its own submodules*.
;;     The trial needs none of that yet, and every extra FetchContent tree is another
;;     commit to pin and re-pin.  Flip it when tool calling needs guarantees.
;;   ENABLE_GGUF=ON — costs one small C library and buys the ability to load a .gguf
;;     directly, which matters for this fleet: LiquidAI publishes GGUF builds of
;;     LFM2.5-24B-A2B, so a GGUF-only path can skip `optimum-cli export openvino'
;;     entirely for a first smoke test.
;;   BUILD_TOKENIZERS stays ON (the upstream default): genai compiles its nested
;;     openvino_tokenizers submodule in the same pass, so there is no separate
;;     openvino-tokenizers package to define and no version skew across the
;;     "tokenizer must match the runtime" contract.  That costs three things this file
;;     pays for rather than downloads: the pinned tokenizers tree (its pinned commit is
;;     unreachable for Guix's depth-1 clone), plus sentencepiece and PCRE2, plus the
;;     TensorFlow frontend headers the core build does not install.  See the two source
;;     helpers and openvino-tensorflow-frontend-headers below.
;;   ENABLE_TESTS / ENABLE_SAMPLES=OFF — skips a sixth FetchContent download
;;     (googletest) and the sample binaries, on a box doing this with 4 cores and
;;     3.9 GiB of RAM for the first time.  Samples are the cheap flip if a C++ CLI is
;;     ever wanted: they need only nlohmann_json, already present.
;;   ENABLE_SYSTEM_OPENCL=OFF — genai's CDPruner can use OpenCL to prune and falls
;;     back to its CPU DPP implementation without it.  The GPU pipeline itself does
;;     not use the CDPruner's OpenCL path (that is speculative-decoding machinery
;;     this trial does not run), so there is nothing to gain from linking OpenCL
;;     into the GenAI layer; the core runtime's own GPU plugin carries the Level
;;     Zero stack it needs.

(define %genai-version "2026.4.1.0")

(define* (genai-thirdparty-source name version origin directory
                                  #:key summary description home-page license)
  "A source tree that openvino.genai would otherwise FetchContent at configure time.
Installed verbatim under DIRECTORY so a FETCHCONTENT_SOURCE_DIR_* flag can name it:
cmake wants a *source* tree there, not an install prefix with headers and a config
file — which is why these are copy-build-system packages rather than the real
packaged versions of nlohmann_json or pybind11."
  (package
    (name name)
    (version version)
    (source origin)
    ;; copy-build-system's unpack phase shells out to unzip for a .zip origin — gguf-tools
    ;; is fetched as GitHub's .zip archive and died in `unpack' with "uncaught exception:
    ;; #<&invoke-error program: \"unzip\">" until this line existed.  Tarballs need nothing,
    ;; so the other four bundles stay input-free.
    (native-inputs (let ((u (origin-uri origin)))
                     ;; string? guard: for a git-fetch origin the uri field is a
                     ;; <git-reference> record, and string-suffix? on it died with "wrong type
                     ;; argument in position 2 (expecting string)".
                     (if (and (string? u) (string-suffix? ".zip" u))
                         (list unzip)
                         '())))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~(list (list "." #$directory))))
    (synopsis summary)
    (description description)
    (home-page home-page)
    (license license)))

(define openvino-genai-nlohmann-json-src
  (genai-thirdparty-source "openvino-genai-nlohmann-json-src" "3.11.3"
    (origin (method url-fetch)
            (uri "https://github.com/nlohmann/json/archive/refs/tags/v3.11.3.tar.gz")
            (sha256 (base32 "01lllyca9b82ca81ic2bqxnbmcj92m4kq6c06hkf754pgypzb3hd")))
    "share/openvino-genai/nlohmann-json"
    #:summary "Header-only JSON library for modern C++ (vendored build source)"
    #:description "nlohmann/json, kept as a source tree so openvino.genai's FetchContent can be satisfied offline."
    #:home-page "https://github.com/nlohmann/json"
    #:license license:expat))

(define openvino-genai-safetensors-src
  (genai-thirdparty-source "openvino-genai-safetensors-src" "974a85d"
    (origin (method url-fetch)
            (uri "https://github.com/hsnyder/safetensors.h/archive/974a85d7dfd6e010558353226638bb26d6b9d756.tar.gz")
            (sha256 (base32 "0jvxjdmdwyxvd732rvappxgnwz6fgch2ln4nmadcy0cnc1hmkbws")))
    "share/openvino-genai/safetensors.h"
    #:summary "Header-only safetensors reader in C (vendored build source)"
    #:description "hsnyder/safetensors.h, kept as a source tree so openvino.genai's FetchContent can be satisfied offline.  Its LICENSE is the Unlicense text, hence public-domain."
    #:home-page "https://github.com/hsnyder/safetensors.h"
    #:license license:public-domain))

(define openvino-genai-gguf-tools-src
  (genai-thirdparty-source "openvino-genai-gguf-tools-src" "bac796a"
    (origin (method url-fetch)
            (uri "https://github.com/Lourdle/gguf-tools/archive/bac796ada809ac293e685db59b075971181cb008.zip")
            (sha256 (base32 "0bf20bq0wmv8nk0c4y22243cv74hyzi3zy7k6cl253a6am8anvjd")))
    "share/openvino-genai/gguf-tools"
    #:summary "Minimal GGUF container reader in C (vendored build source)"
    #:description "Lourdle/gguf-tools, kept as a source tree so openvino.genai's GGUF support can be built offline."
    #:home-page "https://github.com/Lourdle/gguf-tools"
    #:license license:expat))

(define openvino-genai-minja-src
  (genai-thirdparty-source "openvino-genai-minja-src" "3e4c61c"
    (origin (method git-fetch)
            (uri (git-reference
                   (url "https://github.com/google/minja.git")
                   (commit "3e4c61c616eda133cfb1e440fc7a14bf1729bbee")))
            (sha256 (base32 "0413qs5xn5v78a4i0ca7r20gn7dbq3clc0v8bczjcpkvs0nak8va")))
    "share/openvino-genai/minja"
    #:summary "Chat-template engine used by OpenVINO GenAI (vendored build source)"
    #:description "google/minja, the Jinja-ish chat-template interpreter openvino.genai embeds, kept as a source tree so its FetchContent can be satisfied offline.  Its LICENSE file is MIT text despite living under google/."
    #:home-page "https://github.com/google/minja"
    #:license license:expat))

(define openvino-genai-pybind11-src
  (genai-thirdparty-source "openvino-genai-pybind11-src" "3.0.1"
    (origin (method url-fetch)
            (uri "https://github.com/pybind/pybind11/archive/refs/tags/v3.0.1.tar.gz")
            (sha256 (base32 "1jnhd44py0s937bnikidrl5ndnmrazwm860zny5p6z3bfkd365kl")))
    "share/openvino-genai/pybind11"
    #:summary "C++/Python binding headers (vendored build source)"
    #:description "pybind11 3.0.1, the revision openvino.genai's python bindings pin, kept as a source tree so its FetchContent can be satisfied offline."
    #:home-page "https://github.com/pybind/pybind11"
    #:license license:bsd-3))

(define openvino-tokenizers-src
  (genai-thirdparty-source "openvino-genai-openvino-tokenizers-src" "2026.4.1.0-9bfda52"
    (origin (method url-fetch)
            (uri "https://github.com/openvinotoolkit/openvino_tokenizers/archive/9bfda52960774da1b9afafb37f8618673fe4d7c8.tar.gz")
            (sha256 (base32 "1m5zgz1aqmhzcvsjwanl9c8bwy92x6k7zla8sy3pn575wkcpwg06")))
    "share/openvino-genai/openvino-tokenizers"
    #:summary "HF tokenizer.json -> OpenVINO ops compiler (vendored submodule source)"
    #:description "openvino_tokenizers at the commit openvino.genai 2026.4.1.0 pins as a
submodule.  It is vendored rather than fetched recursively because Guix's depth-1 clone
cannot reach that pinned commit, which leaves thirdparty/openvino_tokenizers empty and
stops cmake at add_subdirectory; its own tree has no submodules and its CMake needs only
OpenVINO (both checked against this revision).  The revision was read from the GitHub
contents API for the 2026.4.1.0 tag.  The hash is the archive file's own SHA-256 written in
Guix nix-base32 — a url-fetch origin hashes the download, not the unpacked tree.  (Getting
this printed the obvious way fails: `guix hash -f base32' prints RFC4648, and Guix's base32
reader rejects characters outside its own alphabet, so the digest has to be re-encoded before
the module will load.)"
    #:home-page "https://github.com/openvinotoolkit/openvino_tokenizers"
    #:license license:asl2.0))

;; What openvino_tokenizers/src/CMakeLists.txt FetchContents, vendored for the same reason
;; as the five trees above: the sandbox has no network at configure time.  Both upstream
;; hashes are published in that file's URL_HASH fields, so the digests below are those SHA-256s
;; rewritten in Guix nix-base32 rather than measured locally.
(define openvino-genai-sentencepiece-src
  (genai-thirdparty-source "openvino-genai-sentencepiece-src" "0.2.1"
    (origin (method url-fetch)
            (uri "https://github.com/google/sentencepiece/releases/download/v0.2.1/sentencepiece-0.2.1.tar.gz")
            (sha256 (base32 "1b8znyis0rnvcd837jz9qvjl1kbl6dp036jdlgs848iggk1cwf41")))
    "share/openvino-genai/sentencepiece"
    #:summary "SentencePiece tokenizer library (vendored build source)"
    #:description "google/sentencepiece 0.2.1, the revision openvino_tokenizers pins for its
SPiece/Unigram support.  Vendored because openvino_tokenizers/src/CMakeLists.txt FetchContents
it and a Guix sandbox cannot download during configure."
    #:home-page "https://github.com/google/sentencepiece"
    #:license license:asl2.0))

(define openvino-genai-pcre2-src
  (genai-thirdparty-source "openvino-genai-pcre2-src" "10.48"
    (origin (method url-fetch)
            (uri "https://github.com/PCRE2Project/pcre2/releases/download/pcre2-10.48/pcre2-10.48.zip")
            (sha256 (base32 "181dvfk86v0fvn63l2xj2m8ngdj54y9csp9n0qchz3ilfjzyrngx")))
    "share/openvino-genai/pcre2"
    #:summary "Perl-compatible regular expression library (vendored build source)"
    #:description "PCRE2 10.48, which openvino_tokenizers builds from source for its regex
fast path.  Vendored for the same sandbox reason; the .zip needs unzip, which the helper adds
for zip origins."
    #:home-page "https://github.com/PCRE2Project/pcre2"
    #:license license:bsd-3))

;; openvino_tokenizers' ov_extension.cpp includes
;;   #include <openvino/frontend/tensorflow/extension/conversion.hpp>
;; unconditionally (src/CMakeLists.txt globs every .cpp, so there is no option to turn it
;; off), but the core runtime only installs that header when ENABLE_TENSORFLOW_FRONTEND=ON,
;; which we do not build — and building it means protobuf on a 3.9 GiB box.  The class it
;; uses is a header-only template over openvino::frontend::ExtensionMap, so what is missing
;; is the header, not a library: this package ships exactly that subtree, taken from the SAME
;; origin as the runtime so Guix reuses the checkout derivation already in the store instead
;; of downloading 957 MB again.  Without it the build dies at
;;   fatal error: openvino/frontend/tensorflow/extension/conversion.hpp: No such file
;; which is what happened on the first genai build that got as far as 31 %.
(define openvino-tensorflow-frontend-headers
  (package
    (name "openvino-tensorflow-frontend-headers")
    (version %openvino-version)
    (source (origin (inherit (package-source openvino))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan #~(list (list "src/frontends/tensorflow/include/openvino"
                                        "include/openvino"))))
    (synopsis "TensorFlow frontend headers from the OpenVINO source tree")
    (description "The openvino/frontend/tensorflow header subtree, installed on its own
because openvino_tokenizers includes it while our OpenVINO build does not enable — and
therefore does not install — that frontend.")
    (home-page "https://github.com/openvinotoolkit/openvino")
    (license license:asl2.0)))

(define-public openvino-genai
  (package
    (name "openvino-genai")
    (version %genai-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/openvinotoolkit/openvino.genai.git")
                     ;; No recursive? here on purpose — it does not work for this repo.
                     ;; Guix's shallow (single-branch, depth-1) clone cannot reach the pinned
                     ;; openvino_tokenizers commit, so the submodule directory arrives EMPTY,
                     ;; and configure dies at
                     ;;   CMake Error at thirdparty/CMakeLists.txt:8 (add_subdirectory):
                     ;;     The source directory .../source/thirdparty/openvino_tokenizers
                     ;;     does not contain a CMakeLists.txt file
                     ;; which is exactly what recursive? #t produced here.  The tree is vendored
                     ;; as its own source bundle below and dropped into that path by the
                     ;; populate-tokenizers-submodule phase instead.
                     (commit version)))
              (file-name (git-file-name name version))
              ;; Measured, not transcribed: this value is the `actual hash' Guix itself printed
              ;; when a first attempt mismatched, i.e. it came from this same drv's fixed-output
              ;; build on agents.
              ;; Honestly: this value is the `actual hash' Guix itself printed when a first
              ;; attempt mismatched, i.e. it was measured on agents from this same drv rather
              ;; than transcribed from an earlier recipe.
              (sha256
               (base32
                "1dzxv2c1lnsrsh0rpb2ab8dx1i0s8pazn7wpv3dgh33p35qg61rn"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                ; googletest would be a sixth FetchContent download
      #:build-type "Release"
      #:configure-flags
      #~(append
         ;; find_package(OpenVINO): point at where the core package's config file really
         ;; landed — runtime/cmake, not the lib/cmake/OpenVINO its OV_CPACK_* flags asked
         ;; for (see the module comment: upstream's own layout won).  GenAI also looks at a
         ;; second dir, OpenVINO_DIR_PY, for the python half; upstream points both at one
         ;; tree for non-wheel builds and so do we.
         (list (string-append "-DOpenVINO_DIR=" #$(file-append openvino "/runtime/cmake"))
               (string-append "-DOpenVINO_DIR_PY=" #$(file-append openvino "/runtime/cmake"))

               ;; TBB.  openvino's OpenVINOConfig.cmake resolves its threading dependency in
               ;; config mode — it hardcodes enable_pkgconfig_tbb to "" and enable_system_tbb
               ;; to ON, so neither the pkg-config path nor the "use our vendored copy"
               ;; branch is reachable from outside, and _ov_find_dependency(TBB) lands on a
               ;; plain find_package(TBB).  Guix's oneTBB ships lib/cmake/TBB/TBBConfig.cmake,
               ;; so naming that directory satisfies it:
               ;;   CMake Error ... By not providing "FindTBB.cmake" in CMAKE_MODULE_PATH
               ;;   Could not find a package configuration file provided by "TBB" (2022.3.0)
               ;; which is exactly how this flag earned its place.
               ;; openvino_tokenizers' header-only need for the TensorFlow frontend, which our
               ;; core build does not install (see openvino-tensorflow-frontend-headers).  A
               ;; -isystem rather than a find-path: CMAKE_INCLUDE_PATH only feeds find_path,
               ;; not how #include is resolved.
               (string-append "-DCMAKE_CXX_FLAGS=-isystem"
                              #$(file-append openvino-tensorflow-frontend-headers "/include"))

               (string-append "-DTBB_DIR=" #$(file-append onetbb "/lib/cmake/TBB"))

               ;; install(TARGETS ... LIBRARY DESTINATION ${LIBRARY_DESTINATION}) in
               ;; src/cpp/CMakeLists.txt; headers and the cmake package go to hardcoded
               ;; runtime/include and runtime/cmake, so `lib` here is what makes the
               ;; exported config resolve without patching.
               "-DLIBRARY_DESTINATION=lib"
               "-DARCHIVE_DESTINATION=lib"
               "-DRUNTIME_DESTINATION=bin"

               ;; FetchContent escape hatches (see the block above).  cmake uppercases
               ;; the declared name, dots and all: FETCHCONTENT_SOURCE_DIR_SAFETENSORS.H
               ;; matches upstream's `FetchContent_Declare(safetensors.h ...)'.
               (string-append "-DFETCHCONTENT_SOURCE_DIR_NLOHMANN_JSON="
                              #$(file-append openvino-genai-nlohmann-json-src
                                             "/share/openvino-genai/nlohmann-json"))
               (string-append "-DFETCHCONTENT_SOURCE_DIR_SAFETENSORS.H="
                              #$(file-append openvino-genai-safetensors-src
                                             "/share/openvino-genai/safetensors.h"))
               (string-append "-DFETCHCONTENT_SOURCE_DIR_GGUFLIB="
                              #$(file-append openvino-genai-gguf-tools-src
                                             "/share/openvino-genai/gguf-tools"))
               (string-append "-DFETCHCONTENT_SOURCE_DIR_MINJA="
                              #$(file-append openvino-genai-minja-src
                                             "/share/openvino-genai/minja"))
               (string-append "-DFETCHCONTENT_SOURCE_DIR_PYBIND11="
                              #$(file-append openvino-genai-pybind11-src
                                             "/share/openvino-genai/pybind11"))
               ;; The two trees openvino_tokenizers itself FetchContents.  cmake uppercases
               ;; the declared name, so SENTENCEPIECE / PCRE2 match its sentencepiece / pcre2.
               (string-append "-DFETCHCONTENT_SOURCE_DIR_SENTENCEPIECE="
                              #$(file-append openvino-genai-sentencepiece-src
                                             "/share/openvino-genai/sentencepiece"))
               (string-append "-DFETCHCONTENT_SOURCE_DIR_PCRE2="
                              #$(file-append openvino-genai-pcre2-src
                                             "/share/openvino-genai/pcre2"))

               ;; genai's CDPruner falls back to its CPU DPP implementation; the
               ;; GPU pipeline does not use the CDPruner's OpenCL path, so there is
               ;; no reason to link OpenCL into the GenAI layer.  The core runtime's
               ;; GPU plugin (Level Zero) is what reaches the Arc.
               "-DENABLE_SYSTEM_OPENCL=OFF"
               "-DENABLE_XGRAMMAR=false"
               "-DENABLE_GGUF=ON"
               "-DBUILD_TOKENIZERS=ON"
               ;; tools/continuous_batching (the `openvino_genai_cli' sample) is gated by
               ;; ENABLE_TOOLS, whose upstream default is ON, and it FetchContents cxxopts —
               ;; ENABLE_TOOLS, whose upstream default is ON, and it FetchContents cxxopts —
               ;; another download the sandbox refuses: "error: downloading
               ;; https://github.com/jarro2783/cxxopts/...".  We serve models through the python
               ;; API, so the CLI buys nothing here; re-enabling it means vendoring cxxopts the
               ;; same way nlohmann_json is vendored above.
               "-DENABLE_TOOLS=false"
               ;; Samples have no gate in the root CMakeLists (add_subdirectory(samples) is
               ;; unconditional on directory existence); samples/CMakeLists.txt itself opens
               ;; with `if(ENABLE_SAMPLES)' — whose upstream default is ON.  Leaving it on cost
               ;; a real sandbox failure: samples/cpp/text_generation FetchContents cxxopts and
               ;; the download was refused. Same reason for tests (googletest) and for misaki_cpp
               ;; (ENABLE_MISAKI_CPP defaults ON in src/cpp/CMakeLists.txt line 13 and pulls two
               ;; more trees).  We serve models through python's openvino_genai; Kokoro TTS is
               ;; AllTalk's job on this VM, not OpenVINO GenAI's.
               "-DENABLE_SAMPLES=false"
               "-DENABLE_TESTS=false"
               "-DENABLE_MISAKI_CPP=false"
               "-DENABLE_PYTHON=ON"
               ;; The interpreter cmake builds the pybind11 module against.  Spelled with
               ;; file-append like every other store path in this file: a bare `python' here
               ;; reached string-append as the package OBJECT during gexp lowering and died
               ;; with "wrong type (expecting string): #<package python@3.12.12>".
               (string-append "-DPython_EXECUTABLE=" #$(file-append python "/bin/python3"))))
      #:phases
      #~(modify-phases %standard-phases
          ;; The submodule cmake wants at thirdparty/openvino_tokenizers, refilled from the
          ;; vendored bundle above.  The path is relative to the SOURCE directory, not the drv
          ;; root: with this build system 'unpack leaves the working directory inside `source'
          ;; (proved by the first attempt, which happily created a stray source/source/thirdparty
          ;; tree while the real submodule directory stayed empty).
          (add-after 'unpack 'populate-tokenizers-submodule
            (lambda _
              (copy-recursively #$(file-append openvino-tokenizers-src
                                               "/share/openvino-genai/openvino-tokenizers")
                                "thirdparty/openvino_tokenizers")))
          (add-after 'unpack 'relax-cmake-policies
            (lambda _
              ;; nixpkgs carries this clamp for openvino.genai because the FetchContent
              ;; payloads it pulls still declare cmake_minimum_required(VERSION <3.5),
              ;; which CMake 4 (4.1.3 in guix today) rejects outright.  GenAI-specific:
              ;; the core runtime configures fine under CMake 4 with no clamp at all.
              (setenv "CMAKE_POLICY_VERSION_MINIMUM" "3.5")))
          ;; Same RAM-aware job cap as the core runtime, for the same reason: cpu_convolution_list.cpp OOM-killed cc1plus there at -j4 on a 4-core / 3.9 GiB builder.
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (invoke "cmake" "--build" "." "-j"
                      (number->string (#$ram-capped-job-count 900)))))
          (add-before 'configure 'no-network
            (lambda _
              ;; Every FetchContent above is source-dir-overridden, so cmake should not
              ;; reach for the network at all.  Saying so explicitly turns a stray
              ;; download — which would succeed on a connected machine and fail here,
              ;; or worse, succeed differently — into an immediate loud failure.
              (setenv "FETCHCONTENT_FULLY_DISCONNECTED" "ON"))))))
    ;; onetbb here is not for compiling genai — it is for the TBBConfig.cmake its
    ;; lib/cmake/TBB ships, which openvino's own exported OpenVINOConfig.cmake insists on
    ;; (see the TBB_DIR flag below).  Propagated rather than merely native because the link
    ;; dependency survives into the installed library, and a service profile that carries
    ;; openvino-genai but not oneTBB would start and die at import time.
    (propagated-inputs (list onetbb))
    (native-inputs (list cmake pkg-config gnu-make git-minimal python onetbb
                         openvino-tokenizers-src openvino-genai-sentencepiece-src
                         openvino-genai-pcre2-src
                         openvino-tensorflow-frontend-headers))
    (inputs (list openvino
                  openvino-genai-nlohmann-json-src
                  openvino-genai-safetensors-src
                  openvino-genai-gguf-tools-src
                  openvino-genai-minja-src
                  openvino-genai-pybind11-src))
    (synopsis "LLM generation library on top of the OpenVINO runtime")
    (description
     "OpenVINO GenAI is the generation layer over OpenVINO: an LLMPipeline with streamer
support, its own in-graph tokenizer built from the bundled openvino_tokenizers, chat-template
rendering through minja, GGUF loading, and Python bindings.  Built against the
Level Zero GPU-enabled core runtime above, so an LLMPipeline constructed with
device=\"GPU\" runs on the Arc Pro B70; the GenAI layer itself links no OpenCL
(the CDPruner uses its CPU DPP fallback).")
    (home-page "https://github.com/openvinotoolkit/openvino.genai")
    (license license:asl2.0)))
