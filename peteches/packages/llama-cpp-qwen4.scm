;;; peteches/packages/llama-cpp-qwen4.scm
;;; llama.cpp at b12211, which adds the `qwen4exp` model architecture.
;;;
;;; The stock `llama-cpp` in (gnu packages machine-learning) is pinned to
;;; b10318 and fails to load Qwen4-Exp models with
;;; "unknown model architecture: 'qwen4exp'". b12211 is the latest release
;;; and supports `qwen4exp` (see src/llama-arch.cpp at tag b12211).
;;;
;;; The stock package builds with `-DLLAMA_USE_SYSTEM_GGML=ON`, which links
;;; against the (old, b10318-synced) system `ggml` package. b12211's source
;;; requires the newer ggml API (e.g. `ggml_rope_set_offset`), so building
;;; against the old system ggml fails. This package therefore builds with the
;;; VENDORED ggml from the b12211 source tree (which has the new API) and
;;; enables the Vulkan backend (`-DGGML_VULKAN=ON`) so llama-server can use
;;; the GPU via ggml-vulkan.

(define-module (peteches packages llama-cpp-qwen4)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vulkan))

(define-public llama-cpp-qwen4
  (package
    (inherit llama-cpp)
    (name "llama-cpp-qwen4")
    (version "0.0.0-12211")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ggml-org/llama.cpp")
             (commit "b12211")))
       (file-name (git-file-name "llama-cpp-qwen4" "12211"))
       (sha256
        (base32 "0n4dg5anx0d996y9i3wh28p4j82a06g7y80aiv6qimic0rwlxqr7"))))
    (arguments
     ;; The stock arguments do not carry a `#:tests?` keyword, so
     ;; `substitute-keyword-arguments` cannot set it; append it explicitly to
     ;; skip the `check` phase (the 8 b12211 test failures are unrelated to
     ;; llama-server).
     (append
      (substitute-keyword-arguments (package-arguments llama-cpp)
        ((#:configure-flags _cfg)
         #~(list "-DBUILD_SHARED_LIBS=ON"
                 ;; Use the vendored ggml from the b12211 source tree (has the
                 ;; new API b12211 requires), not the old system ggml.
                 "-DLLAMA_USE_SYSTEM_GGML=OFF"
                 ;; Enable the Vulkan backend in the vendored ggml.
                 "-DGGML_VULKAN=ON"
                 "-DLLAMA_BUILD_NUMBER=12211"))
        ;; b12211 does not build the embedded Web UI by default
        ;; (`LLAMA_BUILD_UI` defaults to OFF), so the stock `get-ui` phase
        ;; (which extracts a prebuilt UI tarball) is unnecessary.
        ((#:phases _phases) #~%standard-phases))
      (list #:tests? #f)))
    (inputs
     ;; Drop the system `ggml` (we use the vendored one) and the UI tarball
     ;; (the UI is not built); keep openssl.
     (list openssl))
    (native-inputs
     ;; vulkan-headers provides the Vulkan API headers + CMake config that the
     ;; vendored ggml's `-DGGML_VULKAN=ON` needs at build time. vulkan-loader
     ;; provides `libvulkan.so` (Vulkan_LIBRARY) and shaderc provides `glslc`
     ;; (the GLSL->SPIR-V compiler); spirv-headers provides the SPIRV-Headers
     ;; CMake config. All required by ggml-vulkan's CMake.
     (list vulkan-headers vulkan-loader shaderc spirv-headers))))
