(define-module (peteches packages koboldcpp)
    ;; Build inputs
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
    ;; Guix basics
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses))
;; -----------------------------------------------------------------------------
;; CPU package (unchanged except tiny cleanup)
;; -----------------------------------------------------------------------------

(define-public koboldcpp
  (package
    (name "koboldcpp")
    (version "1.89")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LostRuins/koboldcpp")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q6wqg5p5g3yq3wxygdcc13b1v0nygx9b9h5v0dlp7x5z8m4rj7y"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; Keep it portable: CPU-only.
      #:configure-flags #~(list "-DLLAMA_CUBLAS=OFF"
                                "-DLLAMA_METAL=OFF"
                                "-DLLAMA_VULKAN=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-bin
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (mkdir-p bin)
                (install-file (car (find-files "." "^koboldcpp$")) bin)
                (with-output-to-file (string-append bin "/koboldcpp-run")
                  (lambda ()
                    (display "#!/bin/sh\nexec \"" )
                    (display (string-append out "/bin/koboldcpp"))
                    (display "\" \"$@\"\n")))
                (chmod (string-append bin "/koboldcpp-run") #o755)
                #t))))))
    (inputs (list zlib))
    (native-inputs (list cmake pkg-config gcc-toolchain))
    (home-page "https://github.com/LostRuins/koboldcpp")
    (synopsis "Run GGUF/GGML models with a lightweight UI/API (CPU-only build)")
    (description
     "KoboldCpp launcher and server for GGUF/GGML models. CPU-only build for Guix.")
    (license agpl3+)))

;; -----------------------------------------------------------------------------
;; CUDA package (inherits from CPU and enables cuBLAS)
;; -----------------------------------------------------------------------------

;; Try to load cuda-toolkit from nonguix; fail clearly if missing.
(define cuda-toolkit
  (let ((mod (false-if-exception
              (resolve-interface '(nonguix packages nvidia)))))
    (if mod
        (module-ref mod 'cuda-toolkit)
        (error
         "koboldcpp-cuda requires the 'nonguix' channel (module (nonguix packages nvidia)) providing cuda-toolkit."))))

(define-public koboldcpp-cuda
  (package
    (inherit koboldcpp)
    (name "koboldcpp-cuda")
    (synopsis "KoboldCpp with CUDA/cuBLAS acceleration")
    (arguments
     (let ((base-args (package-arguments koboldcpp)))
       (substitute-keyword-arguments base-args
         ((#:configure-flags flags
           #~(list
              ;; Enable CUDA/cuBLAS backend in llama.cpp.
              "-DLLAMA_CUBLAS=ON"
              ;; Optional knobs (keep simple/portable; override via extra-args at runtime):
              ;; "-DLLAMA_CUDA_F16=ON"
              ;; You can set architectures via CMAKE_CUDA_ARCHITECTURES if you like.
              )))
         ;; phases remain identical; the upstream target name is the same.
         )))
    ;; Runtime needs CUDA libs; build needs nvcc. Put toolkit in both inputs.
    (inputs (append (list cuda-toolkit) (package-inputs koboldcpp)))
    (native-inputs (append (list cuda-toolkit) (package-native-inputs koboldcpp)))
    (description
     "KoboldCpp built with CUDA/cuBLAS support (via llama.cpp). Requires the non-free CUDA toolkit from the 'nonguix' channel at build and run time.")))
