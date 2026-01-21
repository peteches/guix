(define-module (peteches packages koboldcpp)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)        ; glibc
  #:use-module (gnu packages elf)         ; patchelf
  #:use-module (guix licenses)
  ;; your nonfree CUDA toolkit module (for rpath only)
  #:use-module (guix-science-nonfree packages cuda))

(define* (koboldcpp-prebuilt name version uri sha32 #:key cuda?)
  (package
   (name name)
   (version version)
   (source (origin
            (method url-fetch)
            (uri uri)
	    (file-name (string-append "koboldcpp-" version "-binary"))
            (sha256 (base32 sha32))))
   (build-system copy-build-system)
(arguments
 (list
  ;; We only need (guix build utils) for install-file/mkdir-p/chmod/invoke.
;  #:modules '((guix build utils))
  #:phases
  #~(modify-phases %standard-phases
      ;; No archive → nothing to unpack.
      (delete 'unpack)

      ;; Prebuilt binaries often don’t need/want stripping (can even fail).
      (delete 'strip)

      (replace 'install
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (use-modules (guix build utils))
          (let* ((out (assoc-ref outputs "out"))
                 (bin (string-append out "/bin"))
                 (src (assoc-ref inputs "source"))   ; raw downloaded file
                 (dst (string-append bin "/koboldcpp")))
            (mkdir-p bin)
            (install-file src bin)
            ;; Normalize the filename to a stable program name.
            (rename-file (string-append bin "/" (basename src)) dst)
            (chmod dst #o755))))

      (add-after 'install 'patchelf-fix
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (use-modules (guix build utils))
          (let* ((out    (assoc-ref outputs "out"))
                 (bin    (string-append out "/bin/koboldcpp"))
                 (glibc  (assoc-ref inputs "libc"))     ; e.g. glibc
                 (gcc    (assoc-ref inputs "gcc:lib"))  ; for libstdc++
                 (cuda   (assoc-ref inputs "cuda-toolkit")) ; optional
                 (pat    (search-input-file inputs "/bin/patchelf"))
                 (interp (string-append glibc "/lib/ld-linux-x86-64.so.2"))
                 (rpath  (string-join
                          (append
                           (list (string-append glibc "/lib")
                                 (string-append gcc "/lib"))
                           (if cuda
                               (list (string-append cuda "/lib64"))
                               '()))
                          ":")))
            (invoke pat "--set-interpreter" interp "--set-rpath" rpath bin)))))))
   (native-inputs (list patchelf))
   (inputs (append (list (list "libc" glibc)
			 (list "gcc:lib" gcc "lib"))
                   (if cuda? (list (list "cuda-toolkit" cuda-toolkit)) '())))
   (home-page "https://github.com/LostRuins/koboldcpp")
   (synopsis (string-append "KoboldCpp prebuilt binary (" (if cuda? "CUDA" "CPU") ")"))
   (description
    (string-append
     "Installs the upstream prebuilt KoboldCpp single-file binary and fixes the "
     "ELF interpreter. For the CUDA variant we also set an rpath to the toolkit "
     "libraries so cuBLAS/cuDART resolve on Guix. The NVIDIA driver (libcuda.so.1) "
     "must be present at runtime on CUDA systems."))
   (license expat)))

;; === Pins (use v1.99.4 as requested) ===
(define koboldcpp-version "1.105.4")

;; CUDA-enabled (modern PCs, CUDA12+ AVX2)
(define-public koboldcpp-bin
  (koboldcpp-prebuilt
   "koboldcpp-bin"
   koboldcpp-version
   (string-append "https://github.com/LostRuins/koboldcpp/releases/download/v"
                  koboldcpp-version "/koboldcpp-linux-x64")
   ;; TODO: run 'guix download' on the URL above and paste the base32 hash here:
   "1d7k939wc8zjzzl5kwxiyr2by54njpkg0cmk8d83pnd6wsjji9xp"
   #:cuda? #t))

;; CPU-only (nocuda)
(define-public koboldcpp-nocuda-bin
  (koboldcpp-prebuilt
   "koboldcpp-nocuda-bin"
   koboldcpp-version
   (string-append "https://github.com/LostRuins/koboldcpp/releases/download/v"
                  koboldcpp-version "/koboldcpp-linux-x64-nocuda")
   ;; TODO: run 'guix download' on the URL above and paste the base32 hash here:
   "0f2ffzjij6bpwbkfvfqnfdffddzsb9nhk2a1nwx149la67lkdm5b"
   #:cuda? #f))
