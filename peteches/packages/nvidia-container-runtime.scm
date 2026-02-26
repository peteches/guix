;; Local package file you can install with: guix install -f nvidia-container-toolkit.scm
(define-module (peteches packages nvidia-container-runtime)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module  (guix build utils)
  #:use-module  (guix packages)
  #:use-module  (guix build-system go)
  #:use-module  (guix build-system gnu)
  #:use-module  (guix licenses)

  #:use-module  (gnu packages)
  #:use-module  (gnu packages docker)
  #:use-module  (gnu packages commencement)
  #:use-module  (gnu packages onc-rpc)
  #:use-module  (gnu packages golang)
  #:use-module  (gnu packages check)
  #:use-module  (gnu packages base)
  #:use-module  (gnu packages curl)
  #:use-module  (gnu packages version-control)
  #:use-module  (gnu packages pkg-config)
  #:use-module  (gnu packages gcc)
  #:use-module  (gnu packages tls)
  #:use-module  (gnu packages elf)
  #:use-module  (gnu packages m4)
  #:use-module  (gnu packages linux)

  #:use-module  (nongnu packages nvidia))
(define-public nvidia-modprobe
  (package
   (name "nvidia-modprobe")
   (version "550.54.14")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/NVIDIA/nvidia-modprobe")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1a7q03pnwk3wa0p57whwv2mvz60bv77vvvaljqzwnscpyf94q548"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:tests? #f
     #:phases
     #~(modify-phases %standard-phases
		      (delete 'configure)
		      (add-before 'build 'set-correct-cflags
				  (lambda _
				    (setenv "CFLAGS" "-fPIC")
				    (substitute* "modprobe-utils/nvidia-modprobe-utils.c"
						 (("^static int nvidia_cap_get_device_file_attrs")
						  "int nvidia_cap_get_device_file_attrs"))))
		      (add-after 'build 'build-static-link-libraries
				 (lambda* (#:key outputs #:allow-other-keys)
				   (invoke "ar" "rcs"
					   "_out/Linux_x86_64/libnvidia-modprobe-utils.a"
					   "_out/Linux_x86_64/nvidia-modprobe-utils.o"
					   "_out/Linux_x86_64/pci-sysfs.o")
				   (copy-recursively "_out/Linux_x86_64/"
						     (string-append (assoc-ref outputs "out") "/lib"))))
		      (delete 'check)
		      (add-after 'patch-source-shebangs 'replace-prefix
				 (lambda* (#:key outputs #:allow-other-keys)
				   (setenv "CC" "gcc")
				   (setenv "PREFIX" (assoc-ref outputs "out"))
				   (copy-recursively "modprobe-utils/"
						     (string-append (assoc-ref outputs "out") "/include")))))))
   (native-inputs (list gcc-toolchain m4))
   (synopsis "Load the NVIDIA kernel module and create NVIDIA character device files")
   (description "Load the NVIDIA kernel module and create NVIDIA character device files.")
   (home-page "https://github.com/NVIDIA/nvidia-modprobe")
   (license gpl2)))

(define-public libnvidia-container
  (package
   (name "libnvidia-container")
   (version "1.18.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/NVIDIA/libnvidia-container")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     ;; (patches (list (local-file "libnvidia-container.patch")))
     (sha256
      (base32 "0rw9iw80n35hlss1ycycd8yzs052mrfc3bg4kwmlcrhbca5k44hc"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:tests? #f
     #:make-flags
     #~(list
	;; If you keep nvcgo disabled for now:
	"WITH_NVCGO=no"
	"BUILD_DEFS=src/build.h"

	;; v1.18.x Makefile expects these:
	"VERSION_MAJOR=1"
	"VERSION_MINOR=18"
	"VERSION_PATCH=1"
	"VERSION=1.18.1"
	"VERSION_STRING=1.18.1"
	"TAG=")

     #:phases
     #~(modify-phases %standard-phases
		      (delete 'configure)
		      (delete 'check)
		      (delete 'strip)

		      (add-after 'unpack 'ensure-writable-source
				 (lambda _
				   (setenv "HOME" "/tmp")
				   (for-each make-file-writable
					     '("src/ldcache.c"
					       "src/ldcache.h"
					       "src/nvc_info.c"
					       "src/nvc.c"
					       "mk/nvcgo.mk"
					       "Makefile"))
				   #t))

		      (add-after 'patch-source-shebangs 'replace-prefix
				 (lambda* (#:key outputs inputs #:allow-other-keys)
				   (let* ((out (assoc-ref outputs "out"))
					  (nmm (assoc-ref inputs "nvidia-modprobe")))
				     (substitute* "Makefile"
						  (("/usr/local") out)
						  ((".*nvidia-modprobe\\.mk.*") "\n")
						  (("^all: shared static tools") "all: shared tools")
						  ((".*LIB_STATIC.*libdir.*$") "")
						  ;; Make deps a no-op (avoids mk/nvcgo.mk path)
						  (("^deps:.*$")
						   "deps:\n\ttrue\n"))

				     ;; If mk/nvcgo.mk is invoked anyway, neuter it.
				     (substitute* "mk/nvcgo.mk"
						  (("^[[:space:]]*rm -rf .*")
						   "\tmkdir -p ${SRCS_DIR} && echo \"sources dir: ${SRCS_DIR}\"\n")
						  (("^[[:space:]]*\\$\\(MAKE\\)[[:space:]]+-C[[:space:]]+\\$\\(SRCS_DIR\\).*clean[[:space:]]*$")
						   "\ttrue\n")
						  (("^[[:space:]]*\\$\\(MAKE\\)[[:space:]]+-C[[:space:]]+\\$\\(SRCS_DIR\\).*build[[:space:]]*$")
						   "\ttrue\n")
						  (("^[[:space:]]*\\$\\(MAKE\\)[[:space:]]+-C[[:space:]]+\\$\\(SRCS_DIR\\).*install[[:space:]]*$")
						   "\ttrue\n"))

				     ;; NVML path pinned to system profile.
				     (substitute* "src/cli/libnvc.c"
						  (("libnvidia-ml\\.so\\.1")
						   "/run/current-system/profile/lib/libnvidia-ml.so.1"))
				     (substitute* "src/nvc_internal.h"
						  (("libnvidia-ml\\.so\\.1")
						   "/run/current-system/profile/lib/libnvidia-ml.so.1"))

				     ;; Fix missing prototype (header doesn’t declare it on some builds).
				     ;; Match the include line and inject a prototype immediately after.
				     (substitute* "src/nvc_info.c"
						  (("#include <nvidia-modprobe-utils\\.h>\n")
						   "#include <nvidia-modprobe-utils.h>\n\
int nvidia_cap_get_device_file_attrs(const char *path, unsigned int *major, unsigned int *minor, char *name);\n"))

				     ;; Includes + libs: tirpc + nvidia-modprobe headers/libs.
				     (setenv "C_INCLUDE_PATH"
					     (string-append (or (getenv "C_INCLUDE_PATH") "")
							    ":" #$libtirpc "/include/tirpc"
							    ":" nmm "/include"))
				     (setenv "LIBRARY_PATH"
					     (string-append (or (getenv "LIBRARY_PATH") "")
							    ":" #$libtirpc "/lib"
							    ":" nmm "/lib"))

				     ;; Link what we use.
				     (setenv "LDFLAGS"
					     (string-append (or (getenv "LDFLAGS") "")
							    " -ltirpc -lseccomp -lcap -lnvidia-modprobe-utils"
							    " -Wl,-rpath=" out "/lib"))

				     (setenv "CFLAGS"
					     (string-append (or (getenv "CFLAGS") "")
							    " -DWITH_TIRPC -g"))

				     (substitute* "Makefile"
						  (("^WITH_LIBELF.*no") "WITH_LIBELF ?= yes"))
				     (substitute* "mk/common.mk"
						  (("^REVISION.*")
						   (string-append "REVISION ?= " #$version "\n" "CC := gcc\n"))))
				   #t))

		      (add-after 'patch-source-shebangs 'fix-linker-script-usage
				 (lambda _
				   ;; Upstream expects libnvidia-container.lds to be used as a linker script.
				   ;; If it's passed as a normal input file, ld warns (forgot -T) and the
				   ;; resulting .so can be malformed (RUNPATH validation then fails).
				   (substitute* "Makefile"
						(("src/libnvidia-container\\.lds")
						 "-Wl,-T,src/libnvidia-container.lds"))
				   #t))


		      ;; Generate RPC outputs inside src/ (prevents src/src include weirdness).
		      (add-before 'build 'generate-rpc
				  (lambda _
				    (with-directory-excursion "src"
							      (invoke "rm" "-f" "nvc_rpc.h" "nvc_xdr.c" "nvc_svc.c" "nvc_clt.c")
							      (invoke "rpcgen" "-h" "-DWITH_NVCGO" "-C" "-M" "-N" "-o" "nvc_rpc.h" "nvc_rpc.x")
							      (invoke "rpcgen" "-c" "-DWITH_NVCGO" "-C" "-M" "-N" "-o" "nvc_xdr.c" "nvc_rpc.x")
							      (invoke "rpcgen" "-m" "-DWITH_NVCGO" "-C" "-M" "-N" "-o" "nvc_svc.c" "nvc_rpc.x")
							      (invoke "rpcgen" "-l" "-DWITH_NVCGO" "-C" "-M" "-N" "-o" "nvc_clt.c" "nvc_rpc.x"))
				    #t))

		      ;; Provide BUILD_* macros expected by src/nvc.c.
		      (add-before 'build 'write-build-h
				  (lambda _
				    (call-with-output-file "src/build.h"
				      (lambda (p)
					(format p "/* Guix-generated build metadata */~%")
					(format p "#ifndef BUILD_H~%#define BUILD_H~%~%")
					(format p "#define BUILD_DATE \"unknown\"~%")
					(format p "#define BUILD_REVISION \"~a\"~%" #$version)
					(format p "#define BUILD_COMPILER \"gcc\"~%")
					(format p "#define BUILD_PLATFORM \"guix\"~%")
					(format p "#define BUILD_FLAGS \"WITH_NVCGO=no\"~%~%")
					(format p "#endif~%")))
				    #t)))))
   (native-inputs
    (list
     pkg-config
     rpcsvc-proto
     nvidia-modprobe
     libseccomp libcap elfutils libtirpc
     git-minimal curl tar coreutils
     go gcc-toolchain which docker))
   (synopsis "Library and tools for NVIDIA GPU support in containers")
   (description "The libnvidia-container library and tools used by the NVIDIA Container Toolkit.")
   (home-page "https://github.com/NVIDIA/libnvidia-container")
   (license asl2.0)))

(define-public nvidia-container-toolkit
  (package
    (name "nvidia-container-toolkit")
    (version "1.18.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/nvidia-container-toolkit")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "17z1swyawqmsvin1wmjpvqql0q5vyc5ac5zl3cmjapnd55zhrcda"))))
    (build-system go-build-system)
    (arguments
 (list
  #:go (specification->package "go@1.25.7")
  #:import-path "github.com/NVIDIA/nvidia-container-toolkit"
  #:tests? #f
  #:install-source? #f
  #:phases
  #~(modify-phases %standard-phases
      (add-after 'unpack 'fix-paths
        (lambda* (#:key import-path #:allow-other-keys)
          (substitute* (string-append "src/" import-path "/internal/config/config.go")
            (("/usr/bin") "/run/current-system/profile/bin"))
          #t))

      ;; Force module mode and prevent toolchain downloads in the build container.
      (add-after 'unpack 'go-modules-settings
		 (lambda _
		   (setenv "GO111MODULE" "on")
		   (setenv "GOTOOLCHAIN" "local")
		   (setenv "GOPROXY" "off")
		   (setenv "GOSUMDB" "off")
		   ;; Use vendored deps if present; also avoid VCS stamping.
		   (setenv "GOFLAGS" "-mod=vendor -trimpath -buildvcs=false")
		   #t))
      (replace 'build
	       (lambda* (#:key import-path #:allow-other-keys)
		 (with-directory-excursion (string-append "src/" import-path)
					   (invoke "go" "install" "-ldflags=-s -w"
						   "./cmd/nvidia-ctk"
						   "./cmd/nvidia-container-runtime"
						   "./cmd/nvidia-container-runtime-hook"
						   "./cmd/nvidia-cdi-hook"))
		 #t))

      (add-after 'install 'add-runtime-mode-symlinks
		 (lambda* (#:key outputs #:allow-other-keys)
		   (let* ((out (assoc-ref outputs "out"))
			  (bin (string-append out "/bin")))
		     (with-directory-excursion bin
					       (for-each (lambda (name)
							   (when (file-exists? name) (delete-file name))
							   (symlink "nvidia-container-runtime" name))
							 '("nvidia-container-runtime.legacy"
							   "nvidia-container-runtime.cdi"))))
		   #t)))))
    (propagated-inputs (list libnvidia-container))
    (synopsis "Build and run containers leveraging NVIDIA GPUs")
    (description "NVIDIA Container Toolkit (includes nvidia-container-runtime and hooks).")
    (home-page "https://github.com/NVIDIA/nvidia-container-toolkit")
    (license asl2.0)))
