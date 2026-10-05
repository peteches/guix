;; comfyui.scm — ComfyUI on a Proxmox QEMU/KVM VM with RTX 4090 GPU passthrough.
;;
;; Successor to nug's desktop-hosted comfyui-service-type — nug itself is
;; being reinstalled as the Proxmox host (proxmox3) this VM runs on, so its
;; GPU-bound services had to move into VMs. Unlike nug, this VM runs two GPU
;; workloads -- ComfyUI and koboldcpp -- which are mutually exclusive by
;; configuration because 24GB of VRAM cannot hold both (see the note on the
;; koboldcpp service below). nug's other GPU services (vllm-code-agent,
;; koboldcpp-rp, sillytavern, colibri) were retired -- the local-LLM
;; experiments didn't pan out, though koboldcpp has since come back as pi's
;; LLM backend -- and nginx/certbot were dropped entirely — comfyui was
;; already reachable with real TLS via the existing caddy VM's
;; `comfyui.ts.peteches.co.uk` reverse-proxy entry (see caddy.scm), which
;; only needs repointing at this VM's hostname.
;;
;; The comfyui-configuration below (custom nodes, CUDA/CMake environment
;; variables, the libstdc++ C99 shim) is copied verbatim from nug.scm's
;; hard-won working config — see git history there for the debugging behind
;; each comment. Only two things changed: the model store moved off
;; ColdStorage (nug's spinning-HDD array, kept intact as backup/personal
;; archive) onto /media/models, a dedicated ext4 volume carved from the new
;; `hot-storage` ZFS pool (nug's former HotStorage NVMe, reformatted after
;; its old colibri/vllm model data — dead weight once those services were
;; retired — was wiped).
;;
;; Bootstrap note: /dev/vdb (the hot-storage-backed volume) must be
;; provisioned and formatted (mkfs.ext4) on the Proxmox side, and the 624G
;; under nug's ColdStorage/models/comfyui copied onto it, BEFORE first boot
;; with this file-systems entry — the mount is (mount-may-fail? #f), so a
;; missing/unformatted disk blocks boot. ColdStorage's copy is left in place
;; afterward as a live backup, not deleted.
;;
;; GPU passthrough (hostpci0) is configured on the Proxmox side (qm), not
;; here — #:with-nvidia? just adds the nonguix driver/CUDA stack so the
;; guest can actually use the card once Proxmox hands it over. Mirrors
;; jellyfin.scm's existing RTX 2060 passthrough on proxmox1.

(define-module (peteches systems comfyui)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module ((gnu packages build-tools) #:select (uv))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain-14))
  #:use-module ((gnu packages linux) #:select (linux-libre-headers))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services comfyui)
  #:use-module (peteches services koboldcpp)
  #:use-module (peteches services restic)
  #:use-module (peteches services tailscale)
  #:use-module (sops secrets)
  #:use-module (guix-science-nonfree packages cuda)   ; cuda-13
  #:export (comfyui-os))

(define %comfyui-model-paths
  (plain-file "extra_model_paths.yaml"
              "comfyui:
    base_path: /media/models/comfyui

    checkpoints: checkpoints/
    clip: clip/
    clip_vision: clip_vision/
    configs: configs/
    controlnet: controlnet/
    diffusion_models: diffusion_models/
    embeddings: embeddings/
    hypernetworks: hypernetworks/
    loras: loras/
    text_encoders: text_encoders/
    upscale_models: upscale_models/
    vae: vae/
    unet: unet/
    ipadapter: ipadapter/
    style_models: style_models/
    photomaker: photomaker/
    gligen: gligen/
    vae_approx: vae_approx/
    insightface: insightface/
    facerestore_models: facerestore_models/
    facedetection: facedetection/
    reactor: reactor/
    sams: sams/
    ultralytics: ultralytics/
    audio_encoders: audio_encoders/
    diffusers: diffusers/
    LLavacheckpoints: LLavacheckpoints/
    TTS: TTS/
"))

(define-public comfyui-os
  (operating-system
   (inherit
    (make-vm-os
     #:host-name "comfyui.peteches.co.uk"
     #:ipv4-address "192.168.51.206/23"
     #:ipv6-address "2a10:d582:ef59::112/64"
     #:bootloader
     (bootloader-configuration
      (bootloader grub-efi-removable-bootloader)
      (targets '("/boot/efi"))
      (keyboard-layout (keyboard-layout "us")))
     #:file-systems
     (list
      (file-system
        (mount-point "/boot/efi")
        (device (file-system-label "GNU-ESP"))
        (type "vfat"))
      (file-system
        (mount-point "/")
        (device "/dev/vda2")
        (type "ext4"))
      ;; hot-storage-backed model volume — see the bootstrap note above.
      (file-system
        (mount-point "/media/models")
        (device "/dev/vdb")
        (type "ext4")
        (create-mount-point? #t)
        (mount-may-fail? #f)))
     ;; restic-config and the guix-offload-key secret are both deliberately
     ;; omitted for now:
     ;;  - restic needs the fleet's real shared SSH key/password (the ones
     ;;    nas.peteches.co.uk already trusts), not freshly generated ones --
     ;;    reusing the real credential needs verifying against an existing
     ;;    host's secret first, not guessing.
     ;;  - guix-offload-key needs an offload keypair whose public half is
     ;;    registered on guix-build.scm, which can't happen until guix-build
     ;;    itself is deployed and reachable. #:with-nug-offload? is also off
     ;;    below for the same reason -- there's nothing to offload to yet.
     ;; Revisit both once guix-build exists.
     #:sops-secrets '()
     #:with-nug-offload? #f
     #:with-nvidia? #t
     ;; RTX 4090 (Ada Lovelace) is well clear of the 590+ pre-Turing GPU
     ;; cutoff that bit dagon's Pascal-based GTX 1060 — see (peteches
     ;; systems vm-base)'s nvidia-packages-for-version.
     #:nvidia-driver-version '595
     #:extra-services
     (list
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches"))))
  (service comfyui-service-type
           (list
	    (comfyui-configuration
	     ;; Manual start only -- ComfyUI and the koboldcpp service below
	     ;; share this VM's single RTX 4090 and 24GB cannot hold both.
	     ;; koboldcpp's live allocations total ~20.4GB (15.1GB weights +
	     ;; 3.7GB KV + 0.6GB recurrent state + 1.0GB compute + ~1.5GB CUDA
	     ;; context), leaving ~1.7GB free; ComfyUI's idle CUDA context alone
	     ;; is 386MB and an SDXL/Flux workflow needs 8-16GB. Both daemons
	     ;; auto-started until 2026-10-04, which put them in silent
	     ;; contention -- no OOM only because no image job had run since.
	     ;; `herd stop koboldcpp; herd start comfyui` for an image session,
	     ;; then the reverse when done.
	     (auto-start? #f)
	     (listen "0.0.0.0,::")
	     (extra-model-paths-config %comfyui-model-paths)
	     (runtime-packages (list uv))
	     ;; VideoHelperSuite's (and comfyui-art-venture's) requirements.txt
	     ;; pulls in opencv-python (non-headless), whose highgui module is
	     ;; linked against GTK/glib even though nothing here ever opens a
	     ;; GUI window — without glib reachable, loading either node pack
	     ;; fails at import with "ImportError: libgthread-2.0.so.0: cannot
	     ;; open shared object file". Confirmed live that adding glib to
	     ;; container-extra-packages does NOT fix this, despite making
	     ;; /lib/libgthread-2.0.so.0 genuinely resolvable via the
	     ;; container's own ldconfig cache (verified with `ldconfig -p`
	     ;; inside the running container's mount namespace) — because the
	     ;; venv's python is a Guix-packaged binary, cv2's dlopen() of its
	     ;; own dependencies resolves through Guix's own glibc loader
	     ;; (whatever loaded the python process itself), which never
	     ;; consults /etc/ld.so.cache or the FHS container's /lib the way
	     ;; a binary with glibc-for-fhs as its ELF interpreter would.
	     ;; LD_LIBRARY_PATH is the only mechanism that actually reaches
	     ;; that loader — same reason CUDA's libraries need it below via
	     ;; the default ld-library-paths value. Overriding the whole field
	     ;; here (rather than extra-environment-variables) avoids emitting
	     ;; two separate "LD_LIBRARY_PATH=" env entries, which is
	     ;; ambiguous — repeats ld-library-paths' own default profile
	     ;; paths since setting this field replaces rather than extends
	     ;; them.
	     (ld-library-paths
	      (list "/run/current-system/profile/lib"
		    "/run/current-system/profile/lib64"
		    #~(string-append #$glib "/lib")))
	     (open-firewall? #t)
	     (container-extra-shares (list "/media/models"))
	     ;; Pinned below the plain gcc-toolchain default (16.1.0 as of this
	     ;; VM's channel pin): torch's cpp_extension build refuses to
	     ;; compile against CUDA 13.0 with a g++ >=16.0 ("The current
	     ;; installed version of g++ (16.1.0) is greater than the maximum
	     ;; required version by CUDA 13.0"), confirmed live -- SageAttention
	     ;; failed this VM's first-boot sync with exactly that error. This
	     ;; must have drifted since nug's original working config (this
	     ;; whole file's comments still say "gcc-toolchain (16.2.0)" was
	     ;; fine there); rather than assume why, pin explicitly the same
	     ;; way the vllm config elsewhere in nug.scm already did for an
	     ;; identical CUDA-compiler-cap problem.
	     (c-compiler-package gcc-toolchain-14)
	     ;; nvcc for SageAttention's build-from-source install, via
	     ;; CUDA_HOME only — NOT container-extra-packages, since guix shell
	     ;; can't resolve a guix-science-nonfree package by name inside the
	     ;; FHS container (see comfyui.scm's container-extra-packages field
	     ;; comment); torch's own nvcc discovery checks CUDA_HOME first, so
	     ;; this direct store-path reference is all it needs, made reachable
	     ;; by the container's existing --expose=/gnu/store.
	     ;;
	     ;; cuda-13 (13.0.2), not the channel's default `cuda' (12.9,
	     ;; colibri-engine-cuda's build-time dependency in
	     ;; peteches/packages/colibri.scm — a separate, unrelated build) —
	     ;; torch's cpp_extension build refuses to compile against a CUDA
	     ;; toolkit that doesn't match the version torch.version.cuda
	     ;; reports for whatever torch wheel `uv sync' resolved for
	     ;; ComfyUI, and that was 13.0 as of the last sync. If a future
	     ;; ComfyUI/torch bump resolves a different CUDA-tagged wheel, this
	     ;; needs to be bumped to match — check
	     ;; /var/log/comfyui/comfyui/update.log's
	     ;; "RuntimeError: ('The detected CUDA version...)" message, which
	     ;; names both versions directly.
	     ;;
	     ;; CPATH is also required, separately: nvcc's own host-preprocessing
	     ;; pass doesn't go through the gcc driver's normal default system
	     ;; include search (confirmed live — a plain `gcc' invocation finds
	     ;; <linux/limits.h> in the FHS-unioned /usr/include fine, but nvcc
	     ;; invoking the same gcc for the CUDA-runtime-header preinclude
	     ;; pass reports "fatal error: linux/limits.h: No such file or
	     ;; directory" for that exact file). CPATH is Guix's own
	     ;; native/non-FHS header-search convention and gets honored
	     ;; regardless of how a build tool invokes the compiler internally,
	     ;; so it fixes this generally for any custom node's C/CUDA build,
	     ;; not just SageAttention's.
	     (extra-environment-variables
	      (list #~(string-append "CUDA_HOME=" #$cuda-13)
		    ;; c-compiler-package above (gcc-toolchain-14) has NO effect
		    ;; on which g++ actually runs: the container resolves it via
		    ;; `guix shell gcc-toolchain' (a bare name -- see
		    ;; comfyui-container-runner-file's package-names, built from
		    ;; package-name, which strips the version), and `guix shell'
		    ;; then picks the highest-versioned "gcc-toolchain" in the
		    ;; distro (16.1.0) regardless of which package object this
		    ;; config referenced. Confirmed live: SageAttention's build
		    ;; still failed against g++ 16.1.0 with c-compiler-package
		    ;; set, identical error to before. CC/CXX pointed at direct
		    ;; store paths sidesteps that resolution entirely, same
		    ;; workaround as CUDA_HOME above and for the same reason
		    ;; (--expose=/gnu/store makes any store path reachable
		    ;; whether or not `guix shell' installed it by name).
		    ;; setuptools/distutils' build_ext honors CC/CXX directly.
		    #~(string-append "CC=" #$(file-append gcc-toolchain-14 "/bin/gcc"))
		    #~(string-append "CXX=" #$(file-append gcc-toolchain-14 "/bin/g++"))
		    ;; CC/CXX alone still isn't enough: cc1plus then searches the
		    ;; container's general include path, which (thanks to the
		    ;; SAME package-names/bare-name issue as c-compiler-package
		    ;; above) contains headers from the plain gcc-toolchain the
		    ;; container installed by name (16.x), not gcc-toolchain-14's
		    ;; own. Confirmed live: "'_GLIBCXX26_CONSTEXPR' does not name
		    ;; a type" in .../profile/include/c++/bits/exception.h --
		    ;; that macro is gated on a libstdc++ release gcc-14 doesn't
		    ;; provide, i.e. gcc-14's binary was compiling gcc-16's
		    ;; headers. CPLUS_INCLUDE_PATH forces its own matching
		    ;; headers ahead of that mismatched set.
		    #~(string-append "CPLUS_INCLUDE_PATH="
				     #$(file-append gcc-toolchain-14 "/include/c++"))
		    #~(string-append "C_INCLUDE_PATH="
				     #$(file-append gcc-toolchain-14 "/include"))
		    #~(string-append "CPATH="
				     #$(file-append linux-libre-headers
						    "/include"))
		    ;; Makes ComfyUI_VLM_nodes' llama-cpp-python source build
		    ;; (see its custom-node entry below) target the CUDA
		    ;; backend instead of GGML's CPU-only default. CMake's
		    ;; FindCUDAToolkit doesn't consult CUDA_HOME (that's
		    ;; torch's own convention) — it needs CUDAToolkit_ROOT
		    ;; explicitly, or `nvcc' fails with "CUDA Toolkit not
		    ;; found" even though CUDA_HOME above is set correctly.
		    ;; CUDAToolkit_ROOT alone finds the toolkit's headers/libs
		    ;; but enable_language(CUDA) still needs the compiler
		    ;; binary itself named explicitly via CMAKE_CUDA_COMPILER
		    ;; (or CUDACXX) — without it CMake reports "No
		    ;; CMAKE_CUDA_COMPILER could be found" despite the
		    ;; toolkit having already been located successfully.
		    #~(string-append "CMAKE_ARGS=-DGGML_CUDA=on -DCUDAToolkit_ROOT="
				     #$cuda-13
				     " -DCMAKE_CUDA_COMPILER="
				     #$(file-append cuda-13 "/bin/nvcc"))
			    ;; llama-cpp-python 0.3.35's vendored llama.cpp source calls a lot
			    ;; of C99 additions via std:: (log2, isfinite/round, lround,
			    ;; strtof/strtold/strtoll/strtoull, ...) across different files
			    ;; (ggml-backend-meta.cpp, llama-vocab.cpp, llama-quant.cpp,
			    ;; imatrix-loader.cpp, mtmd-image.cpp, the vendored
			    ;; nlohmann/json.hpp, ...), all of which fail to compile under
			    ;; this Guix gcc-toolchain (16.2.0) with e.g. "'log2' is not a
			    ;; member of 'std'". This is NOT a missing #include: plain
			    ;; `-include cmath` (tried first) does not fix it, and neither
			    ;; does adding/removing _XOPEN_SOURCE at any value — confirmed
			    ;; directly with `g++ -include cmath -std=gnu++17`, which still
			    ;; fails on `std::log2(x)` even though bare `log2(x)` (the global,
			    ;; non-namespaced symbol from <math.h>) compiles fine. So
			    ;; libstdc++'s own <cmath>/<cstdlib> in this toolchain just don't
			    ;; pull these into namespace std, regardless of inclusion order
			    ;; or feature-test macros. The fix is to force-include a shim that
			    ;; manually re-exports them into std:: — patching the vendored
			    ;; (fetched-at-build-time, unpackaged) source directly isn't an
			    ;; option.
			    ;;
			    ;; For the round/trunc/lround/etc. family, a bare `using ::round;`
			    ;; only imports ONE overload (the double-taking global `::round`),
			    ;; not the float/double/long-double *overload set* C++'s <cmath>
			    ;; is supposed to provide under a single name — nor the additional
			    ;; integral-argument overload (promoting to double) C++11 requires
			    ;; on top of that. Missing the float/double/long-double set, code
			    ;; calling `std::round(some_float)` silently gets the double
			    ;; overload back (implicit float->double conversion), and
			    ;; `std::max(std::round(Cc), 0.0f)` (mtmd-image.cpp) fails with
			    ;; "no matching function for call to max(double, float)" since
			    ;; std::max needs matching argument types. Missing the integral
			    ;; overload, `std::log2(n_devs)` where n_devs is size_t
			    ;; (ggml-backend-meta.cpp) fails with "call ... is ambiguous" —
			    ;; size_t converts equally well to float, double, or long double,
			    ;; so overload resolution can't pick among the three real ones.
			    ;; Real overloaded wrapper functions for float/double/long double,
			    ;; plus a SFINAE-constrained template for any integral argument
			    ;; (casting to double), are needed — not plain `using` re-exports
			    ;; — to actually reproduce what <cmath> provides. Confirmed by
			    ;; compiling the generated header directly against both failure
			    ;; patterns before rolling out.
			    ;;
			    ;; isfinite/isnan/isinf/signbit are a separate case again — NOT
			    ;; plain functions in glibc at all, but type-generic C99 macros
			    ;; (dispatch on argument type), so `using ::isfinite;` is a hard
			    ;; error ("has not been declared in '::'") rather than silently
			    ;; missing: a macro isn't a nameable entity a `using'-declaration
			    ;; can refer to. These get real std:: function templates backed
			    ;; by the corresponding GCC builtins instead.
			    ;;
			    ;; strtof/strtold/strtoll/strtoull/atoll are fine as plain `using`
			    ;; re-exports — those are genuinely single distinct C names (not
			    ;; an overload family sharing one C++ name), called directly by
			    ;; callers with the exact matching argument types.
			    ;; CPLUS_INCLUDE_PATH/C_INCLUDE_PATH (set above) did NOT
			    ;; fix the gcc-14-vs-gcc-16-headers mismatch -- confirmed
			    ;; live, identical "_GLIBCXX26_CONSTEXPR" error, same
			    ;; .../profile/include/c++/bits/exception.h path, even
			    ;; with both set. --emulate-fhs must inject its own
			    ;; -isystem/-I for the container's merged profile ahead
			    ;; of env-var-added paths. CXXFLAGS's existing -include
			    ;; below is proven to actually reach cc1plus's real
			    ;; command line (the shim itself gets force-included
			    ;; successfully), so add -isystem here too rather than
			    ;; trust another environment variable.
			    #~(string-append "CXXFLAGS=-isystem "
					     #$(file-append gcc-toolchain-14 "/include/c++")
					     " -include "
				     #$(plain-file "comfyui-libstdcxx-c99-shim.h"
					   (string-append
					    "#include <cmath>\n"
					    "#include <math.h>\n"
					    "#include <cstdlib>\n"
					    "#include <stdlib.h>\n"
					    "#include <type_traits>\n"
					    ;; Guarded: this shim exists because gcc-toolchain's
					    ;; default (16.x as of this comment) doesn't pull
					    ;; these C99 names into std:: -- see the comment
					    ;; above extra-environment-variables. gcc-toolchain-14
					    ;; (pinned via CC/CXX above, for the separate g++
					    ;; version-cap problem) already provides all of these
					    ;; natively, so force-including this unconditionally
					    ;; collides with real definitions ("redefinition of
					    ;; 'float std::log2(float)'" etc, confirmed live)
					    ;; instead of filling a gap. __GNUC__ >= 15 keeps the
					    ;; shim working if this file's CXX is ever pointed
					    ;; back at a newer default gcc-toolchain.
					    "#if __GNUC__ >= 15\n"
					    "namespace std {\n"
					    "inline float       log2(float x) { return ::log2f(x); }\n"
					    "inline double      log2(double x) { return ::log2(x); }\n"
					    "inline long double log2(long double x) { return ::log2l(x); }\n"
					    "inline float       exp2(float x) { return ::exp2f(x); }\n"
					    "inline double      exp2(double x) { return ::exp2(x); }\n"
					    "inline long double exp2(long double x) { return ::exp2l(x); }\n"
					    "inline float       round(float x) { return ::roundf(x); }\n"
					    "inline double      round(double x) { return ::round(x); }\n"
					    "inline long double round(long double x) { return ::roundl(x); }\n"
					    "inline float       trunc(float x) { return ::truncf(x); }\n"
					    "inline double      trunc(double x) { return ::trunc(x); }\n"
					    "inline long double trunc(long double x) { return ::truncl(x); }\n"
					    "inline float       nearbyint(float x) { return ::nearbyintf(x); }\n"
					    "inline double      nearbyint(double x) { return ::nearbyint(x); }\n"
					    "inline long double nearbyint(long double x) { return ::nearbyintl(x); }\n"
					    "inline float       rint(float x) { return ::rintf(x); }\n"
					    "inline double      rint(double x) { return ::rint(x); }\n"
					    "inline long double rint(long double x) { return ::rintl(x); }\n"
					    "inline float       cbrt(float x) { return ::cbrtf(x); }\n"
					    "inline double      cbrt(double x) { return ::cbrt(x); }\n"
					    "inline long double cbrt(long double x) { return ::cbrtl(x); }\n"
					    "inline float       expm1(float x) { return ::expm1f(x); }\n"
					    "inline double      expm1(double x) { return ::expm1(x); }\n"
					    "inline long double expm1(long double x) { return ::expm1l(x); }\n"
					    "inline float       log1p(float x) { return ::log1pf(x); }\n"
					    "inline double      log1p(double x) { return ::log1p(x); }\n"
					    "inline long double log1p(long double x) { return ::log1pl(x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, double>::type log2(T x) { return ::log2((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, double>::type exp2(T x) { return ::exp2((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, double>::type round(T x) { return ::round((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, double>::type trunc(T x) { return ::trunc((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, double>::type nearbyint(T x) { return ::nearbyint((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, double>::type rint(T x) { return ::rint((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, double>::type cbrt(T x) { return ::cbrt((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, double>::type expm1(T x) { return ::expm1((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, double>::type log1p(T x) { return ::log1p((double)x); }\n"
					    "inline long lround(float x) { return ::lroundf(x); }\n"
					    "inline long lround(double x) { return ::lround(x); }\n"
					    "inline long lround(long double x) { return ::lroundl(x); }\n"
					    "inline long long llround(float x) { return ::llroundf(x); }\n"
					    "inline long long llround(double x) { return ::llround(x); }\n"
					    "inline long long llround(long double x) { return ::llroundl(x); }\n"
					    "inline long lrint(float x) { return ::lrintf(x); }\n"
					    "inline long lrint(double x) { return ::lrint(x); }\n"
					    "inline long lrint(long double x) { return ::lrintl(x); }\n"
					    "inline long long llrint(float x) { return ::llrintf(x); }\n"
					    "inline long long llrint(double x) { return ::llrint(x); }\n"
					    "inline long long llrint(long double x) { return ::llrintl(x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, long>::type lround(T x) { return ::lround((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, long long>::type llround(T x) { return ::llround((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, long>::type lrint(T x) { return ::lrint((double)x); }\n"
					    "template<typename T> inline typename std::enable_if<std::is_integral<T>::value, long long>::type llrint(T x) { return ::llrint((double)x); }\n"
					    "inline float       copysign(float x, float y) { return ::copysignf(x, y); }\n"
					    "inline double      copysign(double x, double y) { return ::copysign(x, y); }\n"
					    "inline long double copysign(long double x, long double y) { return ::copysignl(x, y); }\n"
					    "using ::strtof; using ::strtold;\n"
					    "using ::strtoll; using ::strtoull;\n"
					    "using ::atoll;\n"
					    "template<typename T> inline bool isfinite(T x) { return __builtin_isfinite(x); }\n"
					    "template<typename T> inline bool isnan(T x) { return __builtin_isnan(x); }\n"
					    "template<typename T> inline bool isinf(T x) { return __builtin_isinf(x); }\n"
					    "template<typename T> inline bool signbit(T x) { return __builtin_signbit(x); }\n"
					    "}\n"
					    "#endif\n"
					   )))))
	     ;; Package installed but the global --use-sage-attention flag is
	     ;; deliberately left off (enable-sage-attention? default #f) — a
	     ;; per-workflow node (e.g. KJNodes' "Patch Sage Attention") gives
	     ;; finer control and is what video-model wrappers actually expect;
	     ;; see peteches/services/comfyui.scm's install-sage-attention?
	     ;; field comment.
	     (install-sage-attention? #t)
	     (custom-nodes
	      (list
	       ;; KJNodes — includes the "Patch Sage Attention" node used to
	       ;; apply SageAttention per-workflow instead of via the global
	       ;; --use-sage-attention flag (deliberately left off above).
	       (comfyui-custom-node
		(name "ComfyUI-KJNodes")
		(git-repo-url "https://github.com/kijai/ComfyUI-KJNodes"))
	       ;; rgthree-comfy — workflow QoL: Power Lora Loader, Fast Groups
	       ;; Bypasser/Muter, context/reroute nodes.
	       (comfyui-custom-node
		(name "rgthree-comfy")
		(git-repo-url "https://github.com/rgthree/rgthree-comfy"))
	       ;; VideoHelperSuite — VHS_LoadVideo/VHS_VideoCombine load/encode
	       ;; nodes used by the video workflows alongside KJNodes.
	       (comfyui-custom-node
		(name "ComfyUI-VideoHelperSuite")
		(git-repo-url "https://github.com/Kosinkadink/ComfyUI-VideoHelperSuite"))
	       ;; Frame-Interpolation — RIFE/FILM/etc. interpolation nodes for
	       ;; upsampling VideoHelperSuite's frame sequences to a higher
	       ;; output frame rate.
	       (comfyui-custom-node
		(name "ComfyUI-Frame-Interpolation")
		(git-repo-url "https://github.com/Fannovel16/ComfyUI-Frame-Interpolation"))
	       ;; Spectrum: training-free diffusion acceleration via cached/
	       ;; forecasted denoiser features, one repo per model backend.
	       (comfyui-custom-node
		(name "ComfyUI-Spectrum-Proper")
		(git-repo-url "https://github.com/xmarre/ComfyUI-Spectrum-Proper"))
	       (comfyui-custom-node
		(name "ComfyUI-Spectrum-SDXL-Proper")
		(git-repo-url "https://github.com/xmarre/ComfyUI-Spectrum-SDXL-Proper"))
	       (comfyui-custom-node
		(name "ComfyUI-Spectrum-WAN-Proper")
		(git-repo-url "https://github.com/xmarre/ComfyUI-Spectrum-WAN-Proper"))
	       (comfyui-custom-node
		(name "ComfyUI-Spectrum-MiniMax-H3")
		(git-repo-url "https://github.com/xmarre/ComfyUI-Spectrum-MiniMax-H3"))
	       ;; LoadImageFromUrl / LoadVideoFromUrl.
	       (comfyui-custom-node
		(name "comfyui-art-venture")
		(git-repo-url "https://github.com/sipherxyz/comfyui-art-venture"))
	       ;; Local GGUF LLM loader for MiniMax H3 / Qwen-Image prompt
	       ;; writing: LLMLoader runs llama.cpp in-process (no external
	       ;; server), feeding LLMSampler/LLMPromptGenerator/
	       ;; CreativeArtPromptGenerator/Suggester. VLMJSONExtract (same
	       ;; pack) parses Suggester's raw JSON output. llama-cpp-python
	       ;; itself is split into requirements-llama-cpp.txt (not
	       ;; requirements.txt) so a plain install doesn't pull in a
	       ;; from-source compile by default — opt in via
	       ;; extra-requirements-files. CMAKE_ARGS below (in
	       ;; extra-environment-variables) makes that source build target
	       ;; CUDA instead of the GGML CPU backend.
	       (comfyui-custom-node
		(name "ComfyUI_VLM_nodes")
		(git-repo-url "https://github.com/gokayfem/ComfyUI_VLM_nodes")
		(extra-requirements-files (list "requirements-llama-cpp.txt")))
	       ;; MiniMax H3 "Plan v2" prompt-building nodes (Project Setup,
	       ;; Reference/Shot nodes, Prompt Merge, Apply Reference Plan,
	       ;; Qwen enhancement) used by the H3-enhanced-prompt workflow.
	       ;; No Python dependencies; requires ComfyUI's native H3 nodes
	       ;; (Image/Reference to Video, tokenizer), already present.
	       (comfyui-custom-node
		(name "ComfyUI-MiniMax-H3-Guide")
		(git-repo-url "https://github.com/ethanfel/ComfyUI-MiniMax-H3-Guide"))
	       ;; H3QwenVLGenerateText / H3QwenVLGenerationTailLoader — the
	       ;; other pack the H3-enhanced-prompt workflow actually needed
	       ;; (confirmed via ComfyUI's own "Missing Node Packs" dialog,
	       ;; not name-matching). Reconnects MiniMax H3's truncated
	       ;; Qwen3-VL-32B text encoder with its published generation
	       ;; tail for standalone local text/VLM generation. No Python
	       ;; dependencies (pyproject.toml declares none); generation
	       ;; tail model files go under models/text_encoders separately.
	       (comfyui-custom-node
		(name "ComfyUI-H3-Qwen3VL-TextGen")
		(git-repo-url "https://github.com/ethanfel/ComfyUI-H3-Qwen3VL-TextGen"))
	       ;; Voice generation: Qwen3-TTS VoiceDesign (unique voice from a
	       ;; text description) feeding ChatterBox's zero-shot voice
	       ;; conversion (clone that sample onto our own performance
	       ;; recordings, no per-voice training). Model files go under
	       ;; the TTS category registered in %comfyui-model-paths above.
	       ;; extra-pip-packages: OmniVoice's `omnivoice' PyPI package is
	       ;; deliberately left out of TTS-Audio-Suite's own
	       ;; requirements.txt ("We install official OmniVoice separately
	       ;; to avoid letting pip reshape shared deps") — upstream's own
	       ;; install.py installs it standalone with --no-deps, which our
	       ;; sync service doesn't run. Without this, OmniVoice's engine
	       ;; node fails at runtime with "ModuleNotFoundError: No module
	       ;; named 'omnivoice'" — hit this trying OmniVoice's documented
	       ;; "british accent" instruct attribute (k2-fsa/OmniVoice model
	       ;; card), the one engine in this suite with real trained accent
	       ;; control rather than delivery/tone-only instructions.
	       (comfyui-custom-node
		(name "TTS-Audio-Suite")
		(git-repo-url "https://github.com/diodiogod/TTS-Audio-Suite")
		(extra-pip-packages (list "omnivoice")))
	       ;; Semitone pitch-shift for layered voice-design mixes (e.g. a
	       ;; tritone doubling of a TTS Text render before AudioMerge) —
	       ;; TTS-Audio-Suite's own pitch control lives inside its RVC
	       ;; engine and requires a pretrained per-voice .pth model, which
	       ;; doesn't exist for a freshly-designed voice. Lightweight,
	       ;; pure-CPU phase vocoder — no torchaudio/ML dependency.
	       (comfyui-custom-node
		(name "comfyui-audio-pitch")
		(git-repo-url "https://github.com/Takenoko3333/comfyui-audio-pitch"))
	       ;; Krea2EditGroundedEncode / Krea2EditModelPatch — two-reference
	       ;; grounded image editing (subject + outfit/element references,
	       ;; plain-English instruction) for the KREA 2 Identity Edit
	       ;; workflow/LoRA. No pip dependencies (manifest.yaml: pip: []).
	       (comfyui-custom-node
		(name "comfyui-krea2edit")
		(git-repo-url "https://github.com/lbouaraba/comfyui-krea2edit"))
	       ;; Impact Pack — SAMLoader/SAMDetector, BboxDetectorSEGS/
	       ;; SegmDetectorSEGS, SegsToCombinedMask: the box/segm→mask
	       ;; detection-to-mask plumbing used by detailer/inpaint workflows.
	       (comfyui-custom-node
		(name "ComfyUI-Impact-Pack")
		(git-repo-url "https://github.com/ltdrdata/ComfyUI-Impact-Pack"))
	       ;; Impact Subpack — UltralyticsDetectorProvider (and
	       ;; ONNXDetectorProvider): loads the actual YOLO/ONNX detector
	       ;; weight file Impact Pack's detector nodes need.
	       (comfyui-custom-node
		(name "ComfyUI-Impact-Subpack")
		(git-repo-url "https://github.com/ltdrdata/ComfyUI-Impact-Subpack")))))))
      ;; The primary workload on this VM, and the sole LLM backend for
      ;; every pi session on claude-workstation (all three accounts, via
      ;; configs/pi/defaults/models.json -> Caddy's
      ;; koboldcpp.ts.peteches.co.uk). Was declared auto-start? #f for
      ;; "ad-hoc GPU-context experiments" but has in practice run
      ;; continuously (11 days uptime, observed 2026-10-04), so it now
      ;; auto-starts -- otherwise, with ComfyUI also manual-start above, a
      ;; reboot would leave this VM serving nothing.
      ;;
      ;; Shares this VM's single RTX 4090 with ComfyUI, which is therefore
      ;; manual-start: the card cannot hold both. See the
      ;; comfyui-configuration comment above for the VRAM arithmetic and
      ;; the stop/start dance.
      ;;
      ;; model-name is huihui-ai's abliterated (uncensored) build of the
      ;; same Qwen3.8-27B checkpoint below, swapped in for the stock one --
      ;; same base model and quant tier (UD-DW Q4_K_M, 16.6GB vs. the
      ;; original Q4_K_M's 17.77GB). Headroom has now been re-checked
      ;; against this file (load log 2026-09-28 22:00:47): it is resident at
      ;; 15,092.05 MiB -- 1,164 MiB lighter than the original's 16,256.70
      ;; MiB -- and still offloads 66/66 layers, so the tuning below holds.
      ;;
      ;; model-name points at a stable "current-model.gguf" symlink inside
      ;; model-path rather than a specific checkpoint filename, so swapping
      ;; between the coding tune above and an RP tune (e.g.
      ;; Huihui-Qwen3.5-27B-abliterated) is a plain `ln -sfn <file>
      ;; current-model.gguf` on the VM followed by `herd restart koboldcpp`
      ;; -- no Guix reconfigure/redeploy needed to change checkpoints, only
      ;; to change the flags below (context-size/extra-args) if a different
      ;; checkpoint needs different tuning.
      ;;
      ;; --gpulayers 999 gives full GPU offload (66/66 layers per the load
      ;; log). The ~43 tokens/s single-stream figure measured against the
      ;; original coding checkpoint still holds, and is both larger-context
      ;; and faster than the vLLM deployment of the AWQ-INT4 quant of the
      ;; same model previously tried on nug (see that host's git history
      ;; before its retirement) -- thanks to this GGUF quant's smaller
      ;; footprint (17.77GB vs. 19.24GB) and to llama.cpp maintaining real
      ;; per-token KV cache for only 16 of this hybrid Gated-DeltaNet
      ;; architecture's 64 layers (the other 48 use the much cheaper
      ;; fixed-size recurrent state instead -- confirmed live in
      ;; koboldcpp's own load log).
      ;;
      ;; Measured 2026-10-04 across the week's 12,739 requests, generation
      ;; speed degrades ~3.5x over the context range. That, not VRAM, is
      ;; the real cost of a large window, and is why context-size below is
      ;; sized to observed demand:
      ;;   0-16k: 87.3 T/s   64k: 62.1 T/s   128k: 44.8 T/s   176k+: 24.8
      ;;   32k:   70.9 T/s   96k: 54.6 T/s   160k: 36.6 T/s
      (service koboldcpp-service-type
               (koboldcpp-configuration
                (service-name "koboldcpp")
                (auto-start? #t)
                (model-path "/media/models/koboldcpp-test")
                (model-name "current-model.gguf")
                (host "0.0.0.0")
                (port 5001)
                ;; 262144 = this checkpoint's n_ctx_train (confirmed live in
                ;; koboldcpp's own load log) -- the model's full native
                ;; context, and a hard ceiling: going past it would be RoPE
                ;; extrapolation beyond what the model was trained for.
                ;;
                ;; Chosen so usable prompt space is 131072, because
                ;; koboldcpp subtracts --genlimit from the context window
                ;; rather than adding to it: 163840 - 32768 = 131072.
                ;; Confirmed live -- under the 147456/16384 pairing below,
                ;; received prompt sizes topped out at p99 = 131,072
                ;; exactly, i.e. contextsize - genlimit.
                ;;
                ;; 131072 usable is sized to demand, not to VRAM: over 1,678
                ;; pi requests demand measured p50 59,167 / p90 108,258 /
                ;; p95 115,897 / p99 132,020 / max 208,782, so it covers p95
                ;; with 13% headroom and lands within 1% of p99, and only 19
                ;; requests (1.1%) context-shift rather than fit whole. It
                ;; also removed the pathology this was tuned for: the old
                ;; 196608/32768 pairing left 163,840 usable while clients
                ;; were told 196,608, producing a 924-request pile-up at
                ;; 163,925-163,984 that burned 31.4 GPU-hours of re-prefill
                ;; in a week (45% of all request time, from 7.4% of
                ;; requests). After the fix the most repeated CtxLimit value
                ;; occurred twice.
                ;;
                ;; contextsize was first cut to 147456 alongside --genlimit
                ;; 16384, which gave the same 131072 usable prompt but
                ;; starved generation -- see the --genlimit note below. It
                ;; is 163840 rather than 147456 so the generation budget can
                ;; return to 32768 without giving up any prompt space.
                ;;
                ;; The binding constraint is host RAM, not VRAM. koboldcpp's
                ;; SmartCache keeps KV snapshots in RAM and, for this hybrid
                ;; recurrent model, the C++ "RNN Lifeboat" path runs it
                ;; whether --smartcache is passed or not, across 7 slots at
                ;; ~18.65 KB per token of live context. Before the fix that
                ;; reached 20.9 GB against a 19.5 GB VM and the box thrashed
                ;; (koboldcpp VmSwap 9.5 GB, 49 MiB RAM available, vmstat
                ;; si/so to 59 MB/s, PSI memory full avg300=13.36 -- 13% of
                ;; all wall-clock time stalled on memory). What actually
                ;; cleared it was pi compacting at 131072 instead of driving
                ;; sessions into the wall: afterwards PSI was 0.00, si 0,
                ;; VmSwap 2.8 GB and the live slots 8.0 GB, slot size being
                ;; bounded by tokens in use rather than by contextsize.
                ;;
                ;; VRAM cost of 163840 over 147456 is ~585 MiB (KV is
                ;; 19.13 KiB/token: 17.998 across the 16 real-KV layers plus
                ;; 1.125 for the 1-layer MTP draft cache), comfortably inside
                ;; the ~3.1 GB the smaller window freed. Must be kept in step
                ;; with contextWindow in configs/pi/defaults/models.json
                ;; (= context-size minus --genlimit) or pi compacts against a
                ;; window the server cannot actually accept.
                (context-size 163840)
                (gpu-layers 999)
                (cuda-device "0")
                ;; Caddy's koboldcpp.ts.peteches.co.uk reverse proxy
                ;; (peteches/systems/caddy.scm) reaches this over Tailscale,
                ;; so the port must be open -- was missing, leaving the
                ;; service unreachable despite running.
                (open-firewall? #t)
                (extra-args
                 (list
                  ;; q4_0 KV quantization (flash attention is on by
                  ;; default in this koboldcpp version, which quantkv
                  ;; requires for both K and V rather than K only).
                  ;; Confirmed live: this quant level loads and serves the
                  ;; full 262144-token native context with ~815MB VRAM
                  ;; headroom to spare on this RTX 4090 (measured against
                  ;; the heavier original checkpoint; the lighter
                  ;; abliterated one leaves more), and passed a
                  ;; 3-needle retrieval test (facts planted at 10/50/90%
                  ;; depth in a ~216k-token prompt, all recalled verbatim
                  ;; via koboldcpp's OpenAI-compatible API) with no
                  ;; degradation vs. the fp16 baseline. q8_0 also passed
                  ;; the same test but only reaches 155648 tokens before
                  ;; hitting the same VRAM ceiling -- q4_0 gets the whole
                  ;; native context with more headroom, not less, so
                  ;; there's no reason to settle for q8_0 here. (That
                  ;; 155648 figure predates the lighter abliterated
                  ;; checkpoint; q8_0 would now reach further, but with no
                  ;; measured retrieval gain over q4_0 it isn't worth the
                  ;; halved context.)
                  "--quantkv" "q4_0"
                  ;; Server-side backstop: pi (the coding-agent client)
                  ;; was found requesting completions sized to fill
                  ;; whatever context room remained (seen live: a
                  ;; 36,409-token generation request), which reads as
                  ;; "stuck" since it just grinds toward that oversized
                  ;; target instead of stopping at a normal reply length.
                  ;; Fixed client-side in configs/pi/defaults/models.json
                  ;; (maxTokens decoupled from contextWindow), but cap it
                  ;; here too so any client misbehaving the same way
                  ;; can't do it again. 8192 was too tight in practice --
                  ;; it clipped a legitimate long-form task mid-generation
                  ;; ("stopReason": "length" at exactly 8192, confirmed
                  ;; live in a pi session doing a 35-item audit writeup).
                  ;;
                  ;; Cut to 16384 on 2026-10-04 and reverted the next day
                  ;; -- the same failure mode the 8192 note above records.
                  ;; The case for cutting it was that this budget is
                  ;; reserved *inside* --contextsize (so it shrinks usable
                  ;; prompt space) and that measured output length is p50 91
                  ;; / p90 1,305 / p95 1,804 / p99 6,993, with only 12 of
                  ;; 1,678 generations exceeding 16384. Both measurements
                  ;; were right and the conclusion was wrong: that ~0.7%
                  ;; tail is exactly the work that has to finish in one
                  ;; pass, so capping it does not remove long generations,
                  ;; it converts successful ones into visible failures.
                  ;; Post-deploy, 8 of 944 requests (0.85%) were cut at
                  ;; precisely 16,384 output tokens with 60-77k of context
                  ;; still free, which pi surfaces as "Response was
                  ;; truncated before completion" -- roughly a 9x rise on
                  ;; the 0.6% rate at 32768. Do not retune this from output
                  ;; length percentiles alone.
                  ;;
                  ;; Back to 32768, with the prompt-space cost paid by
                  ;; raising --contextsize above instead, so usable prompt
                  ;; stays at 131072 and nothing is traded away. Must be
                  ;; kept in step with maxTokens in
                  ;; configs/pi/defaults/models.json.
                  "--genlimit" "32768"
                  ;; Tool calls were coming back as garbled pseudo-XML text
                  ;; (mismatched <function=bash>...</parameter> tags) that
                  ;; pi's client can't parse, instead of a real structured
                  ;; tool call -- reads as the session just sitting there
                  ;; inert. Root cause per `koboldcpp --help`: plain
                  ;; --jinja explicitly does NOT apply to tool calls
                  ;; ("Tool calls are done without jinja"); koboldcpp falls
                  ;; back to its own generic tool-call formatting, which
                  ;; this Qwen checkpoint wasn't trained to match.
                  ;; --jinja_tools routes tool-call formatting through the
                  ;; model's actual GGUF-embedded Jinja template instead.
                  "--jinja_tools"
                  ;; Speculative decoding via the checkpoint's own MTP
                  ;; head -- no draft model needed. `koboldcpp --analyze`
                  ;; on this GGUF reports qwen35.nextn_predict_layers = 1
                  ;; with a complete block at blk.64 (nextn.eh_proj,
                  ;; enorm, hnorm, shared_head_norm plus its own attn/ffn
                  ;; tensors), and the load log already reports "offloaded
                  ;; 66/66 layers" -- so those weights were resident and
                  ;; idle, since usemtp defaults to off.
                  ;;
                  ;; NOTE: this forecloses --parallelrequests, and
                  ;; deliberately so. koboldcpp.py:2381 refuses continuous
                  ;; batching outright when usemtp (or draftmodel) is set,
                  ;; and gpttype_adapter.cpp:4873 requires !draft_ctx,
                  ;; which --usemtp creates. No real loss: koboldcpp.py:2393
                  ;; also refuses batching for any request carrying an
                  ;; OpenAI tools array (set in transform_genparams at
                  ;; :4430, which runs before the eligibility check at
                  ;; :7311), which is all of pi's traffic. Batching would
                  ;; additionally force --noshift (koboldcpp.py:2002) and
                  ;; stall non-batchable requests behind in-flight ones.
                  "--usemtp"
                  ;; nextn_predict_layers = 1, so the head is trained one
                  ;; token ahead and koboldcpp loops it autoregressively
                  ;; (gpttype_adapter.cpp:968) -- acceptance decays on each
                  ;; extra drafted token, so the default 4 over-drafts.
                  ;; This also sets n_rs_seq (gpttype_adapter.cpp:3227),
                  ;; scaling the 150MB recurrent-state buffer by roughly
                  ;; 1+N, so a lower value is cheaper in VRAM too.
                  "--draftamount" "3"
                  ;; Tried --reasoningeffort low, then --jinjathink false +
                  ;; --jinja_kwargs '{"enable_thinking":false}', to bound/
                  ;; disable thinking server-side -- neither is actually
                  ;; needed. Reasoning-length control belongs in the
                  ;; client instead: koboldcpp Lite's own Settings > Tokens
                  ;; > Thinking > Reasoning Effort actively force-closes
                  ;; the <think> block once its budget is hit (rather than
                  ;; hoping the model honors a hint) and sends
                  ;; "reasoning_effort" per-request, which per koboldcpp's
                  ;; own docs overrides any server-side default anyway --
                  ;; so a server flag here would at best be redundant, and
                  ;; at worst (the enable_thinking:false path) forces
                  ;; thinking off entirely for both the RP and coding
                  ;; checkpoints with no per-session override.
                  ))))
      (service alloy-service-type
               (alloy-configuration
                (hostname "comfyui.peteches.co.uk")
                (log-files (list (cons "/var/log/messages" "syslog")
                                 (cons "/var/log/prometheus-node-exporter.log" "node-exporter")
                                 (cons "/var/log/ntpd.log" "ntpd")
                                 (cons "/var/log/alloy.log" "alloy")
                                 (cons "/var/log/tailscaled-*.log" "tailscale")
                                 (cons "/var/log/comfyui/comfyui/update.log" "comfyui"))))))))))

comfyui-os
