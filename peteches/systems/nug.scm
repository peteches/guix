;; nug.scm — nug.peteches.co.uk using (peteches systems base)
;;
;; Notes:
;; - Hyprland greeter via gtkgreet (from base.scm).
;; - Intel CPU microcode + NVIDIA firmware toggled on via flags.
;; - If you want root-on-LUKS, uncomment the LUKS section and comment the non-LUKS one.
;; - make-base-os should append %base-file-systems internally.

(define-module (peteches systems nug)
  #:use-module (nongnu packages linux)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services certbot)
  #:use-module (gnu packages base)           ; e.g. glibc-locales if you want it
  #:use-module (sops secrets)
  #:use-module (peteches systems base)
  #:use-module (peteches services firewall)
  #:use-module (peteches services comfyui)
  #:use-module (peteches packages colibri)
  #:use-module (peteches services colibri)
  #:use-module (peteches services sillytavern)
  #:use-module (peteches services vllm)
  #:use-module (peteches systems network-mounts)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages rust-apps)
  #:use-module ((gnu packages linux) #:select (linux-libre-headers))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module (guix-science-nonfree packages cuda))

;; Bring service types into scope for any host-specific additions.
(use-service-modules base linux cups desktop networking ssh xorg)

(define %fix-perms-hook
  (with-imported-modules
   (source-module-closure '((ice-9 rdelim)
                            (guix build utils)))
   (program-file
    "generate-cert-key-pem-file"
    #~(begin
        (use-modules (ice-9 rdelim)
                     (guix build utils))

        (define cert "/etc/letsencrypt/live/nug.peteches.co.uk/fullchain.pem")
        (define key  "/etc/letsencrypt/live/nug.peteches.co.uk/privkey.pem")
        (define dst-dir "/home/peteches/.local/share/certs")
        (define dst (string-append dst-dir "/nug.peteches.co.uk.pem"))

        ;; absolute store paths, no PATH reliance
        (define chown #$(file-append coreutils "/bin/chown"))
        (define chmod #$(file-append coreutils "/bin/chmod"))
	(define cat #$(file-append coreutils "/bin/cat"))

        (mkdir-p dst-dir)

	;; Concatenate by streaming, no get-string-all, no invoke keywords
	(call-with-output-file dst
          (lambda (out)
            (call-with-input-file cert (lambda (in) (dump-port in out)))
            (newline out)
            (call-with-input-file key  (lambda (in) (dump-port in out)))))

        ;; owner only, no need to assume "users" group exists
        (invoke chown "-R" "peteches:" dst-dir)
        (invoke chmod "600" dst)))))


(define %comfyui-model-paths
  (plain-file "extra_model_paths.yaml"
              "comfyui:
    base_path: /media/ColdStorage/models/comfyui

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

(operating-system
 (inherit
  (make-base-os
 #:host-name "nug"
 #:kernel linux
 ;; Base will append %base-file-systems — you only give machine-specific mounts here.
 #:firmware (list linux-firmware)
 #:users-extra (list (user-account
		      (name "guix-offload")
		      (comment "Build offload user")
		      (group "users")
		      (system? #t)
		      (home-directory "/var/empty")))
 #:mapped-devices
 (list
  (mapped-device
   (source (uuid "820e9368-484a-4bc0-af58-f3f0c29fe0fa"))
   (target "cryptroot")
   (type luks-device-mapping))
  (mapped-device
   (source (uuid "3d049249-8d28-45a3-bd06-980429edf7b7"))
   (target "ColdStorage")
   (type luks-device-mapping)
   (arguments '(#:key-file "/etc/keys/cold-storage.key")))
  (mapped-device
   (source (uuid "8e15e4a6-a1ac-4638-b9d2-814257363cab"))
   (target "HotStorage")
   (type luks-device-mapping)
   (arguments '(#:key-file "/etc/keys/hot-storage.key")))
  (mapped-device
   (source (uuid "65689173-95c7-4f7a-83ff-7a66cb1c6695"))
   (target "WarmStorage")
   (type luks-device-mapping)
   (arguments '(#:key-file "/etc/keys/warm-storage.key"))))

 #:file-systems
 (list
  (file-system
   (mount-point "/")
   (device "/dev/mapper/cryptroot")	; ← ext4 root
   (type "ext4"))
  (file-system
   (mount-point "/boot")
   (device (uuid "2c8fb9c4-f41d-4415-9540-86b588e91bac" 'ext4))
   (type "ext4"))
  (file-system
   (mount-point "/boot/efi")
   (device (uuid "7222-0EC9" 'fat32)) ; ← EFI system partition
   (type "vfat"))
  (file-system
   (mount-point "/media/ColdStorage")
   (device (uuid "0cd3cea0-2ffc-4bf7-9cd0-91b9bbfa716b" 'ext4))
   (create-mount-point? #t)
   (type "ext4"))
  (file-system
   (mount-point "/media/HotStorage")
   (device (uuid "0b30a1c2-64d8-47ec-bfeb-d6ec47292886" 'ext4))
   (create-mount-point? #t)
   (type "ext4"))
  (file-system
   (mount-point "/media/WarmStorage")
   (device (uuid "2f08c1cc-68cb-47ca-8b1a-e4aff62def08" 'ext4))
   (create-mount-point? #t)
   (type "ext4"))
  scoreplay-cifs-mount)

 ;; Bootloader (UEFI)
 #:bootloader
 (bootloader-configuration
  (bootloader grub-efi-bootloader)
  (targets '("/boot/efi"))
  (keyboard-layout (keyboard-layout "us")))

 ;; Host-specific packages (optional)
 #:extra-packages
 '()

 ;; Feature flags
 #:laptop? #f	       ; nug is a desktop
 #:intel-cpu? #t     ; used for thermald when laptop?; harmless here
 #:with-printing? #f
 #:with-bluetooth? #t
 #:with-nonguix? #t
 #:with-nvidia? #t
 #:with-docker? #t
 #:offload-builds? #f
 ;; Generates nug's age keypair on first boot (/etc/age/keys.pub) — see
 ;; the module comment on with-sops? in peteches/systems/base.scm.
 #:with-sops? #t
 ;; secrets/shared/colibri.yaml carries the colibri API key. It is
 ;; currently encrypted with nug's age key only, NOT the operator PGP key
 ;; every other secret in this repo also carries — that recipient needs
 ;; gpg-agent with the real private key material, which isn't available
 ;; in the environment that generated this file. Run, from a machine that
 ;; does have it:
 ;;   sops updatekeys secrets/shared/colibri.yaml
 ;; secrets/shared/ is for keys used by more than one machine (client or
 ;; server) — add a machine's age key to its .sops.yaml recipient list
 ;; and run `sops updatekeys` again to let it decrypt this too.
 #:sops-secrets
 (list
  (sops-secret
   (key '("api-key"))
   (file (local-file "../../secrets/shared/colibri.yaml"))
   (path "/run/secrets/colibri-api-key")
   (permissions #o400))
  (sops-secret
   (key '("password"))
   (file (local-file "../../secrets/shared/sillytavern.yaml"))
   (path "/run/secrets/sillytavern-password")
   (permissions #o400)))

 ;; Host-only services
 #:extra-services
 (list
  ;; Restored 2026-08-09: deleted in ec83cfe ("Remove unused Tailscale
  ;; and Certbot configurations", 2026-06-05) as apparently-dead code —
  ;; it wasn't unused, %fix-perms-hook's only caller was this service.
  ;; Deleting it silently stopped LE renewal for nug.peteches.co.uk,
  ;; which koboldcpp-rp and sillytavern's SSL both depend on. Renews
  ;; twice daily via the 'certbot-certificate-renewal shepherd timer
  ;; (manually: `sudo herd trigger certbot-certificate-renewal`), plus
  ;; once at boot via 'renew-certbot-certificates. deploy-hook re-runs
  ;; %fix-perms-hook to refresh ~/.local/share/certs/nug.peteches.co.uk.pem.
  (service certbot-service-type
           (certbot-configuration
            (email "certbot@peteches.co.uk")
            (certificates
             (list
              (certificate-configuration
               (domains '("nug.peteches.co.uk"))
               (deploy-hook %fix-perms-hook))))))
  (service comfyui-service-type
           (list
	    (comfyui-configuration
	     (listen "0.0.0.0,::")
	     (extra-model-paths-config %comfyui-model-paths)
	     (runtime-packages (list uv))
	     (open-firewall? #t)
	     (container-extra-shares (list "/media/ColdStorage"))
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
			    #~(string-append "CXXFLAGS=-include "
				     #$(plain-file "comfyui-libstdcxx-c99-shim.h"
					   (string-append
					    "#include <cmath>\n"
					    "#include <math.h>\n"
					    "#include <cstdlib>\n"
					    "#include <stdlib.h>\n"
					    "#include <type_traits>\n"
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
		(git-repo-url "https://github.com/Takenoko3333/comfyui-audio-pitch")))))))
  ;; CUDA-enabled (colibri-engine-cuda, sm_89/Ada — see
  ;; peteches/packages/colibri.scm), capped at vram-gb 12: a static middle
  ;; ground, not a dynamic split — colibri has no live/signal-based VRAM
  ;; reconfiguration (fixed at process start) and this repo has no cross-
  ;; service reactive/watcher pattern to build "colibri yields when ComfyUI
  ;; needs VRAM" on, so a restart-cycling watcher was ruled out as its own
  ;; project that would cost the warm cache it's meant to protect. 12GB +
  ;; the engine's own ~2GB CUDA_RESERVE_GB leaves ComfyUI ~10GB — measured
  ;; via `coli plan`/`coli doctor` to matter: projected_hit_rate was 9.35%
  ;; at the previous 6GB cap vs 24.13% with the full card free. ComfyUI is
  ;; needed alongside colibri specifically for in-roleplay illustrations via
  ;; SillyTavern's image-gen extension, not just separate heavy sessions, so
  ;; some headroom stays reserved rather than maximizing colibri outright.
  ;; Reached via Caddy at
  ;; colibri.ts.peteches.co.uk -> nug.spaniel-cordylus.ts.net:8000 (see
  ;; peteches/systems/caddy.scm); --allowed-host must match that public
  ;; domain because Caddy forwards the client's original Host header
  ;; unchanged, and colibri's DNS-rebinding guard otherwise 403s anything
  ;; that isn't loopback or its own literal bind address.
  ;;
  ;; model-dir lives on HotStorage (NVMe), not ColdStorage (spinning HDD):
  ;; expert streaming is a random-read workload, which a platter disk
  ;; handles catastrophically worse than NVMe.
  ;;
  ;; model-dir and model-mirror both point at a stable "current" symlink
  ;; under models/colibri/ (identical layout on both drives) rather than a
  ;; specific model's directory directly, so switching models is a
  ;; filesystem operation (repoint both symlinks, chown the new targets,
  ;; herd restart) — no guix system reconfigure needed for routine swaps.
  ;; Model directories live as siblings of current, e.g. .../glm-5.2,
  ;; .../deepseek-v4-flash. The config was made to match each drive's
  ;; actual on-disk layout rather than the other way around.
  ;;
  ;; Keeping both symlinks pointing at the SAME model is a manual
  ;; discipline requirement now, not something enforced automatically:
  ;; colibri expects primary and mirror to be byte-identical copies of the
  ;; same model, and there's nothing stopping HotStorage's current and
  ;; WarmStorage's current from being repointed independently and
  ;; silently desyncing. Repoint them together.
  ;;
  ;; direct-io? and pipe? aren't specific to the mirror existing — DIRECT
  ;; (O_DIRECT) is a general NVMe win per upstream's own guidance,
  ;; independent of whether a second copy exists; pipe? overlaps expert
  ;; disk-loads with matmul via I/O worker threads instead of doing them
  ;; sequentially — byte-identical output, purely a scheduling change.
  ;; pipe-workers left at the engine's own default (8) until there's a
  ;; measurement to tune it against.
  (service colibri-service-type
           (colibri-configuration
            (package colibri-engine-cuda)
            (model-dir "/media/HotStorage/models/colibri/current")
            (model-mirror "/media/WarmStorage/models/colibri/current")
            (direct-io? #t)
            (pipe? #t)
            ;; Per-turn latency percentiles, expert-I/O totals, cache-tier
            ;; fill and a tuning verdict on stderr -> colibri.log. Purely
            ;; diagnostic output, no behavioral effect.
            (perf-metrics? #t)
            ;; Both straight from coli plan/doctor's own tuning verdict for
            ;; this deployment: draft-tokens 0 disables MTP speculation,
            ;; which at this cache-hit rate widens the expert union and
            ;; adds disk reads rather than saving time (byte-identical
            ;; output regardless — drafts are always verified, never
            ;; trusted blindly); cuda-pipe? engages the multi-step GPU
            ;; attention pipeline, recommended for this single-GPU box.
            (draft-tokens 0)
            (cuda-pipe? #t)
            (host "0.0.0.0")
            (open-firewall? #t)
            (auto-start? #t)
            (gpu "auto")
            ;; Raised from 12 (originally sized to leave ComfyUI ~10GB
            ;; headroom for concurrent in-roleplay illustrations) to 20:
            ;; `coli doctor' against the current model (Qwen3.6-35B-A3B,
            ;; ~23GB, 41 shards) shows 100% projected expert residency at
            ;; ~18GB, so 20 leaves a few GB of margin on the 24GB card for
            ;; 100% hit rate rather than partial disk streaming. This
            ;; assumes colibri and koboldcpp-qwen are not run concurrently
            ;; (koboldcpp-qwen's auto-start already defaults to #f) --
            ;; revisit if ComfyUI-alongside-colibri usage returns.
            (vram-gb 20)
            ;; Without --auto-tier the engine falls back to a naive cap
            ;; guess (observed: cap=8, apparently confusing Qwen3.6's
            ;; topk=8 for a real budget) that mismatches n_experts=256 and
            ;; disables the GPU tier entirely, running RAM/CPU-resident
            ;; instead (confirmed live: nvidia-smi showed ~428MiB VRAM used
            ;; with this flag missing, vs `coli doctor' projecting 100%
            ;; expert residency at ~18GB when the plan is actually applied).
            (extra-args (list "--auto-tier"))
            ;; Required: colibri refuses to bind beyond localhost without an
            ;; API key (COLI_ALLOW_INSECURE_BIND=1 exists to bypass this,
            ;; deliberately not used — this is reachable over Tailscale via
            ;; Caddy, not just loopback). Decrypted from
            ;; secrets/shared/colibri.yaml by the sops-secrets entry above
            ;; at boot, not a manually-placed file — see #:sops-secrets.
            (api-key-file "/run/secrets/colibri-api-key")
            (allowed-hosts (list "colibri.ts.peteches.co.uk"))))
  ;; vLLM trial for the coding-agent role, as an alternative to
  ;; koboldcpp-qwen (peteches/home/configs/nug.scm): koboldcpp's fork of
  ;; llama.cpp has a bug in how it handles Qwen3.6-35B-A3B's reasoning +
  ;; tool-calling combination together (confirmed live: the model's EOS
  ;; token fires immediately after it closes a `</think>` block, before
  ;; producing the actual answer that's supposed to follow, breaking pi's
  ;; agentic tool-calling flow mid-task). vLLM has first-party "Day-0"
  ;; support for the whole Qwen3.8 family (see vllm.ai/blog/2026-08-12-
  ;; qwen3.8), including native Triton flash-linear-attention kernels and
  ;; a hybrid KV cache manager built specifically for this
  ;; Gated-Attention/Gated-DeltaNet hybrid architecture -- a stronger
  ;; signal than koboldcpp's community fork.
  ;;
  ;; Model swapped to Qwen3.8-27B (dense, not MoE) rather than staying on
  ;; Qwen3.6-35B-A3B: vLLM's available quantized checkpoints for the 35B-
  ;; A3B MoE model are sized for multi-GPU setups (the AWQ-4bit checkpoint
  ;; alone is 24GB -- the whole card, its own model card says a single
  ;; RTX 4090 needs "severe additional quantization or offloading"). The
  ;; dense 27B model's AWQ-INT4 checkpoint is only ~21GB, leaving genuine
  ;; headroom for KV cache + vLLM's own overhead on this card.
  ;;
  ;; auto-start? #f deliberately: not yet confirmed stable, and running
  ;; both this and koboldcpp-qwen simultaneously would exceed the card's
  ;; VRAM (each wants nearly the whole 24GB) -- start whichever one you
  ;; want to use for a given session with `herd start`.
  ;;
  ;; --tool-call-parser qwen3_coder and --reasoning-parser qwen3 straight
  ;; from the ecosystem's documented Qwen3.8 vLLM serving guidance, not
  ;; independently verified against this exact checkpoint yet.
  ;; Runs inside an FHS-emulating container (peteches/services/vllm.scm),
  ;; same rationale as ComfyUI below: vLLM's dependency tree (PyTorch,
  ;; Triton, prebuilt CUDA/PTX binaries bundled in the triton wheel, ...)
  ;; hardcodes FHS assumptions that plain Guix doesn't provide. Confirmed
  ;; live, one at a time, running this WITHOUT a container first:
  ;; "FileNotFoundError: /sbin/ldconfig", "Failed to find C compiler",
  ;; "fatal error: linux/errno.h: No such file or directory", and finally
  ;; "RuntimeError: Cannot find ptxas" even though the bundled ptxas
  ;; binary exists and is executable (running it directly: "No such file
  ;; or directory" -- the classic foreign-ELF-interpreter symptom). The
  ;; container's own baseline packages (c-compiler-package,
  ;; linux-libre-headers, and glibc-for-fhs's real ldconfig/ld-linux) fix
  ;; that whole class of problem at once.
  (service vllm-service-type
           (list
            (vllm-configuration
             (service-name "vllm-code-agent")
             (auto-start? #f)
             (runtime-packages (list uv python))
             (model "casperhansen/mistral-small-24b-instruct-2501-awq")
             (served-model-name "mistral-small-24b-awq")
             ;; Default cache-dir (/var/cache/vllm/...) lives on the root
             ;; filesystem, which has only ~4.6GB free (confirmed live: a
             ;; ~21GB HF download there failed with "OSError: [Errno 28]
             ;; No space left on device"). Redirect to HotStorage, which
             ;; has terabytes free -- same drive colibri's model-dir uses.
             (cache-dir "/media/HotStorage/models/vllm/vllm-code-agent/cache")
             (host "::")
             (port 8002)
             ;; Swapped from Qwen3.8-27B-AWQ-INT4 to this smaller dense
             ;; 24B checkpoint specifically to reclaim KV-cache headroom:
             ;; the previous model's ~19.24GiB weights left only ~1.24GiB
             ;; for KV (13,400-token ceiling); this checkpoint's weights
             ;; (~14.3GiB) should leave several times that. Confirmed
             ;; live: an initial probe at 65536 crash-looped every ~6s --
             ;; "User-specified max_model_len (65536) is greater than the
             ;; derived max_model_len (max_position_embeddings=32768.0 ...
             ;; in model's config.json)" -- this checkpoint's native
             ;; ceiling is 32768, not the 128K some Mistral models offer.
             ;; Set to the model's real max; the point of this swap was
             ;; KV *headroom* at that length, not a longer context.
             (max-model-len 32768)
             (gpu-memory-utilization 0.95)
             ;; No explicit quantization: this checkpoint is casperhansen's
             ;; AWQ conversion; let vLLM auto-detect from config.json as
             ;; with the previous model, to avoid the same
             ;; --quantization-vs-checkpoint-format conflict seen before.
             (trust-remote-code? #t)
             ;; --enforce-eager: kept from the previous model's config --
             ;; the CUDA-graph-capture OOM risk is about total scratch
             ;; memory on this VRAM-constrained card, not specific to the
             ;; old checkpoint, so keeping it disabled by default here too
             ;; until proven unnecessary.
             ;; Dense (non-hybrid) architecture, no reasoning-model
             ;; behavior -- dropped --reasoning-parser qwen3. Tool calling
             ;; uses vLLM's "mistral" parser, matching this model family's
             ;; native tool-call format, not qwen3_coder's.
             ;; --chat-template: confirmed live, this AWQ repackaging's own
             ;; tokenizer_config.json chat_template only handles
             ;; user/system/assistant roles and never renders `tools` into
             ;; the prompt at all -- pi's read/bash/edit/write tools were
             ;; silently invisible to the model regardless of
             ;; --tool-call-parser, so it answered from training data
             ;; instead of refusing or calling a tool. Point at vLLM's own
             ;; upstream examples/tool_chat_template_mistral.jinja (copied
             ;; to /srv/llm/vllm/, already shared into the FHS container
             ;; since it's under uv-project-dir) instead of relying on the
             ;; checkpoint's own template.
             (extra-args (list "--enable-auto-tool-choice"
                                "--tool-call-parser" "mistral"
                                "--chat-template" "/srv/llm/vllm/tool_chat_template_mistral.jinja"
                                "--enforce-eager"))
             ;; HF_HUB_DISABLE_XET: confirmed live, reproduced twice in a
             ;; row -- huggingface_hub's Xet fast-transfer path crashes
             ;; partway through downloading this checkpoint with
             ;; "RuntimeError: Task error: File reconstruction error:
             ;; Internal Writer Error: Background writer channel closed".
             ;; Falls back to plain HTTP downloads instead (slower, but
             ;; this is a one-time ~21GB download cached under HF_HOME
             ;; afterward, not a per-request cost).
             ;; VLLM_USE_FLASHINFER_SAMPLER=0: confirmed live, FlashInfer's
             ;; sampling kernels JIT-compile via nvcc at runtime and this
             ;; container's gcc-toolchain (16.x) is newer than nvcc 12.8's
             ;; supported-host-compiler ceiling (gcc<=14) -- plus a missing
             ;; curand.h, since the full CUDA toolkit headers aren't in
             ;; this container (guix-science-nonfree packages, can't be
             ;; named inside guix shell's base-distribution-only package
             ;; resolution). Falls back to the native PyTorch/Triton
             ;; sampling path instead, which already works.
             (extra-environment-variables
              (list "HF_HUB_DISABLE_XET=1"
                    "VLLM_USE_FLASHINFER_SAMPLER=0"))
             (open-firewall? #t))))
  ;; Verified directly (not just reasoned about): a plain `npm install` on
  ;; this codebase needed no native compilation, and `node server.js`
  ;; served HTTP cleanly — no FHS-container treatment needed here, unlike
  ;; ComfyUI. Port 8001, not upstream's default 8000, to avoid colliding
  ;; with colibri on the same box. Same host-header-guard reasoning as
  ;; colibri: reachable via Caddy over Tailscale, so whitelistMode (IP-
  ;; based) is off in favour of hostWhitelist (Host-header based) + basic
  ;; auth — see peteches/services/sillytavern.scm's module comment.
  (service sillytavern-service-type
           (sillytavern-configuration
            (host "0.0.0.0")
            (open-firewall? #t)
            (basic-auth? #t)
            (basic-auth-password-file "/run/secrets/sillytavern-password")
            (allowed-hosts (list "sillytavern.ts.peteches.co.uk"))))
  (simple-service 'nug-guix-publish-firewall
                  firewall-service-type
                  (nftables-rules
                   (input (list "tcp dport 3000 accept comment \"guix-publish\""))))
  (service guix-publish-service-type
           (guix-publish-configuration
            (host "::")
            (port 3000)
            (compression '(("zstd" 9)))
            (advertise? #t)
            (cache "/var/cache/guix/publish")))
  ;; Trusts claude-workstation's Guix archive signing key so nug's daemon
  ;; accepts store items it sends during offload -- separate from the SSH
  ;; guix-offload key above, which only grants login. Without this, offload
  ;; connects fine but every export fails with "unauthorized public key"
  ;; and silently falls back to a local build.
  (simple-service 'guix-offload-signing-keys
                  guix-service-type
                  (guix-extension
                   (authorized-keys
                    (list (plain-file "claude-workstation-signing-key.pub"
                                      "(public-key \n (ecc \n  (curve Ed25519)\n  (q #EFED7FDADFFF4E2559977AFD10310E21C4EEF7685C6297595D5333CBEF037EDE#)\n  )\n )\n")))))
  (simple-service 'guix-offload-authorized-keys
                  openssh-service-type
                  `(("guix-offload"
                     ,(plain-file "claude-workstation-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIVVAw63bjPSR1Pmm8MixkVZgNYty3IrWbJMyWe7CEVo guix-offload@claude-workstation\n")
                     ,(plain-file "arr-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJCmWvNZoNQhpVpeYU6VXtYcrtS8XfgrK5S5WCs5OtM1 guix-offload@arr\n")
                     ,(plain-file "nyarlothotep.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEP+/uHdoUNfL+LuniZGTEwPJkxvSgDpuR58yxfw/u74 guix-build@nyarlothotep\n")
                     ,(plain-file "caddy-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ5OyvcOFlI3lnunv9FzkOms2CO9i7y12EnSSBDmp6ob guix-offload@caddy\n")
                     ,(plain-file "downloads-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOTVFFGPzZsX0hV4fY2bhptvW1Zs6lilcYMGTOli1UoL guix-offload@downloads\n")
                     ,(plain-file "git-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDP/krMF4ECdoMVIqv9K5mZHvbJUv7+ZFSx2FlVlHSOf guix-offload@git\n")
                     ,(plain-file "grafana-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINw3+hJHHSzwhGquTWRtXx5+uVdvarpu3gJGCnXrj61Q guix-offload@grafana\n")
                     ,(plain-file "jellyfin-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILGGBMFH7Bei/lTu2s4xqFveXOmxOdqHGQiVmnDRfBt5 guix-offload@jellyfin\n")
                     ,(plain-file "loki-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN9R8rSJi1VRa2okQhXFxxBHJXwmV1rVOl8HelpepFVg guix-offload@loki\n")
                     ,(plain-file "pihole-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMNAir7xhAl7Z50tloQKOfCeVPqTqDgmIuSVxtfFdLES guix-offload@pihole\n")
                     ,(plain-file "prometheus-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM+nhIk+ySFYMj7I4SDwA/LKyM8MH3+8NMIabyAIuMSC guix-offload@prometheus\n")
                     ,(plain-file "prowlarr-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPxWQLDvXJGgp4HsOpSEMyLTGi0lL2zYcvRvARuVv/nU guix-offload@prowlarr\n")
                     ,(plain-file "rustdesk-offload.pub" "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII0DTgAJjaG1+0STwTBDRfUrbP/q0KFVnY5OdjrqKasS guix-offload@rustdesk\n")))))))
 ;; peteches gets full passwordless sudo on this desktop, on top of
 ;; make-base-os's normal (password-required) `%wheel ALL=(ALL) ALL' line
 ;; -- requested directly, so this is broader than the usual scoped-command
 ;; pattern; be aware this removes the password prompt as a safety check
 ;; for every sudo invocation by this user, including accidental ones.
 (sudoers-file (plain-file "sudoers"
                           (string-append
                            "root ALL=(ALL) ALL\n"
                            "%wheel ALL=(ALL) ALL\n"
                            "peteches ALL=(ALL) NOPASSWD:ALL\n"
                            "#includedir /run/sudoers.d\n"))))
