;; comfyui.scm — ComfyUI on a Proxmox QEMU/KVM VM with RTX 4090 GPU passthrough.
;;
;; Successor to nug's desktop-hosted comfyui-service-type — nug itself is
;; being reinstalled as the Proxmox host (proxmox3) this VM runs on, so its
;; GPU-bound services had to move into VMs. Unlike nug, this VM runs two GPU
;; workloads -- ComfyUI and koboldcpp -- which are mutually exclusive by
;; configuration because 24GB of VRAM cannot hold both: since 2026-11-07 ComfyUI
;; auto-starts and koboldcpp is manual-start, so a boot leaves the card with a
;; full VRAM budget for image work instead of the 2GB remainder it had when
;; koboldcpp auto-started (see the note on the koboldcpp service below). A
;; third GPU workload joined on 2026-11-13: AllTalk TTS (XTTS v2, ~4GB
;; resident) auto-starts alongside ComfyUI on the 4090 -- small enough to
;; share, unlike koboldcpp; starting koboldcpp now means stopping one of the
;; other two. nug's
;; other GPU services (vllm-code-agent,
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
;; GPU passthrough is configured on the Proxmox side (qm/terraform), not here
;; — this file only supplies what the *guest* needs to use the cards once
;; Proxmox hands them over.  Two GPUs are passed through to VM 120:
;;
;;   hostpci0  01:00  NVIDIA RTX 4090  — driven by the nonguix NVIDIA stack
;;                                      (#:with-nvidia? below).  Shared by
;;                                      ComfyUI and koboldcpp.
;;   hostpci1  09:00  Intel Arc B70    — Battlemage [8086:e223], driven by the
;;                                      in-tree `xe' driver (#:with-intel-gpu?).
;;
;; Both declared in infra/terraform/main.tf's `comfyui' module; hostpci1 was
;; specced there but never actually applied to Proxmox until 2026-10-27.
;;
;; The B70 is the reason this VM runs the NONFREE kernel (see #:kernel below).
;; linux-libre deblobs firmware filenames out of in-tree drivers, so its xe.ko
;; declares zero MODULE_FIRMWARE entries and GuC loading fails with -ENOENT —
;; the card enumerates over PCI but never binds, leaving no /dev/dri node.
;; nonguix's `linux' retains them (41 firmware entries in xe.ko at 7.1.8).
;; This does not disturb the 4090: nvidia.ko is out-of-tree and Guix rebuilds
;; it against whichever kernel the OS selects (package-for-kernel in
;; gnu/services.scm), which is the same combination dagon already runs.

(define-module (peteches systems comfyui)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  ;; Canonical home of `uv' at the pinned guix commit (b13c7c0) is
  ;; (gnu packages build-tools); (gnu packages rust-apps) only carries a
  ;; deprecated alias for it, and importing from there makes every expansion
  ;; site emit `'uv' is deprecated, use '(@ (gnu packages build-tools) uv)'
  ;; instead' under a forced compile-file.  Verified at c947335: `(gnu
  ;; packages build-tools)' resolves and carries uv 0.10.12
  ;; (gnu/packages/build-tools.scm:1840), so this is the canonical import the
  ;; deprecation message itself names.
  ;; The load failure seen here historically was not the import but a drifted
  ;; local guix (64d4de2, where the package itself moved to
  ;; rust-apps) -- fixed by `guix pull -C peteches/channels/manual.scm`, which
  ;; also brought the sops/guix-science/nonguix channels this file needs.
  ;; Use the profile guix, not the system one: PATH here resolves to
  ;; /run/current-system/profile/bin/guix, still at the off-pin commit.
  #:use-module ((gnu packages build-tools) #:select (uv))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain-14))
  #:use-module ((gnu packages linux) #:select (linux-libre-headers))
  ;; Nonfree kernel — required by #:with-intel-gpu?, see the header note.
  ;; Uses the nonguix channel default `linux' (the latest nonfree kernel). Its
  ;; in-tree `xe' driver retains the firmware entries the B70 needs AND
  ;; enumerates the Arc Pro B70 (0x8086:0xe223, Battlemage): the xe driver
  ;; binds the card and presents a /dev/dri render node. Do NOT pin this to an
  ;; older kernel (e.g. linux-6.12) — the 6.12 xe driver's device table does
  ;; not include 0xe223, so the B70 is detected over PCI but never bound, and
  ;; no render node appears (verified on 6.12.103: xe loads, card unbound).
  #:use-module ((nongnu packages linux) #:select (linux))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module (peteches systems vm-base)
  #:use-module (peteches services alloy)
  #:use-module (peteches services alltalk)
  #:use-module (peteches services comfyui)
  #:use-module (peteches services koboldcpp)
  #:use-module (peteches services ovms)
  #:use-module (peteches services restic)
  #:use-module (peteches services transcriber)
  #:use-module (peteches services tailscale)
  ;; KEEP, even though the strata service is retired: this module is where
  ;; the Intel Arc packages in #:extra-packages are actually defined
  ;; (intel-igc-core, intel-igc-opencl, libigdgmm, intel-level-zero-gpu,
  ;; intel-opencl-icd), and those are OVMS's deps, not strata's.  Dropping
  ;; this import fails the build with "intel-level-zero-gpu: unbound
  ;; variable".  If the module is ever renamed, move those five packages
  ;; somewhere neutral (e.g. (peteches packages intel-gpu)) rather than
  ;; leaving them under a retired engine's name.
  #:use-module (peteches packages strata)
  #:use-module (gnu packages base)         ; glibc (for the /lib64 symlink)
  #:use-module (gnu packages gcc)          ; gcc `lib' output (libstdc++/libgcc_s)
  #:use-module (gnu packages oneapi)       ; level-zero (libze_loader.so.1)
  #:use-module (gnu packages opencl)       ; opencl-icd-loader (libOpenCL.so.1)
  #:use-module (gnu packages compression)  ; zstd (libzstd.so.1, an IGC dep)
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
     ;; Nonfree kernel, needed for the Arc B70's `xe' firmware (header note).
     ;; #:with-nonguix? registers the nonguix substitute server so the kernel
     ;; and the NVIDIA stack are fetched rather than built locally.
     ;; Channel default `linux' (latest) — its xe driver enumerates the B70.
     #:kernel linux
     #:with-nonguix? #t
     #:with-intel-gpu? #t
     #:with-nvidia? #t
     ;; RTX 4090 (Ada Lovelace) is well clear of the 590+ pre-Turing GPU
     ;; cutoff that bit dagon's Pascal-based GTX 1060 — see (peteches
     ;; systems vm-base)'s nvidia-packages-for-version.
     #:nvidia-driver-version '595
     ;; Intel Arc B70 (Battlemage) OpenCL / IGC / Level-Zero stack.  These
     ;; are the *system* library deps of OVMS's intel_gpu plugin, which is
     ;; what runs on the B70 now that strata is retired -- see the
     ;; "RUNTIME ENVIRONMENT" and "RUNPATH COVERS OVMS, NOT THE HOST'S ICD
     ;; CHAIN" sections of (peteches services ovms) for the measured dlopen
     ;; chain and the A/B/C/D device-enumeration table.  Also in the
     ;; profile so `clinfo`/`ze_*` are available for debugging the B70.
     ;;
     ;; DO NOT TRIM THIS LIST.  It was trimmed once already -- to
     ;; `(list level-zero)', in the same edit that removed the strata
     ;; service, on the reasoning that the rest was strata's -- and the
     ;; failure was silent and total: intel.icd vanished from the profile,
     ;; each instance's opencl-vendors symlink went dangling, OpenVINO
     ;; enumerated ['CPU'] only, and ovms-qwen36 crash-looped on
     ;; "[GPU] Context was not initialized for 0 device" until shepherd
     ;; disabled it.  NOTHING in that failure names a missing package, and
     ;; the box looks healthy from every other angle.  Per (peteches
     ;; services ovms) the four that demonstrably matter are
     ;; intel-opencl-icd, intel-igc-opencl, libigdgmm and zstd:lib; the
     ;; rest are kept because they are documented there as the installed
     ;; set, and re-deriving which are load-bearing costs an outage to get
     ;; wrong.  level-zero + intel-level-zero-gpu stay too: OVMS's plugin
     ;; is built with GPU_RT_TYPE=ZE, so the ZE loader is on the path even
     ;; though device *enumeration* goes through OpenCL.
     #:extra-packages
     (list intel-level-zero-gpu intel-igc-core intel-igc-opencl
           intel-opencl-icd libigdgmm level-zero
           ;; `libOpenCL.so.1' comes from opencl-icd-loader.  libstdc++.so.6/
           ;; libgcc_s.so.1 come from gcc's `lib' OUTPUT -- gcc's default
           ;; `out' is the compiler, not the runtime libs, so the (PACKAGE
           ;; OUTPUT) cons pair is required (the OS `packages' field
           ;; supports it).  `libzstd.so.1' comes from zstd's `lib' OUTPUT
           ;; (zstd's default `out' is the command, not the library): the
           ;; Intel IGC (libigc.so.2, loaded by the NEO runtime via dlopen)
           ;; NEEDs it, and without it the compiler interface fails to load.
           (list zstd "lib") opencl-icd-loader (list gcc "lib"))
     #:extra-services
     (list
      (service tailscale-service-type
               (list (tailscale-instance-configuration
                      (name "peteches"))))
  (service comfyui-service-type
           (list
	    (comfyui-configuration
	     ;; Auto-start -- ComfyUI owns the card at boot; the koboldcpp service
	     ;; below is manual-start, because 24GB cannot hold both and the split
	     ;; decides whether ComfyUI's weights stay resident or are re-streamed
	     ;; per step. With koboldcpp up it holds 20.47GB (nvidia-smi,
	     ;; 2026-11-07), leaving ComfyUI ~2GB, so its dynamic-VRAM loader cannot
	     ;; keep the 5.1GB int8 MOSS model resident and re-streams it on every
	     ;; KSampler step: measured 4.4-4.6 s/it (~18-21s per prompt) against
	     ;; 2.5 it/s (1.85s per prompt) measured on 2026-11-02 with the model
	     ;; resident. PCIe bandwidth is the smaller term -- 3.2-3.3 GB/s
	     ;; measured through the x4 slot vs ~13 GB/s on the Gen5 x8 path the
	     ;; card used before -- and it only bites because nothing is resident.
	     ;; For an LLM session: `herd start koboldcpp', and `herd stop comfyui'
	     ;; if the image work matters at that moment, then the reverse
	     ;; afterwards.
	     (auto-start? #t)
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
      ;; AllTalk TTS v2 — the GPU TTS server SillyTavern (agents VM) talks to,
      ;; replacing the CPU Kokoro path (koboldcpp --ttsmodel): XTTS v2 on the
      ;; 4090 generates several times realtime with per-voice emotional
      ;; delivery cloned from ~6 s reference WAVs, where CPU Kokoro was slow
      ;; AND flat.  Runs INSIDE the FHS/uv sandbox as its own service (see
      ;; (peteches services alltalk) for the design): API on :7851 (what
      ;; SillyTavern's native AllTalk provider speaks, proxied by caddy as
      ;; alltalk.ts.peteches.co.uk) + Gradio management UI on :7852
      ;; (alltalk-ui.ts.peteches.co.uk — engine settings, voice library,
      ;; in-app model downloads).
      ;;
      ;; VRAM budget: auto-start, UNLIKE koboldcpp below.  XTTS v2 resident
      ;; is ~4GB (1.9GB weights fp32 + runtime), a different class from
      ;; koboldcpp's 20.47GB: ComfyUI (~13.5GB typical resident) + AllTalk
      ;; fits the 24GB card with room for ComfyUI's dynamic loader to keep
      ;; working.  The combination that does NOT fit is all three: starting
      ;; koboldcpp for an LLM session needs `herd stop alltalk' first (or
      ;; accepting ComfyUI streams weights per step — see the arithmetic in
      ;; the comfyui-configuration comment above).
      ;;
      ;; First boot: `sudo herd start alltalk-sync' once — clones the
      ;; alltalkbeta branch into /media/models/alltalk, builds the venv
      ;; (~8GB of cu128 wheels, project-dir lives on the hot-storage volume,
      ;; NOT the 74%-full root disk) and downloads the XTTS v2 weights
      ;; (~1.9GB from HuggingFace); then `sudo herd start alltalk'.  The
      ;; daemon auto-starts on every boot after that.
      (service alltalk-service-type
               (list
                (alltalk-configuration
                 (auto-start? #t)
                 (project-dir "/media/models/alltalk")
                 (container-extra-shares (list "/media/models"))
                 (open-firewall? #t)
                 ;; Serve the Gradio UI over https through caddy's
                 ;; alltalk-ui.ts.peteches.co.uk vhost.  This makes AllTalk
                 ;; build every API/audio URL from this absolute https base
                 ;; (via the ALLTALK_API_URL source patch) instead of
                 ;; http://<Host-header>:7851 -- without it the proxied UI
                 ;; POSTs to <proxy-host>:7851 (routes nowhere -> 3-minute
                 ;; retry hang) and would be http under an https page.
                 ;; SillyTavern is unaffected either way: it calls the API
                 ;; (alltalk.ts.peteches.co.uk -> :7851) directly and gets
                 ;; relative /audio/ URLs back from tts_server.py.
                 (api-public-url "https://alltalk.ts.peteches.co.uk")
                 (runtime-packages (list uv)))))
      ;; WhisperX transcription + speaker diarization — answers "who said
      ;; what", which AllTalk's Transcribe tab cannot (plain whisper,
      ;; single-stream; its diarization toggle is wired to nothing).  Lazy
      ;; model loading + 10-min idle unload: unlike AllTalk it does NOT
      ;; squat VRAM between requests, so it coexists with ComfyUI+AllTalk
      ;; (~4GB transient during a transcription).  API + upload UI on :8787,
      ;; proxied by caddy as transcribe.ts.peteches.co.uk — single-origin
      ;; FastAPI app, so none of AllTalk's two-port proxy problems apply.
      ;; Diarization is NVIDIA SortFormer v2.1 in ONNX on CPU (see
      ;; (peteches services transcriber)): ungated, so no HF account/token and
      ;; no browser terms-acceptance step — the checkpoint downloads itself into
      ;; /media/models/transcriber-cache/onnx (~470 MB) on the first diarized
      ;; request.  Threads are capped below the box's 16 because this CPU work
      ;; shares the machine with OVMS inference and ComfyUI.
      ;; `summarize=true' sends the labelled transcript to ovms-qwen36 on
      ;; loopback (auto-start? #t, so it is up) for minutes + task
      ;; allocation.  It used to go to the retired strata engine on :8000,
      ;; which is why summarizer-base-url and summarizer-model are set
      ;; explicitly below instead of being left at their strata defaults.
      ;; OVMS answers BOTH /v1/chat/completions and /v3/chat/completions
      ;; (measured live on :8093), so transcriber-server.py's hardcoded
      ;; "/v1/..." paths work against it unmodified.  The
      ;; service writes no output files at all — this VM is a processor, not a
      ;; store, and nothing on it is restic-backed — so transcripts and minutes
      ;; live wherever the client put them (`transcribe' uses -o,
      ;; $TRANSCRIBE_OUT_DIR or stdout).
      ;; First boot: `sudo herd start transcriber-sync' once (~10GB of cu128
      ;; wheels into /media/models/transcriber), then the daemon auto-starts on
      ;; boot.
      (service transcriber-service-type
               (list
                (transcriber-configuration
                 (auto-start? #t)
                 (project-dir "/media/models/transcriber")
                 (cache-dir "/media/models/transcriber-cache")
                 (diarizer-threads 8)
                 ;; Minutes backend: ovms-qwen36 on this VM, replacing the
                 ;; retired strata engine.  Loopback reaches it because the
                 ;; container runner passes `--network', sharing the host's
                 ;; network namespace.  The model id is pinned rather than
                 ;; left empty (empty = ask /v1/models) so a model swap on
                 ;; the OVMS side cannot silently change what summarises
                 ;; transcripts.  Must match ovms-qwen36's `model-name'.
                 (summarizer-base-url "http://127.0.0.1:8093")
                 (summarizer-model "Qwen3.6-35B-A3B-int4-ov")
                 (container-extra-shares (list "/media/models"))
                 (open-firewall? #t)
                 (runtime-packages (list uv)))))
      ;; The LLM backend for every pi session on claude-workstation (all three
      ;; accounts, via configs/pi/defaults/models.json -> Caddy's
      ;; koboldcpp.ts.peteches.co.uk), but manual-start (reverted on 2026-11-07):
      ;; its 20.47GB live allocation is exactly what pushes ComfyUI into a ~2GB
      ;; VRAM budget and per-step weight streaming, so a boot now leaves the card
      ;; with ComfyUI resident and an LLM session is started by hand with
      ;; `herd start koboldcpp' -- accepting that image generation runs at
      ;; ~4.5 s/it instead of ~0.4 s/it while it is up, or stopping comfyui
      ;; first. See the comfyui-configuration comment above for the arithmetic.
      ;;
      ;; It was auto-start? #t from 2026-10-04 because it ran continuously in
      ;; practice; that split is no longer preferred now that image work on this
      ;; box has a measured cost attached to it.
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
                (auto-start? #f)
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
                                 ;; AllTalk TTS daemon log (XTTS server for
                                 ;; SillyTavern) — model-load failures and the
                                 ;; /api/ready startup window show up here.
                                 (cons "/var/log/alltalk/alltalk.log" "alltalk")
                                 ;; WhisperX transcriber daemon log.
                                 (cons "/var/log/transcriber/transcriber.log"
                                       "transcriber")
                                 (cons "/var/log/comfyui/comfyui/update.log" "comfyui")
                                 ;; The LLM server's shepherd log -- the
                                 ;; fleet's only view of the `llm' provider
                                 ;; pi, herdr and the bots all share.  OVMS's
                                 ;; own per-request lines are what tells a
                                 ;; truncation from a parse failure once a
                                 ;; turn goes wrong.
                                 ;;
                                 ;; HISTORY: during the 2026-11-26 migration
                                 ;; BOTH engines' logs shipped, the
                                 ;; openvino-server.py shim's alongside
                                 ;; OVMS's, so the two stayed comparable side
                                 ;; by side in Loki.  The shim's log is what
                                 ;; made docs/openvino-think-budget.md
                                 ;; diagnosable in the first place.  Its
                                 ;; entry (/var/log/openvino/
                                 ;; openvino-lfm2.log, label "openvino") was
                                 ;; dropped here when openvino-service-type
                                 ;; was retired and its module deleted; this
                                 ;; is now the only LLM log the fleet ships.
                                 ;; Same ownership and mode as alltalk.log
                                 ;; above (root:root 640), which alloy already
                                 ;; reads.  (This used to cite strata.log too;
                                 ;; that entry went with the strata service.)
                                 ;; GLOB, not a per-instance filename: there
                                 ;; are now three ovms instances
                                 ;; (ovms-qwen36, ovms-embed, ovms-rerank)
                                 ;; and a glob picks up the next one too.
                                 ;; This is the ONLY
                                 ;; fleet-visible signal for either instance
                                 ;; -- OVMS exposes no metrics endpoint
                                 ;; through this service type, and
                                 ;; prometheus.scm has no HTTP-probe/blackbox
                                 ;; job -- so an instance that is alive but
                                 ;; wedged is invisible to the fleet unless
                                 ;; its log ships.  local.file_match globs
                                 ;; already work here; cf. the
                                 ;; /var/log/tailscaled-*.log entry above.
                                 (cons "/var/log/ovms/*.log" "ovms")))))
      ;; ── OVMS on the Arc Pro B70 (GPU): the fleet's LLM endpoint ────────
      ;; Intel's own OpenVINO Model Server serving Qwen3.6-35B-A3B INT4, on
      ;; port 8093 -- the port caddy's llm.ts.peteches.co.uk upstream already
      ;; points at, so no FUNCTIONAL caddy change is needed.  caddy.scm's
      ;; comment block for that route IS rewritten in the same commit (it
      ;; described the deleted shim), but that edit is comment-only and
      ;; provably derivation-neutral -- HEAD's caddy.scm and the edited copy
      ;; compute a byte-identical system.drv -- so caddy is not owed a deploy.
      ;; See (peteches
      ;; services ovms) for the service's runtime-environment requirements
      ;; and (peteches packages ovms) for why this is a prebuilt-tarball
      ;; package rather than a source build (OVMS builds only via Docker +
      ;; Bazel, and Guix packages neither bazel nor drogon).
      ;;
      ;; WHAT IT REPLACED, AND WHY.  This box served LFM2.5-24B-A2B INT8
      ;; until 2026-11-26, first through a 1095-line hand-written OpenAI shim
      ;; over the openvino-genai Python bindings
      ;; (peteches/services/openvino-server.py) and then through OVMS.  Both
      ;; that instance and the shim are gone: the MODEL was the problem, not
      ;; the server.  An identical agentic benchmark task (report the exact
      ;; line counts of three files using only the `read' tool, no shell) was
      ;; run through real pi subagent sessions and scored by counting
      ;; repeated n-grams in the transcript:
      ;;
      ;;   Qwen3.6-35B-A3B int4 + OVMS + B70      8-gram repeat 2, largest
      ;;     thinking block 2972 chars, 6 clean tool calls, 3/3 correct, 1m2s
      ;;   control, remote qwen3.8-max            repeat 1, 1188 chars,
      ;;     15 clean calls, 3/3 correct, 1m9s
      ;;   LFM2.5 int8 + OVMS continuous batching, best of four sampling
      ;;     configs                              repeat 47, 40121 chars,
      ;;     13 calls, 1/3 correct, 7.5m
      ;;   LFM2.5 int8 + OVMS 2026.5.0 weekly, CB repeat 407, 89199 chars,
      ;;     10 calls, 0/3 correct, 10m
      ;;   LFM2.5 int8 + OVMS --pipeline_type LM  repeat 3, 3664 chars,
      ;;     5 calls, clean output but then HUNG (30 min timeout)
      ;;   LFM2.5 Q4_K_M + koboldcpp Vulkan       repeat 5, 7920 chars,
      ;;     3 calls, 0/3, tool markup leaked as text, 42s
      ;;   Qwen3.6-35B-A3B Q4_K + koboldcpp Vulkan repeat 0, 22 clean calls,
      ;;     0/3, starved, 99m then killed
      ;;
      ;; Ruled out as the cause of LFM2.5's degeneration, each by direct
      ;; measurement: abliteration (the official non-abliterated INT4 export
      ;; spiralled worst, one line repeated 1430x), a missing paged-attention
      ;; graph fix (the served INT8 graph sha256-matches blaj's
      ;; `paged-attention' branch, which adds an explicit position_ids
      ;; Parameter layer), sampling temperature, repetition_penalty,
      ;; KV-cache size (1.4 GB vs 8 GB made no difference), prefill
      ;; throughput, and the OVMS version.  Two independent serving stacks
      ;; (OpenVINO/OVMS and llama.cpp/koboldcpp) both fail with LFM2.5, so
      ;; the model is the variable.
      ;;
      ;; THE MODEL.  HuggingFace `OpenVINO/Qwen3.6-35B-A3B-int4-ov', an
      ;; official Intel export created 2026-05-20, 19.43 GB total:
      ;; openvino_language_model.bin 17.99 GB plus openvino_mtp_model.bin
      ;; 0.44 GB, openvino_text_embeddings_model.bin 0.51 GB and
      ;; openvino_vision_embeddings_merger_model.bin 0.44 GB.  Intel
      ;; publishes ONLY INT4 for this model -- there is no int8 or fp16
      ;; export to choose between, so unlike the LFM2.5 instance this one
      ;; replaced there is no precision trade-off to re-litigate.
      ;; config.json says model_type qwen3_5_moe and max_position_embeddings
      ;; 262144.
      ;;
      ;; CONTEXT CEILING, VERIFIED EMPIRICALLY rather than read from config:
      ;; a 224017-token prompt was accepted; a 280017-token prompt returned
      ;; `HTTP 400 ... Number of prompt tokens: 280017 exceeds model max
      ;; length: 262144'.  So the usable window is 262144 and OVMS enforces
      ;; it.  That number is what configs/pi/defaults/models.json declares
      ;; as contextWindow and what peteches/home/modules/pi.scm derives
      ;; reserveTokens from -- move the two together.
      ;;
      ;; MEASURED ON OVMS 2026.4.1 / ARC PRO B70, 2026-11-26.  ~35 s to
      ;; AVAILABLE; `Initializing Language Model Continuous Batching
      ;; servable' under --pipeline_type AUTO; decode 101.4-101.9 T/s over
      ;; 1500-token completions, stable across temperature 1.0 / model
      ;; default / 0; KV cache 1.0 GB allocated dynamically, peaking at
      ;; 67.6% during the agentic test.  Streaming: prose turns give block
      ;; order R C F=stop (reasoning strictly before content), tool turns
      ;; give R T F=tool_calls with incremental tool-call deltas, a
      ;; deliberate max_tokens=8 truncation returns finish_reason=length,
      ;; and streaming `usage' is present in the final chunk when
      ;; stream_options.include_usage is set.  generation_config.json ships
      ;; temperature 1.0 / top_k 20 / top_p 0.95; pi sends no sampling
      ;; parameters, so those defaults are what production runs on and what
      ;; every number here was taken at.
      ;;
      ;; PARSERS ARE AUTO-DETECTED -- do not pin them.  The startup log reads
      ;; `Auto-detected tool_parser: qwen3coder' and `Auto-detected
      ;; reasoning_parser: qwen3', with no `Minja cannot render' failure
      ;; (that failure mode was specific to LFM2, whose template the C++
      ;; minja engine cannot render).  This instance therefore sets neither
      ;; `tool-parser' nor `reasoning-parser'; both now default to #f in
      ;; (peteches services ovms), which omits the flags entirely.  The
      ;; retired LFM2.5 instance pinned both to `lfm2', which is wrong for
      ;; Qwen.
      ;;
      ;; PREFIX CACHING IS THE WHOLE GAME -- 8.2x, never turn it off.  Cold
      ;; prefill of a 44817-token prompt took 30.84 s (1453 T/s); the SAME
      ;; prompt repeated with --enable_prefix_caching took 3.78 s (11850 T/s
      ;; effective).  Multi-turn agent work re-sends almost the whole
      ;; conversation every turn, so this is the difference between a viable
      ;; backend and an unusable one: koboldcpp on the same card, which has
      ;; no GPU-resident prefix cache, spent 99 minutes and never finished
      ;; the task OVMS finished in 62 seconds.  `enable-prefix-caching?'
      ;; defaults to #t in the record, so it is not set explicitly here.
      ;;
      ;; MTP / SPECULATIVE DECODING: TESTED AND REJECTED.  Do not add
      ;; --draft_model_path.  The IR ships openvino_mtp_model.xml/.bin and
      ;; config.json carries "mtp_num_hidden_layers": 1, so
      ;; `--draft_model_path <same model dir> --draft_device GPU' does start
      ;; -- it logs `Draft model strategy: MTP (Multi-Token Prediction)' and
      ;; switches the servable to `Initializing Visual Language Model
      ;; Continuous Batching servable', because the IR also contains a vision
      ;; embeddings merger.  But:
      ;;   * It is MUTUALLY EXCLUSIVE WITH PREFIX CACHING.  With both
      ;;     enabled the first request aborts the executor -- `Check
      ;;     '!m_scheduler->get_config().enable_prefix_caching' failed at
      ;;     continuous_batching/pipeline_impl.cpp:388: Linear-attention
      ;;     verifier speculative decoding requires
      ;;     enable_prefix_caching=false (not yet supported).' -- and OVMS
      ;;     then SEGFAULTS rather than failing gracefully (dmesg:
      ;;     `ovms[6824]: segfault at 55ec78d2662e ip 000055e8f4e31173 ...
      ;;     in ovms[3c3173,55e8f4a6e000+21be000]').
      ;;   * Measured with prefix caching OFF: decode 104-110 T/s against the
      ;;     101.4-101.9 baseline, but the repeated 44817-token prompt cost
      ;;     31.92 s instead of 3.78 s -- no reuse at all.  That is +5-10%
      ;;     decode in exchange for 8.4x worse prefill on every turn after
      ;;     the first: over a 20-turn agent session, roughly +560 s of
      ;;     prefill to save ~18 s of decode.  Prefix caching wins by ~30x.
      ;;   * It forces greedy decoding (the binary contains `WARNING: MTP
      ;;     greedy decoding enforced: setting do_sample to false and
      ;;     num_beams to 1.' and `Overriding num_assistant_tokens to default
      ;;     value of 5 for MTP decoding') AND CHANGES GREEDY OUTPUT:
      ;;     baseline greedy produced 6100 chars of reasoning and 1077 of
      ;;     content, MTP greedy produced 6627 and 50.  Greedy speculative
      ;;     decoding should be token-exact, so that is a correctness concern
      ;;     independent of speed.
      ;;
      ;; model-name MUST stay "Qwen3.6-35B-A3B-int4-ov".  OVMS routes
      ;; requests by --model_name and /v1/models returns it verbatim, and
      ;; configs/pi/defaults/models.json pins that exact id, so any other
      ;; value 404s every pi request.  During testing it was temporarily
      ;; spoofed as "LFM2.5-24B-A2B-int8-ov" so the then-live client config
      ;; kept working; that must not survive into this file.
      ;;
      ;; The same client/server coupling runs the other way for the
      ;; `developer' role and is a LIVE PRODUCTION REQUIREMENT: pi computes
      ;; the instruction role as `model.reasoning &&
      ;; compat.supportsDeveloperRole ? "developer" : "system"', this
      ;; provider sets reasoning true, and Qwen3's chat template validates
      ;; roles -- measured against this endpoint, role=system OK,
      ;; role=user OK, role=developer HTTP 400 "Unexpected message role."
      ;; So configs/pi/defaults/models.json must carry
      ;; "supportsDeveloperRole": false or every single pi request 400s in
      ;; under a second.  LFM2.5's template tolerated `developer', which is
      ;; why six hours of LFM2.5 testing never surfaced it.  See (peteches
      ;; home modules pi).
      ;;
      ;; target-device "GPU" is unambiguous on this VM despite it also
      ;; holding an RTX 4090 (reserved for ComfyUI, never used for LLM
      ;; inference): (peteches services ovms) synthesises a per-instance
      ;; OpenCL vendors directory containing only intel.icd, so OVMS cannot
      ;; enumerate the NVIDIA card or a pocl pseudo-GPU.  Pointing
      ;; OCL_ICD_VENDORS at the whole profile merge is exactly what let an
      ;; earlier hand-run select the 4090 and fail to allocate.
      ;;
      ;; open-firewall? #t with the same three tailnet CIDRs caddy arrives
      ;; over (caddy dials the MagicDNS name, and Go's dialer may resolve it
      ;; to either the CGNAT v4 or the ULA v6 address).  ovms-firewall-rules
      ;; REFUSES to compile open-firewall? #t with no api-key-file and no
      ;; sources, so the CIDRs are not optional.  Loopback is untouched, so
      ;; on-box health checks still work.
      ;; ALL THREE OVMS instances live in ONE service value (a list) rather
      ;; than three separate `(service ovms-service-type ...)' forms, so that
      ;; ovms-accounts' dedup-by-user-name actually runs.  As separate
      ;; instances each contributes its own `ovms' user and group, and Guix
      ;; warns "the following accounts appear more than once: ovms" -- which
      ;; lands duplicate /etc/passwd lines rather than erroring, so it is easy
      ;; to miss.  The list form is exactly what ovms-configs in (peteches
      ;; services ovms) exists to accept, and dedup is verified to hold at
      ;; three configs (ovms-accounts returns exactly one user-group and one
      ;; user-account; /etc/passwd has a single `ovms' line).  Per-instance
      ;; state and log dirs still differ, because those are keyed on
      ;; service-name, not on the shared account; the first config asking for
      ;; a given user name wins that account's home-directory field, which is
      ;; harmless only because OVMS writes nothing to $HOME -- see
      ;; ovms-resolved-state-dir.
      (service ovms-service-type
               (list
                (ovms-server-configuration
                 (service-name "ovms-qwen36")
                 (model-name "Qwen3.6-35B-A3B-int4-ov")
                 (model-path "/media/models/openvino/Qwen3.6-35B-A3B-int4-ov")
                 (port 8093)
                 (host "0.0.0.0")
                 (target-device "GPU")
                 (pipeline-type "AUTO")
                 (auto-start? #t)
                 (open-firewall? #t)
                 (firewall-sources '("100.64.0.0/10"
                                     "fd7a:115c:a1e0::/48"
                                     "fd7a:115c:a6e0::/48")))
      ;; ==================================================================
      ;; CLIENT CONTRACT for the retrieval endpoints (ovms-embed :8094 and
      ;; ovms-rerank :8095).  Everything below is work the CLIENT must do --
      ;; OVMS does none of it.  Read this before wiring a bot to either
      ;; endpoint; the two easiest ways to get silently-bad retrieval are
      ;; omitting the query instruction and indexing degenerate PDF chunks.
      ;;
      ;; --- QUERY SIDE: the instruction prefix is MANDATORY, not a tuning
      ;; --- nicety ----------------------------------------------------
      ;; Qwen3-Embedding is instruction-aware and ASYMMETRIC: queries and
      ;; documents are embedded differently.  OVMS applies no prompt of any
      ;; kind -- it embeds the bytes you send.  The model's own
      ;; config_sentence_transformers.json ships
      ;;   prompts.query    = "Instruct: Given a web search query, retrieve
      ;;                       relevant passages that answer the query\nQuery:"
      ;;   prompts.document = ""
      ;; so the client must prepend the instruction to QUERIES ONLY and never
      ;; to documents at index time.  For a rules-lawyer, tune it to the
      ;; domain, e.g.
      ;;   "Instruct: Given a rules question, retrieve the rule text that
      ;;    answers it\nQuery: "
      ;; Qwen's own docs put the gain at 1-5% and warn that omitting it
      ;; "can lead to a drop in retrieval performance by approximately 1%
      ;; to 5%".
      ;;
      ;; FOR DISCORD/CHAT BOTS IT IS LOAD-BEARING, NOT OPTIONAL.
      ;; arXiv:2604.06176 (2026-02-03), "Robustness Risk of Conversational
      ;; Retrieval: ... Noise Sensitivity in Qwen3-Embedding Model", found
      ;; that under conversational retrieval WITHOUT query prompting,
      ;; "structured dialogue-style noise can become disproportionately
      ;; retrievable and intrude into top-ranked results"; that the failure
      ;; "emerges consistently across model scales, remains largely invisible
      ;; under standard clean-query benchmarks, and is significantly more
      ;; pronounced in Qwen3" than in earlier Qwen variants and other dense
      ;; baselines; and that "lightweight query prompting qualitatively
      ;; alters retrieval behavior, effectively suppressing noise
      ;; intrusion."  A Discord transcript is exactly that noise.  MTEB will
      ;; never show you this failure.
      ;;
      ;; Pick ONE query instruction and ONE (empty) document convention and
      ;; use them FLEET-WIDE.  Instructions change the vector, so prefixing
      ;; some corpora and not others -- or two bots using different wording
      ;; over one shared collection -- silently degrades retrieval.  Bots may
      ;; have separate collections; they must not have separate conventions.
      ;;
      ;; --- CHUNKING: align to the document's own structure -------------
      ;; Rulebooks are legal-code-shaped: numbered, hierarchical, heavily
      ;; cross-referential.  arXiv:2605.19806v2 (2026-05-19), on the German
      ;; Civil Code, found strategies "aligned with the inherent legal
      ;; structure ... achieve the highest recall, while more complex
      ;; approaches that override this structure perform worse" -- and that
      ;; they are cheaper than contextual chunking / RAPTOR / Lumber.  So:
      ;;   * Split on rule numbering and section headings, NOT on a fixed
      ;;     token window.  A rule is the unit of meaning; 200 arbitrary
      ;;     tokens usually is not, and cutting mid-rule produces a chunk
      ;;     that retrieves well and answers badly.
      ;;   * Carry parent scope into the chunk ("7.3 Combat -- 7.3.2
      ;;     Charging -- <rule text>") so the embedding sees the hierarchy,
      ;;     and keep the same fields in metadata for filtering by game,
      ;;     edition, chapter, and base-rule-vs-errata.  Metadata filtering
      ;;     is worth as much as embedding quality on a multi-game corpus.
      ;;   * Preserve cross-references ("see 7.3.2") rather than stripping
      ;;     them -- they are signal, both lexically and structurally.
      ;;   * SMALL-TO-BIG: index the small unit (one rule) for precise
      ;;     retrieval, but return the larger parent section to the LLM.
      ;;     arXiv:2511.18177 measured a 65% win rate over baseline chunking
      ;;     for +0.2 s of latency.
      ;;   * Do NOT invest in semantic / embedding-driven chunking.  Two
      ;;     studies found it does not pay: arXiv:2410.13070 across document
      ;;     retrieval, evidence retrieval and retrieval-based answer
      ;;     generation ("the computational costs ... are not justified by
      ;;     consistent performance gains") and arXiv:2607.01852 ("cluster-
      ;;     based chunking did not outperform simpler strategies").
      ;;   * Chunk size must fit the servable's max_length -- see the batch
      ;;     trap under BULK INDEXING below.
      ;;
      ;; --- DEGENERATE CHUNKS MUST BE DROPPED BEFORE INDEXING -------------
      ;; PDF extraction of rulebooks routinely yields empty pages, image-only
      ;; pages, headers/footers, page numbers and failed table extraction.
      ;; Filter these at the indexer -- drop anything empty after .strip(),
      ;; or simply guarantee at least one non-empty element per batch.
      ;; The second alone is sufficient, and the precise reason is in the
      ;; KNOWN DEFECT note below: what PERMANENTLY WEDGES ovms-embed is a
      ;; batch in which EVERY element is content-free, not a batch that
      ;; merely contains an empty string.  A mixed batch is completely safe
      ;; (measured: 20 batches of 32 with 20% empty returned 640/640
      ;; vectors).  The realistic way to hit the fatal case is a ragged
      ;; FINAL batch drawn from a run of degenerate chunks, or a filter that
      ;; strips everything -- which would abort a multi-hour index run
      ;; partway through and need `herd restart ovms-embed' to resume.
      ;; You also do not want empty chunks in the store regardless: they
      ;; match nothing or everything.  Whitespace-only chunks are NOT a wedge
      ;; risk (they tokenize to something), but they are still noise.
      ;;
      ;; --- BULK INDEXING -------------------------------------------------
      ;; Index and query MUST use the same model: they have to live in one
      ;; vector space or cosine similarity is meaningless.  Changing model
      ;; later means re-embedding the whole corpus, which is why the embedder
      ;; is the locked-in decision here and the reranker is not.
      ;; At this project's scale -- tens of GB of PDFs, i.e. roughly 10^5 to
      ;; 10^6 chunks -- a full re-index costs hours, not weeks, so the
      ;; lock-in is genuinely cheap.  Prefer a working pipeline now over a
      ;; marginally better model later; revisit the tier only if your own
      ;; eval (below) says it matters.
      ;; BATCH TRAP: OVMS checks the WORST-CASE PADDED tensor size
      ;; (batch size x model context length) BEFORE tokenizing and rejects
      ;; over 1 GiB, so with a 32768-token context even short strings can be
      ;; refused in a large batch.  Tune --max_length and --max_batch_size
      ;; DOWN TOGETHER via extra-args.  The request body is separately capped
      ;; at 2,000,000 bytes.
      ;;
      ;; --- DIMENSIONS AND MATRYOSHKA (MRL) -------------------------------
      ;; Output is 1024-dim and L2-normalised, so use cosine (equivalently,
      ;; dot product on the normalised vectors).
      ;; Qwen3-Embedding supports user-defined output dims from 32 to 1024,
      ;; but OVMS does NOT accept the OpenAI `dimensions' parameter (it is in
      ;; the endpoint's documented unsupported list), so truncation is
      ;; CLIENT-SIDE: take the first N dims, then re-normalise to unit L2.
      ;; That is the model's own recipe (last-token pooling + F.normalize),
      ;; so truncate-then-renormalise is the intended mechanism.
      ;; The quality cost at 512/256 is UNPUBLISHED -- Qwen asserts support
      ;; with no degradation curve, and the MRL paper's evidence is ImageNet
      ;; classification (arXiv:2205.13147), which does not transfer to text
      ;; retrieval.  At 1024 dims a million passages is only ~4.2 GB fp32
      ;; (plus ~0.13 GB of HNSW graph at m=16), so INDEX AT FULL 1024 and
      ;; A/B truncation only if index RAM ever becomes the constraint.
      ;; Note a 4096-dim model would not even be indexable in pgvector
      ;; (`vector' <= 2000 dims, `halfvec' <= 4000) -- one more reason the
      ;; 1024-dim tier is the right default here.
      ;;
      ;; --- HYBRID RETRIEVAL: the sparse side comes from the DB -----------
      ;; Not from the model.  bge-m3 does emit learned sparse/lexical weights
      ;; and ColBERT multi-vectors, but OVMS's embeddings calculator exposes
      ;; exactly seven options (models_path, normalize_embeddings,
      ;; target_device, plugin_config, pooling, truncate, max_length,
      ;; max_batch_size -- verified in embeddings_calculator_ov.proto): no
      ;; sparse, no multi-output.  So on this stack hybrid means the VECTOR
      ;; DB supplies BM25/full-text and you fuse (RRF or weighted) yourself.
      ;; That is a perfectly good design; it just is not bge-m3's learned
      ;; sparsity, which is why bge-m3's main advantage is unreachable here.
      ;; Hybrid is insurance against dense-only failure modes rather than a
      ;; uniform uplift: arXiv:2611.26514 measured BM25 alone at MRR 0.864 vs
      ;; hybrid 0.872/0.879 but DENSE-ONLY at 0.663/0.655, and concluded
      ;; "improvements over a strong lexical baseline are modest and
      ;; metric-dependent".  Rules text is dense with exact identifiers --
      ;; card names, rule numbers, defined terms -- which is precisely the
      ;; dense-failure case, so hybrid is worth having here.
      ;; Fixed dense/sparse weighting is the weak point; per-query dynamic
      ;; weighting (arXiv:2503.23013, DAT) "consistently significantly
      ;; outperforms fixed-weighting hybrid retrieval".
      ;;
      ;; --- RERANKING: the biggest lever, and the reversible one ----------
      ;; Retrieve WIDE with ovms-embed (50-100 candidates), then rerank down
      ;; to ~5 with ovms-rerank.  Measured gains are an order of magnitude
      ;; larger than any embedder swap discussed here:
      ;;   * arXiv:2603.16877v2 (2026-02-18), 1500 queries over S&P 500
      ;;     10-Ks: answer correctness 33.5% -> 49.0% (+15.5 pp), completely-
      ;;     incorrect answers 35.3% -> 22.5% (-12.8 pp).
      ;;   * arXiv:2511.18177v1: "+59% absolute improvement ... for MRR@5".
      ;; A cross-encoder reads query and passage JOINTLY through every layer,
      ;; which is exactly what separates mechanically-different rules that
      ;; share wording -- something a bi-encoder cannot do at ANY parameter
      ;; count, because it must compress each side into one vector before the
      ;; two ever meet.  That is the structural reason reranking beats
      ;; scaling the embedder.
      ;; COST: one forward pass per (query, document) PAIR, so cost scales
      ;; with candidate count, not one pass per query.  Size the candidate
      ;; list deliberately.
      ;; A reranker needs NO re-embedding to add, tune or swap -- it is
      ;; stateless at query time.  That asymmetry is the argument for putting
      ;; the quality investment here rather than in the embedder.
      ;; KEEP IT HONEST: reranking is not automatically a win.  REBEL
      ;; (arXiv:2504.07104) shows "maximizing for context relevance alone can
      ;; degrade downstream response quality", and arXiv:2609.13856 found
      ;; plain dense beat RRF+cross-encoder on 2632 held-out queries (though
      ;; on intent-classification accuracy, not retrieval nDCG, so it is weak
      ;; evidence).  Measure on your own corpus; do not assume transfer.
      ;;
      ;; --- EVALUATE ON YOUR OWN CORPUS BEFORE OPTIMISING ANY OF THIS -----
      ;; Leaderboard deltas at this level are noise.  Recomputed from the raw
      ;; mteb/results dataset (8,984,367 rows) using MTEB's own eng-v2
      ;; retrieval task list: nvidia/NV-Embed-v2 ranks #14 at 62.84 and
      ;; Qwen3-Embedding-0.6B #17 at 61.83 -- a 1.01-point mean difference
      ;; against per-task swings of +/-19 (FiQA2018 -19.1, HotpotQA-HN -16.1,
      ;; Touche2020 +12.6, ClimateFEVER-HN +10.2), a per-model std dev of ~20
      ;; over n=10 tasks, and Qwen3 winning 6 of the 10.  The whole top-30
      ;; spans 10.06 points, and MTEB-BR (arXiv:2607.04581v2) found "the top
      ;; six are statistically too close to order".  NV-Embed-v2's famous
      ;; 72.31 was an OVERALL score across 56 tasks whose lead came from
      ;; classification/clustering/STS -- tasks a RAG system never runs.
      ;; Rank also transfers poorly across domains: MTEB-BR measured Spearman
      ;; rho = 0.75 between global and Portuguese ranks, with one model 3rd
      ;; globally and 49th locally.
      ;; So: build 50-100 real rules questions with judged correct-rule
      ;; labels FIRST.  Every choice above -- embedder tier, chunk size and
      ;; shape, hybrid weighting, rerank candidate count, MRL truncation --
      ;; then becomes a measurement instead of an argument.  arXiv:2607.23507
      ;; makes the same point from the other direction: "the model that tops
      ;; a leaderboard is rarely the best choice for a given deployment", and
      ;; model choice must be reasoned about "as one decision within a
      ;; complete retrieval pipeline rather than in isolation".
      ;;
      ;; Rough order of expected value for this workload, best first:
      ;;   1. query instruction prefix (free, +1-5%, fixes the Discord noise
      ;;      failure mode)         2. cross-encoder reranker (+15.5 pp)
      ;;   3. hybrid dense+BM25 (cheap insurance, large when dense-only is
      ;;      weak)                 4. structure-aligned chunking +
      ;;      small-to-big (65% win rate at +0.2 s)
      ;;   5. a bigger embedder -- LAST: ~1 point for 13x the parameters at
      ;;      the top of the board, and it forces a full corpus re-embed.
      ;; ==================================================================
      ;; ── OVMS embeddings on the Arc Pro B70 (second instance) ──────────
      ;; OpenVINO/Qwen3-Embedding-0.6B-int8-ov on port 8094, proxied by caddy
      ;; as embeddings.ts.peteches.co.uk.  English RAG retrieval, always-on
      ;; alongside ovms-qwen36 above.
      ;;
      ;; WHY THIS MODEL, AND WHY NOT NV-EMBED-V2.  NV-Embed-v2 was the
      ;; original choice on MTEB merit (Eng-v2 Retrieval 62.84) and is
      ;; UNSERVABLE BY OVMS -- not merely unconverted but architecturally
      ;; incompatible.  OVMS's embeddings servable requires a rank-3
      ;; [batch,seq,hidden] output and appends its own pooling
      ;; (embeddings_servable.cpp throws "No output with 3 dimensions found";
      ;; create_post_model hardcodes PartialShape{-1,-1,dim}), while
      ;; NV-Embed's latent-attention head returns an ALREADY-POOLED rank-2
      ;; [batch,4096] and additionally needs a `pool_mask' input that OVMS
      ;; never supplies.  Independently of that: optimum-cli refuses the
      ;; custom `nvembed' architecture outright ("Trying to export a nvembed
      ;; model, that is a custom or unsupported architecture"), its
      ;; `custom_export_configs' escape hatch is Python-only with no CLI
      ;; flag, and the version windows do not intersect (optimum-intel 2.2.0
      ;; needs transformers>=4.51; NV-Embed's bundled code is frozen at
      ;; <=4.46, and under 5.x the v4.47 rotary refactor makes
      ;; MistralDecoderLayer.forward throw).  Do not re-attempt it without an
      ;; upstream change to one of those three walls.
      ;;
      ;; Qwen3-Embedding-0.6B is the strongest thing OVMS CAN serve.  A full
      ;; sweep of all 476 repos in the OpenVINO HF org found only three
      ;; embedding families published as official IRs: Qwen3-Embedding-0.6B,
      ;; bge-m3 and bge-base-en-v1.5.  There is NO Qwen3-Embedding-4B or -8B
      ;; IR -- community INT4/CTranslate2/AWQ/bnb/ONNX builds exist but none
      ;; is an official -ov export, and using one would reintroduce exactly
      ;; the conversion problem above.  At 613 MB it scores Eng-v2 Retrieval
      ;; 61.83 against NV-Embed-v2's 62.84 -- statistically adjacent at 1/26th
      ;; the weight size -- with 32768 max context.  That context is the
      ;; decider against the alternatives: bge-base-en-v1.5 is capped at 512
      ;; tokens, which cripples it for RAG passages, and bge-m3 (8192, CLS
      ;; pooling) only wins for multilingual or sparse-hybrid retrieval,
      ;; which this workload is not.  Apache-2.0, and it is the exact model
      ;; Intel's own OVMS embeddings documentation uses.
      ;;
      ;; MEASURED ON THIS CARD with qwen36 + this instance resident
      ;; (2026-11-27, OVMS 2026.4.1).  This is the TWO-servable figure, taken
      ;; before ovms-rerank existed; see that block below for the
      ;; three-resident total (19.22 GiB).  `intel_gpu_top' DOES NOT WORK on
      ;; this driver -- it exits
      ;; with "Detected Xe device which is not supported by intel_gpu_top.
      ;; Please use 'gputop'" -- so read the xe VRAM-manager node directly:
      ;;   /sys/kernel/debug/dri/0000:02:00.0/tile0/vram_mm
      ;;     card total                     31.9 GiB   (34242297856 B)
      ;;     ovms-qwen36 alone              17.88 GiB  (14.0 GiB free)
      ;;     + this instance                18.54 GiB  (13.35 GiB free)
      ;;     this instance's own footprint  +668 MiB
      ;; So the two genuinely coexist always-on with ~13 GiB to spare.  There
      ;; is no VRAM pressure and therefore no reason to hunt for a smaller
      ;; tier; killing the test process returned VRAM to the exact 17.88 GiB
      ;; baseline.
      ;;
      ;; `task' is "embeddings" -- verified against this exact OVMS build's
      ;; own --help ("Supported tasks: text_generation, embeddings, rerank,
      ;; image_generation, text2speech, speech2text"), NOT "text-embedding".
      ;; The endpoint is /v3/embeddings (OVMS answers the /v1/ spelling too,
      ;; which is what the transcriber's summarizer client uses).  The
      ;; embeddings-specific flags (--pooling CLS|LAST|MEAN, --normalize,
      ;; --truncate, --max_length, --num_streams) are omitted so OVMS
      ;; auto-detects them from the IR: measured, it logs "Detected pooling
      ;; 'LAST' from .../1_Pooling/config.json" for this model and returns
      ;; L2-normalised vectors (norms 0.9997-1.0001) at dim 1024, so its
      ;; normalize=true default is already what we want.  Add them via
      ;; extra-args only if auto-detection ever proves wrong.
      ;;
      ;; KNOWN DEFECT, MEASURED 2026-11-27 -- AN ALL-EMPTY BATCH WEDGES THIS
      ;; SERVICABLE PERMANENTLY.  The trigger is NARROWER than it first
      ;; looked, and getting it wrong sends you to the wrong mitigation.
      ;;
      ;;   FATAL (permanent wedge): an input array in which EVERY element is
      ;;     content-free -- [""], ["",""], [""] x 32.
      ;;   SAFE: any batch containing at least one non-empty element.  Mixed
      ;;     batches are completely fine and the empty members still come
      ;;     back with vectors: [GOOD,""] -> 200 with 2 vectors, ["",GOOD]
      ;;     -> 200, [GOOD,"",GOOD] -> 200 with 3, and 1 empty + 31 good at
      ;;     B32 -> 200 with all 32.  A realistic dirty-batch simulation
      ;;     (20 batches x 32, 20% empty + 10% whitespace) returned
      ;;     640/640 vectors with every post-check 200.
      ;;
      ;; Established by a 48-cell matrix (12 input shapes x 2 models x 2
      ;; devices) with a FRESHLY RESTARTED INSTANCE PER CELL.  That matters:
      ;; the first pass -- and the first live probe against this endpoint --
      ;; tested [""] alone BEFORE testing a mixed batch, so the mixed batch
      ;; then failed as a CONSEQUENCE of the existing wedge and read as its
      ;; own cause.  Ordering-confounded; do not repeat that mistake.
      ;;
      ;; On wedge: HTTP 400 `RET_CHECK failure
      ;; (src/embeddings/embeddings_calculator_ov.cc:382)', and every request
      ;; AFTER that also 400s until `herd restart ovms-embed'.  Permanence
      ;; verified, not assumed: 30 consecutive benign probes over 90 s (one
      ;; every 3 s, each on a brand-new HTTP connection, all ordinary English
      ;; text) returned 400 in ~0.002 s.  0/30 recovered.  The process stays
      ;; alive and shepherd reports it running, so nothing self-heals.
      ;; GREP BOTH ASSERTION SITES when diagnosing -- the log shows
      ;; `:382 ... false' AND `:388 ... embeddingsTensor.get_shape().size()
      ;; == 2'; searching only for :382 misses half the signature.
      ;;
      ;; CAUSE is LAST pooling with no token to land on.  TWO QUALIFICATIONS:
      ;;   * It is GPU-SPECIFIC.  On `--target_device CPU' the same model
      ;;     returns 200 for [""] and merely rejects an all-empty batch with
      ;;     400 while staying alive.  (That is not an argument for running
      ;;     CPU -- see the measured throughput and host-RAM findings below.)
      ;;   * Whitespace is NOT a trigger.  Measured 200 and harmless on both
      ;;     models and both devices: "   \n\t  ", " ", "\n", "\n\n\n",
      ;;     "\u200b", ".", "!", "a", "hi", CJK, emoji.  They all tokenize to
      ;;     something, so LAST pooling has a token.  Only the truly empty
      ;;     string is content-free.
      ;; A CLS-pooled model does not wedge, but is not fully "immune" either:
      ;; bge-m3 accepts [""] with 200 yet still rejects an all-empty
      ;; MULTI-element batch with 400 -- it just stays alive afterwards.
      ;; Do NOT "fix" this by forcing --pooling CLS on THIS model:
      ;; Qwen3-Embedding is trained for last-token pooling, so CLS would
      ;; silently produce meaningless vectors rather than an error.
      ;;
      ;; MITIGATION, AND WHY THERE ISN'T ONE YET.  Prevention belongs at the
      ;; source and is one line: in the indexer and every query path, drop
      ;; chunks that are empty after .strip(), OR guarantee at least one
      ;; non-empty element per batch.  Either is sufficient -- the second is
      ;; enough on its own given the trigger above.  Filtering empties is
      ;; good practice regardless, since an empty chunk in the store matches
      ;; nothing or everything, but it is the ALL-EMPTY BATCH that is
      ;; load-bearing to prevent (a ragged final batch from a corpus of
      ;; degenerate chunks, or a filter that strips everything, are the
      ;; realistic ways to produce one).  Detection is weaker than it looks.  The
      ;; /var/log/ovms/*.log alloy glob makes the RET_CHECK lines QUERYABLE
      ;; in Loki, but NOTHING IN THIS FLEET ALERTS: no alertmanager is
      ;; deployed anywhere, prometheus.scm's `rule-files' field is supported
      ;; but unset, and there is no Loki ruler.  A wedge is therefore
      ;; something a human has to go and look for, not something that pages
      ;; anyone -- which is the real reason this is only acceptable while the
      ;; endpoint has no consumers.
      ;; TODO(retrieval, UNOWNED): add a DETECT-ONLY canary -- an mcron job on
      ;; this box POSTing a fixed benign payload to all three instances every
      ;; 60 s and appending `ovms-canary <name> ok|HTTP <code>' to a file
      ;; under /var/log/ovms/, which the glob already ships.  That gives a
      ;; greppable fleet-visible signal and a Grafana panel with ZERO restart
      ;; authority.  Do NOT let it auto-restart: a canary that acts converts
      ;; one permanent outage into repeated bounded ones, each with a model
      ;; reload, on a GPU shared with the LLM and ComfyUI; and a fixed safe
      ;; payload cannot detect the wedge BEFORE a client trips it, so it only
      ;; shortens the window.  Give it restart authority only after the
      ;; detect-only version has shown the wedge to be rare and restarts to be
      ;; clean, and then hard rate-limit it (1 restart per N minutes, M per
      ;; hour, then stop and stay broken loudly).
      ;; If the defect ever does bite, the two real alternatives are a
      ;; validating proxy in front of :8094 -- which is an argument for
      ;; routing embedding clients through caddy rather than tailnet-direct,
      ;; since direct callers would bypass any such filter -- or switching to
      ;; a CLS-pooled model.  bge-m3 keeps CLS with 8192 context, is already
      ;; on disk at /media/models/openvino/bge-m3-int8-ov, and does not wedge
      ;; on any input tried (it does still 400 on an all-empty MULTI-element
      ;; batch, but stays alive); it costs roughly 10 points of English
      ;; retrieval and 4x less context, and is 1.53x FASTER to index with
      ;; (517 vs 333 pass/s) and lower-latency at query time (p50 10.1 vs
      ;; 15.9 ms).  Note the reranker has NO analogous defect: nothing
      ;; wedges it.
      ;; Relatedly, oversized input is rejected rather than truncated
      ;; (--truncate defaults false): `Input length 60002 longer than allowed
      ;; 32768'.  That one returns a clean INVALID_ARGUMENT per request and
      ;; does NOT wedge the servable, but a RAG pipeline chunking past 32768
      ;; tokens will see 400s.
      ;;
      ;; MEASURED PERFORMANCE AND BULK-INDEXING SETTINGS (2026-11-27, this
      ;; box, hand-run instances on :8099 -- production was never load-
      ;; tested).  Deterministic corpus, 2048 unique Markov-generated
      ;; passages per length bucket, token counts taken from the server's own
      ;; usage.prompt_tokens.  Reproducible: the harness is kept at
      ;; /media/models/bench-embedding/ (bench.py, gen_corpus.py --seed
      ;; 20261127, report.py, run_all.sh) with results in results/REPORT.txt
      ;; plus per-phase .jsonl and 1 Hz .res.csv samples.  Re-measure after
      ;; any model or flag change rather than trusting these.
      ;;
      ;; THROUGHPUT, L200 (~178 tok/passage), GPU, best config:
      ;;   Qwen3-Embedding-0.6B  333 pass/s (B32, 2 clients)  = 59.2k tok/s
      ;;   bge-m3                517 pass/s                  = 113k  tok/s
      ;;   bge-base-en-v1.5     1768 pass/s                  = 323k  tok/s
      ;; Use PASSAGES/s for corpus wall-clock, NOT tok/s: bge-m3's tokenizer
      ;; emits ~24% more tokens on identical text, so tok/s overstates its
      ;; lead (1.88x in tok/s is only 1.53x in pass/s).
      ;; GPU batching is worth 6.2x over B1 (Qwen3 51.3 -> 322.4 pass/s).
      ;;
      ;; EXTRAPOLATED time to index N passages of ~200 tokens on GPU, Qwen3:
      ;; 100k = 5 min, 1M = 50 min, 10M = 8.3 h.  These EXCLUDE HNSW graph
      ;; construction, chunking, document I/O and DB writes, all of which are
      ;; real -- treat them as lower bounds.
      ;;
      ;; QUERY LATENCY, batch=1, ~20-token query, 400 iterations:
      ;;   Qwen3 GPU  p50 15.9 ms / p95 23.3 / p99 26.1
      ;;   Qwen3 CPU  p50 25.0 ms / p95 33.0 / p99 38.4
      ;;   bge-m3 GPU p50 10.1 ms / p99 19.2
      ;; Either is immaterial next to the LLM call that follows it.
      ;;
      ;; CONTENTION -- THE IMPORTANT OPERATIONAL FACT.  Sustained GPU
      ;; embedding load (L200, B128, 4 clients, ~329 pass/s, +1.96 GiB VRAM)
      ;; degrades ovms-qwen36 on :8093 by ~11x, measured with single small
      ;; chat probes before/during/after:
      ;;   before  non-stream 0.163 s, stream TTFT 0.066 s
      ;;   during  non-stream 1.79-1.96 s (4/4 probes), TTFT 0.31-0.42 s
      ;;   after   non-stream 0.151 s, TTFT 0.056 s  (recovered within 5 s)
      ;; Completion token counts were identical throughout, so correctness is
      ;; unaffected -- this is latency only, and it is fully reversible.  So a
      ;; bulk index job CAN run while the fleet LLM is live, but interactive
      ;; latency will be ~10x worse for the duration.  That makes indexing a
      ;; SCHEDULING question: 5 min for 100k is imperceptible, 50 min for 1M
      ;; is a coffee break, 8.3 h for 10M wants an overnight window.
      ;;
      ;; CPU IS NOT A VIABLE INDEXING DEVICE above ~100k passages, despite
      ;; the wedge being GPU-only.  Three independent reasons, all measured:
      ;;   * 42-50x slower.  Qwen3 CPU 7.3 pass/s vs 322 GPU -> 1M passages
      ;;     is 36 h instead of 50 min.  "Overnight" does not cover it.
      ;;   * It CANNOT BE PARALLELISED.  OVMS embeddings on CPU serialises:
      ;;     throughput at 1 client and 8 clients is identical to within 1%
      ;;     while p95 request latency goes 4.8 s -> 38.6 s.  Running 8
      ;;     indexing workers buys exactly nothing.
      ;;   * HOST-RAM RISK.  A single L1024xB128 request (131k tokens) drove
      ;;     ovms RSS to 19.5 GiB and host MemAvailable down to 928 MB on a
      ;;     box co-hosting a live 35B LLM, then died without completing.  No
      ;;     kernel OOM-kill fired -- that was luck, not design.  Cap CPU
      ;;     requests well below 70k nominal tokens if you ever do this.
      ;;
      ;; DO NOT SET --num_streams.  Measured harmful: ns2 is 19-30% WORSE
      ;; (Qwen3 CPU B32 1214 -> 869 tok/s; bge-m3 2008 -> 1396) and ns4 is
      ;; within noise at 1 client (+3-5%) for a 1.4-1.7x peak-RSS increase.
      ;; OVMS's own help text ("use at least 2 on 2 socket CPU systems") does
      ;; not transfer to this single-socket i9-14900KS.
      ;;
      ;; CONCURRENCY: use 2 clients for bulk work.  C1->C2 is the only gain
      ;; that exists (+7% Qwen3, +8% bge-m3, +29% bge-base); beyond that
      ;; throughput is dead flat to C8 while request latency inflates 5-11x,
      ;; i.e. pure queueing.
      ;;
      ;; VRAM: weights at rest are small (+0.66 GiB Qwen3, +0.62 bge-m3).
      ;; The large peaks are ACTIVATION memory from long-context x big-batch
      ;; combinations -- L1024xB128 peaked at +8.47 GiB.  At realistic
      ;; indexing settings (L200, B128, C4) Qwen3 costs only +1.96 GiB,
      ;; which fits the ~13 GiB free.  So: batch 128-256, 2 clients, and
      ;; LENGTH-BUCKET the corpus so 200-token chunks are never padded to
      ;; 1024.  Avoid L1024xB128 specifically.
      ;;
      ;; RELIABILITY: a bge-m3 GPU instance segfaulted once in ~8 exposures
      ;; (libstdc++, after 343 successful passages) and was NOT reproduced in
      ;; 7 further fresh-instance attempts over ~25k requests.  Intermittent,
      ;; not a reason to reject the model, but a reason to put a liveness
      ;; probe + auto-restart on the INDEXING JOB (a batch script can safely
      ;; retry; the auto-restart caution in the TODO above is about the
      ;; shared production servable, which is a different risk).  Note this
      ;; box has pre-existing OVMS-on-Arc crash history in dmesg unrelated to
      ;; any of this testing; neither production servable was affected.
      ;;
      ;; `model-name' is the CLIENT-VISIBLE id and is deliberately a STABLE
      ;; ALIAS rather than the IR directory name, unlike ovms-qwen36 above.
      ;; That is a considered departure: swapping the embedding model then
      ;; needs no client change, whereas pinning the IR name is precisely what
      ;; forced the Qwen3.6 migration to temporarily spoof its own model-name
      ;; to keep pi working (see ovms-qwen36's comment and the COUPLING TRAP
      ;; note in (peteches services ovms)).  Clients POSTing here must send
      ;; "model": "embeddings".
      ;;
      ;; The generation-only flags are NOT rendered for this instance, and
      ;; that is load-bearing.  OVMS rejects --pipeline_type and all three
      ;; Continuous-Batching booleans at the PARSE phase under
      ;; --task embeddings ("error parsing options - unmatched arguments :
      ;; --pipeline_type, AUTO, --enable_tool_guided_generation, false,
      ;; --enable_prefix_caching, true, --dynamic_split_fuse, true"), so
      ;; ovms-serve-args now gates all four on the task -- see
      ;; %ovms-generative-tasks in (peteches services ovms).  Without that
      ;; guard this instance could never start, and the failure is a bare
      ;; option-parse error that does not obviously point at the service
      ;; type.  qwen36's rendered command line is unchanged by that guard.
      ;;
      ;; auto-start? #t, so all three ovms instances come up on every boot.
      ;; The IR
      ;; is already on disk at model-path below (613 MB), downloaded and
      ;; proven end-to-end on port 8099 -- AVAILABLE, real /v3/embeddings
      ;; vectors, GPU-resident on /dev/dri/renderD128 (xe), not a silent CPU
      ;; fallback -- before auto-start was enabled here.  It is an
      ;; out-of-band artifact like every other ovms model, so there is
      ;; deliberately no sync service: if it is ever deleted from the box,
      ;; `herd stop ovms-embed' before rebooting, or it will crash-loop and
      ;; shepherd will disable it.
                (ovms-server-configuration
                 (service-name "ovms-embed")
                 (model-name "embeddings")
                 (model-path "/media/models/openvino/Qwen3-Embedding-0.6B-int8-ov")
                 (port 8094)
                 (host "0.0.0.0")
                 (target-device "GPU")
                 (task "embeddings")
                 (auto-start? #t)
                 (open-firewall? #t)
                 (firewall-sources '("100.64.0.0/10"
                                     "fd7a:115c:a1e0::/48"
                                     "fd7a:115c:a6e0::/48")))
                ;; ── OVMS reranker on the Arc Pro B70 (third instance) ────
                ;; OpenVINO/bge-reranker-v2-m3-int8-ov on port 8095.  The
                ;; second stage of retrieve-then-rerank: ovms-embed recalls
                ;; wide (50-100 candidates), this reorders them down to ~5.
                ;; See the CLIENT CONTRACT block above for why this is the
                ;; highest-value layer in the pipeline and the only one that
                ;; can be added, tuned or swapped later WITHOUT re-embedding
                ;; the corpus.
                ;;
                ;; WHY THIS WORKS WHEN NV-EMBED-V2 COULD NOT.  A reranker is
                ;; a *ForSequenceClassification model with a trained scalar
                ;; score head, so the model's own output IS the relevance
                ;; logit.  There is no hidden-state -> pooled-vector post-
                ;; processing chain to violate and no extra mask input to
                ;; supply -- which is precisely the contract
                ;; (rank-3 [batch,seq,hidden] plus OVMS's own CLS/LAST/MEAN
                ;; pooling) that made nvidia/NV-Embed-v2 unservable under
                ;; --task embeddings.  `task' is "rerank", verified against
                ;; this OVMS build's own --help task list.
                ;;
                ;; WHY bge-reranker-v2-m3 AND NOT Qwen3-Reranker-0.6B.  Both
                ;; are Apache-2.0 and both appear in OVMS's rerank demo
                ;; tested-models list, but: (a) the only Qwen3 seq-cls IR
                ;; published is FP16 at 1217.5 MB, roughly 2x this one's
                ;; 603.3 MB int8 -- the int8 Qwen3 IR that does exist is the
                ;; yes/no-logit build that OVMS rerank does NOT accept;
                ;; and (b) OVMS's own demo states the Qwen3 seq-cls export
                ;; exists because "original model is not supported in OVMS"
                ;; and that THE CLIENT MUST APPLY THE CHAT TEMPLATE ITSELF,
                ;; shipping a literal <|im_start|>system prefix/suffix.  That
                ;; would put template plumbing in every bot for no measured
                ;; gain.  bge-reranker-v2-m3 needs none of it.
                ;;
                ;; API IS COHERE-SHAPED, NOT OPENAI-SHAPED -- the single
                ;; biggest integration trap here.  POST /v3/rerank with
                ;; {model, query, documents[], top_n?, return_documents?}
                ;; returning {results:[{index, relevance_score, document?}]}.
                ;; With the `cohere' Python client set base_url to
                ;; http://<host>:8095/v3.  An OpenAI client will NOT work,
                ;; and neither will reusing the embeddings client code.
                ;; Unsupported from Cohere: rank_fields, and dict-form
                ;; documents (see below).
                ;;
                ;; MEASURED ON THIS BUILD 2026-11-27 on port 8098.  These
                ;; observations OVERRIDE the upstream docs, two of which are
                ;; simply wrong for OVMS 2026.4.1:
                ;;   * DOCUMENTS MUST BE PLAIN STRINGS.  The object form
                ;;     {"text":...,"title":...} always fails: `title' becomes
                ;;     mandatory the moment you send an object, and even a
                ;;     complete {text,title} (with or without `chunks') dies
                ;;     in the tokenizer with an unhandled INTERNAL
                ;;     `SpecialTokensSplit' exception.  bge-reranker-v2-m3 is
                ;;     XLMRobertaForSequenceClassification with a single
                ;;     LABEL_0 score head and has no title/chunks template.
                ;;     Consequence: the documented "10 chunks per document"
                ;;     feature is UNREACHABLE with this model -- do not
                ;;     design around it.
                ;;   * THE 100-DOCUMENT LIMIT IS NOT ENFORCED.  101, 500 and
                ;;     2000 documents all returned 200 (2000 docs in 138 ms).
                ;;     The real ceiling is the server-side
                ;;     `--max_allowed_chunks' (default 10000); 10001 docs
                ;;     gives 400 "Number of documents exceeds
                ;;     max_allowed_chunks".  Do NOT build client-side paging
                ;;     around a 100-doc assumption.
                ;;   * SCORES ARE SIGMOID 0-1, not raw logits (observed
                ;;     1.7e-5 to 0.9998); no normalisation flag exists or is
                ;;     needed.  `top_n' is honoured and results always come
                ;;     back score-sorted; top_n=0 returns {"results":[]}
                ;;     with 200 and a negative top_n behaves like omission,
                ;;     so validate client-side.  return_documents=true nests
                ;;     the text as document.text, NOT a bare string.
                ;;   * CALIBRATION WARNING: small positive scores are NOT
                ;;     weak relevance.  On a rules query the two relevant
                ;;     documents scored 0.993 and 0.983 while plausible-but-
                ;;     wrong rules scored <=0.002 -- but an EMPTY-STRING
                ;;     document scored 0.054, i.e. HIGHER than a real
                ;;     irrelevant rule.  The relevant/noise cut is therefore
                ;;     corpus-dependent and must be tuned on your own judged
                ;;     pairs; treat the whole 0.01-0.1 band as noise.
                ;;   * ROBUSTNESS -- nothing wedges this servable.  Unlike
                ;;     ovms-embed (see its KNOWN DEFECT note), back-to-back
                ;;     empty-string documents, whitespace-only documents,
                ;;     empty queries and over-context documents all return
                ;;     200 and it keeps serving normally.  Two inputs return
                ;;     400 WITHOUT wedging: a missing `query' field (clean
                ;;     INVALID_ARGUMENT) and `documents: []` (an ugly
                ;;     unhandled INTERNAL quoting intel_cpu/src/graph.cpp
                ;;     SpecialTokensSplit / basic_string::_M_create).  So
                ;;     guard against an empty candidate list, but a slip will
                ;;     not take the endpoint down.
                ;;   * CONTEXT is 8194 tokens (max_position_embeddings) and
                ;;     longer documents are SILENTLY TRUNCATED, not rejected:
                ;;     one ~12000-word document returned 200 in 955 ms.
                ;;     Latency scales with TOTAL tokens across all pairs (the
                ;;     query is repeated per document), so pre-chunking is
                ;;     what keeps p99 sane.
                ;;   * MEASURED LATENCY, warm, short rule-sized docs, on the
                ;;     Arc: 20 docs median 12 ms; 100 docs median 18 ms;
                ;;     500 docs 43 ms; 2000 docs 138 ms.  Cold first hit
                ;;     77-139 ms.  Sublinear at these sizes because all pairs
                ;;     batch into a few GPU passes.  A retrieve-100-then-
                ;;     rerank-to-5 loop therefore costs ~18 ms, which is
                ;;     noise next to the LLM call that follows it.
                ;;   * Optional knobs if ever needed: `--num_streams'
                ;;     (default 1) and `--max_allowed_chunks' (default
                ;;     10000).  There is NO --top_n, no score-normalisation
                ;;     flag and no max-documents flag.
                ;;
                ;; `model-name' is the stable alias "reranker", same
                ;; reasoning as ovms-embed's "embeddings": clients pin the
                ;; role, not the checkpoint, so a reranker swap is a
                ;; model-path change with no bot-side edit.  Send
                ;; "model": "reranker".
                ;;
                ;; The generation-only flags are omitted for this instance by
                ;; the same task gate that covers ovms-embed -- see
                ;; %ovms-generative-tasks in (peteches services ovms).  That
                ;; gate keys on "text_generation", so `embeddings' and
                ;; `rerank' both fall through to the reduced flag set without
                ;; either needing a special case here.
                ;;
                ;; VRAM, MEASURED via the xe node
                ;; /sys/kernel/debug/dri/0000:02:00.0/tile0/vram_mm: this
                ;; instance costs +633 MiB (663,617,536 B), and usage returned
                ;; to the exact prior figure once the test process was
                ;; killed.  With all three servables resident the card sits
                ;; at 20,639,469,568 B = 19.22 GiB of 31.9 GiB, leaving
                ;; ~12.7 GiB -- so the retrieval pair (embedder +668 MiB,
                ;; reranker +633 MiB, ~1.3 GiB together) is not a meaningful
                ;; tenant next to ovms-qwen36's 17.88 GiB.  Confirmed on the
                ;; Arc, not a CPU fallback: the test process held
                ;; /dev/dri/renderD128, whose driver symlink is
                ;; /sys/bus/pci/drivers/xe and whose card is PCI
                ;; 0000:02:00.0.  (renderD129 is the 4090 and was untouched.)
                ;; Caddy now proxies this as rerank.ts.peteches.co.uk (added
                ;; 2025-11-28), so external clients can reach it over TLS.
                ;; Fleet-internal bots that were already dialing
                ;; comfyui.spaniel-cordylus.ts.net:8095 over the tailnet
                ;; (already encrypted) continue to do so — the firewall below
                ;; still narrows it to the same three CIDRs, and the CNAME
                ;; for rerank.ts.peteches.co.uk points at caddy.spaniel-
                ;; cordylus.ts.net instead of here, so the direct path is
                ;; unchanged for existing consumers.
                ;;
                ;; auto-start? #t, and the IR is an out-of-band artifact like
                ;; every other ovms model: it must be present at model-path
                ;; before this is deployed or the instance crash-loops and
                ;; shepherd disables it.
                (ovms-server-configuration
                 (service-name "ovms-rerank")
                 (model-name "reranker")
                 (model-path "/media/models/openvino/bge-reranker-v2-m3-int8-ov")
                 (port 8095)
                 (host "0.0.0.0")
                 (target-device "GPU")
                 (task "rerank")
                 (auto-start? #t)
                 (open-firewall? #t)
                 (firewall-sources '("100.64.0.0/10"
                                     "fd7a:115c:a1e0::/48"
                                     "fd7a:115c:a6e0::/48")))))

      ;; NOTE: the `strata' service (SC117 abliterated Qwen3.8-Flash-Next on
      ;; the B70 via SYCL/Level Zero) was RETIRED here, replaced by the OVMS
      ;; instances above.  It is deliberately not kept as a manual-start
      ;; fallback: two GPU inference engines on one 31.9 GiB card is what
      ;; this box is trying to get away from, and OVMS's prefix caching
      ;; (8.2x on repeated prompts) plus its native streaming tool/reasoning
      ;; parsers make it the better backend for the fleet's agentic traffic.
      ;; Removed with it: its ~325-line configuration block, the (peteches
      ;; services strata) and (peteches packages pocl) imports, the strata
      ;; entry in the alloy log-files list, and the `strata' +
      ;; `pocl-tests-off' entries in #:extra-packages.  (peteches packages
      ;; strata) STAYS imported -- it defines the Intel Arc packages OVMS
      ;; needs; see the note on that import.  Recover any of the rest from
      ;; git history rather than reconstructing it.  The Intel
      ;; OpenCL/IGC/Level-Zero packages in #:extra-packages STAY too --
      ;; those are OVMS's deps, not strata's; see the DO-NOT-TRIM note
      ;; there.
      ;; /lib64 -> glibc/lib: non-store binaries carrying
      ;; PT_INTERP = /lib64/ld-linux-x86-64.so.2, an FHS path that does not
      ;; exist on a pure-Guix system.  The symlink makes the dynamic loader
      ;; path resolvable so the kernel can load them.  The Strata bundle's
      ;; private CPython and downloaded Intel engine were the original
      ;; reason and are gone with that service; the CUDA toolchain pulled
      ;; in above (and anything else the ComfyUI sandbox fetches
      ;; out-of-band) still assumes the FHS path, so this stays.
      ;; (Runtime *library* resolution for a non-store binary is handled
      ;; separately via LD_LIBRARY_PATH -- such a binary cannot use Guix's
      ;; per-package ld.so.cache, which is looked up relative to the
      ;; executable.)
      (simple-service 'lib64-symlink
                      activation-service-type
                      #~(begin
                          (use-modules (guix build utils))
                          (if (file-exists? "/lib64")
                              (delete-file "/lib64")
                              #f)
                          (symlink (string-append #$glibc "/lib") "/lib64"))))))))

comfyui-os
