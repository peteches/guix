(define-module (peteches packages lycheeslicer)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages gl)
  #:use-module (nongnu packages engineering))

(define-public lycheeslicer-7.6.2
  (package
    (inherit lycheeslicer)
    (version "7.6.2")
    ;; mesa is already in the transitive graph (via gtk+).  Adding it
    ;; explicitly gives stable store paths for the wrapper's GL fallback
    ;; and the Vulkan ICD directory (see add-gl-fallback-to-bin-wrapper).
    (inputs (modify-inputs (package-inputs lycheeslicer)
              (append mesa)))
    (arguments
     (substitute-keyword-arguments (package-arguments lycheeslicer)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; The .desktop file installed by nonguix points Exec at
            ;; lib/LycheeSlicer/lycheeslicer, the inner wrapper that only
            ;; prepends its own dir to LD_LIBRARY_PATH.  Launchers like wofi
            ;; have no ambient LD_LIBRARY_PATH, so the binary dies instantly
            ;; (libnss3.so not found).  Point Exec at the full guix wrapper
            ;; in bin/ instead.
            (add-after 'fix-desktop-file 'point-desktop-at-wrapper
              (lambda _
                (substitute* '("share/applications/lycheeslicer.desktop")
                  (((string-append #$output "/lib/LycheeSlicer/lycheeslicer"))
                   (string-append #$output "/bin/lycheeslicer")))))
            ;; Electron 12 (Chromium 89, ANGLE default backend = desktop
            ;; GL) dlopens libGL.so.1.  If no GL driver is reachable it
            ;; silently renders on the CPU via bundled SwiftShader.  But we
            ;; must NOT unconditionally add mesa to LD_LIBRARY_PATH: on
            ;; NVIDIA machines (dagon) the session may provide the vendor
            ;; libGL/Vulkan ICDs, and mesa would shadow them with llvmpipe
            ;; (also CPU) -- which is presumably why mesa used to be
            ;; stripped here.  So: prefer vendor drivers from the ambient
            ;; environment, fall back to mesa (correct for AMD, harmless
            ;; llvmpipe elsewhere).  Bundled Electron libs still take
            ;; precedence over both: the inner wrapper prepends the app dir.
            (add-after 'install-wrapper 'add-gl-fallback-to-bin-wrapper
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (wrapper (string-append out "/bin/lycheeslicer"))
                       (mesa #$(this-package-input "mesa")))
                  (substitute* wrapper
                    (("exec -a")
                     (string-append
                      "__mesa_lib=\"" mesa "/lib\"\n"
                      "__have_libgl=\n"
                      "__old_ifs=$IFS; IFS=:\n"
                      "for __d in $LD_LIBRARY_PATH; do\n"
                      "  [ -e \"$__d/libGL.so.1\" ] && { __have_libgl=1; break; }\n"
                      "done\n"
                      "IFS=$__old_ifs\n"
                      "[ -n \"$__have_libgl\" ] || "
                      "export LD_LIBRARY_PATH=\"$__mesa_lib"
                      "${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH\"\n"
                      "if [ -z \"$VK_DRIVER_FILES\" ]; then\n"
                      "  __icds=\n"
                      "  for __f in \"" mesa "/share/vulkan/icd.d/\"*.json; do\n"
                      "    __icds=\"${__icds:+$__icds:}$__f\"\n"
                      "  done\n"
                      "  export VK_DRIVER_FILES=\"$__icds\"\n"
                      "  export VK_ICD_FILENAMES=\"${VK_ICD_FILENAMES:-$__icds}\"\n"
                      "fi\n"
                      "exec -a")))
                  #t)))))))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://mango-lychee.nyc3.cdn.digitaloceanspaces.com/LycheeSlicer-"
             version ".deb"))
       (sha256
        (base32 "0vlr2mxb0h957ww6rifh84d880lpn12nhxid78ljk26ks371h6il"))))))
