(define-module (peteches packages lycheeslicer)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnome)   ; zenity (native dialog helper, see inputs)
  #:use-module (nongnu packages engineering))

;; Upstream stopped publishing .deb releases after 7.6.6; 8.0.0+ ship as an
;; AppImage only (LycheeSlicer-<version>-x86_64.GLIBC-<glibc>.AppImage), and
;; the app moved from Electron to CEF (libcef.so + LycheeSlicerHelper
;; subprocesses, payload under opt/Lychee).  So this package no longer
;; piggybacks on nonguix's deb-unpacking phases: 'binary-unpack is replaced
;; with an unsquashfs-based extraction that rearranges the AppImage tree into
;; the same lib/LycheeSlicer + share layout the .deb used, and everything
;; downstream (chromium-binary-build-system's patchelf + install-wrapper, the
;; bin symlink, the inner LD_LIBRARY_PATH wrapper) works as before.
(define-public lycheeslicer-8.0.2
  (package
    (inherit lycheeslicer)
    (version "8.0.2")
    ;; mesa is already in the transitive graph (via gtk+).  Adding it
    ;; explicitly gives stable store paths for the wrapper's GL fallback
    ;; and the Vulkan ICD directory (see add-gl-fallback-to-bin-wrapper).
    ;;
    ;; zenity: the Emeraude engine implements every native dialogue (file
    ;; open/save via `zenity --file-selection`, plus --entry/--question/
    ;; --password/--notification, kdialog on KDE) by exec'ing an external
    ;; helper found on PATH -- it does NOT use CEF's RunFileDialog or the
    ;; XDG portal.  Regular distros always ship zenity; Guix does not, and
    ;; with neither zenity nor kdialog present hasZenity()/hasKdialog()
    ;; fail and dialogues silently do nothing (import/save appear dead,
    ;; nothing logged anywhere).  install-wrapper below prepends every
    ;; input's bin/ to the wrapper's PATH, so adding zenity here is all
    ;; that is needed -- no system change, and it travels with the package.
    (inputs (modify-inputs (package-inputs lycheeslicer)
              (append mesa zenity)))
    (native-inputs (list squashfs-tools))
    (arguments
     (list
      ;; Bundled CEF libs (libcef.so & co) resolve via $ORIGIN runpaths and
      ;; the LD_LIBRARY_PATH set up by the wrappers below, which the
      ;; runpath validator does not model.
      #:validate-runpath? #f
      ;; patchelf these to the Guix glibc interpreter and give them an RPATH
      ;; covering the implicit chromium inputs: the main binary, the CEF
      ;; subprocess helper, the sandbox helper (never used setuid here -- the
      ;; desktop entry passes --no-sandbox -- but it must at least load), and
      ;; the lgsconverter plugin binary.
      #:wrapper-plan
      #~'("lib/LycheeSlicer/LycheeSlicer"
          "lib/LycheeSlicer/LycheeSlicerHelper"
          "lib/LycheeSlicer/chrome-sandbox"
          "lib/LycheeSlicer/plugins/longer/lgsconverter")
      #:phases
      #~(modify-phases %standard-phases
          ;; The source is an AppImage: a statically linked ELF runtime with
          ;; a zstd-compressed squashfs image appended.  Locate the superblock
          ;; and extract with unsquashfs (no FUSE, no executing the vendored
          ;; runtime), then rearrange into the historic .deb layout.
          (replace 'binary-unpack
            (lambda* (#:key source #:allow-other-keys)
              (use-modules (ice-9 popen)
                           (ice-9 rdelim)
                           (srfi srfi-1))
              (define (candidate-offsets file)
                ;; Every byte offset of the little-endian squashfs magic in
                ;; FILE.  The runtime blob can contain spurious matches (it
                ;; does for 8.0.2: one at ~194KiB before the real superblock
                ;; at ~944KiB), so each candidate is validated by trying to
                ;; extract it.
                (let ((port (open-pipe* OPEN_READ
                                        "grep" "-aob" "hsqs" file)))
                  (let loop ((offsets '()))
                    (let ((line (read-line port)))
                      (if (eof-object? line)
                          (begin
                            (close-pipe port)
                            (reverse offsets))
                          (loop (cons (string->number
                                       (car (string-split line #\:)))
                                      offsets)))))))
              (mkdir "binary")
              (chdir "binary")
              (unless (any (lambda (offset)
                             (zero? (system* "unsquashfs" "-f" "-q"
                                             "-offset" (number->string offset)
                                             "-d" "." source)))
                           (candidate-offsets source))
                (error "no valid squashfs superblock found in AppImage"))
              ;; opt/Lychee + usr/share + AppRun shim -> lib/ + share/, the
              ;; layout the .deb releases (and nonguix's phases) used.
              (mkdir-p "lib")
              (rename-file "opt/Lychee" "lib/LycheeSlicer")
              (mkdir-p "share")
              (copy-recursively "usr/share" "share")
              (delete-file-recursively "opt")
              (delete-file-recursively "usr")
              ;; AppImage runtime shims, useless in the store; the desktop
              ;; file and icon live on under share/ (duplicates of these).
              (delete-file "AppRun")
              (delete-file "LycheeSlicer.desktop")
              (delete-file "LycheeSlicer.svg")
              #t))
          ;; The AppImage desktop file runs "Exec=AppRun %u", where AppRun
          ;; cds into the app dir and execs ./LycheeSlicer --no-sandbox.
          ;; Launchers like wofi have no ambient LD_LIBRARY_PATH, so point
          ;; Exec at the full guix wrapper in bin/ instead (with the
          ;; --no-sandbox the chrome-sandbox would otherwise require, since
          ;; it cannot be setuid in the store), and restore the
          ;; StartupWMClass the .deb shipped for window grouping.
          (add-after 'binary-unpack 'fix-desktop-file
            (lambda _
              (rename-file "share/applications/LycheeSlicer.desktop"
                           "share/applications/lycheeslicer.desktop")
              (substitute* "share/applications/lycheeslicer.desktop"
                (("Exec=AppRun %u")
                 (string-append "Exec=" #$output
                                "/bin/lycheeslicer --no-sandbox %U"))
                (("Terminal=false")
                 "Terminal=false\nStartupWMClass=LycheeSlicer"))))
          (add-before 'install-wrapper 'symlink-binary-file
            (lambda _
              (mkdir-p (string-append #$output "/bin"))
              (symlink (string-append #$output "/lib/LycheeSlicer/LycheeSlicer")
                       (string-append #$output "/bin/lycheeslicer"))))
          ;; The main binary's bundled DT_NEEDED libs (libcef.so,
          ;; libEmeraude.so.1, ...) live next to it and are not covered by
          ;; the patchelf RPATH; prepend the app dir to LD_LIBRARY_PATH.
          (add-after 'install-wrapper 'wrap-where-patchelf-does-not-work
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (wrap-program (string-append out "/lib/LycheeSlicer/LycheeSlicer")
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append out "/lib/LycheeSlicer")))))))
          ;; Fail loudly instead of hanging: libEmeraude reads the machine
          ;; UUID from /var/lib/dbus/machine-id, which Guix System never
          ;; creates (no systemd; the base.scm dbus-machine-id activation
          ;; service provides it).  Without it the license-check promise
          ;; rejects with "Machine UUID not available !" and the UI spins
          ;; on "Check licenses" forever with nothing logged.  This wrapper
          ;; is what wofi launches and stderr is lost there, so also try a
          ;; desktop notification (notify-send comes from libnotify, which
          ;; is already on the wrapper's PATH via the chromium inputs)
          ;; before exiting.
          (add-after 'install-wrapper 'add-machine-id-precheck-to-bin-wrapper
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((wrapper (string-append (assoc-ref outputs "out")
                                            "/bin/lycheeslicer")))
                (substitute* wrapper
                  (("exec -a")
                   (string-append
                    "if [ ! -r /var/lib/dbus/machine-id ]; then\n"
                    "  __msg=\"LycheeSlicer needs /var/lib/dbus/machine-id, which is missing; the license check would hang forever. Fix: sudo mkdir -p /var/lib/dbus && sudo cp /etc/machine-id /var/lib/dbus/machine-id\"\n"
                    "  echo \"$__msg\" >&2\n"
                    "  if command -v notify-send >/dev/null 2>&1; then\n"
                    "    notify-send -u critical -a LycheeSlicer \"LycheeSlicer cannot start\" \"$__msg\" >/dev/null 2>&1 || true\n"
                    "  fi\n"
                    "  exit 1\n"
                    "fi\n"
                    "exec -a")))
                #t)))
          ;; CEF dlopens GL/Vulkan drivers (ANGLE's desktop-GL backend wants
          ;; libGL.so.1).  If none is reachable it silently renders on the
          ;; CPU via the bundled SwiftShader.  But we must NOT
          ;; unconditionally add mesa to LD_LIBRARY_PATH: on NVIDIA machines
          ;; (dagon) the session may provide the vendor libGL/Vulkan ICDs,
          ;; and mesa would shadow them with llvmpipe (also CPU).  So:
          ;; prefer vendor drivers from the ambient environment, fall back
          ;; to mesa (correct for AMD, harmless llvmpipe elsewhere).
          ;; Bundled CEF libs still take precedence over both: the inner
          ;; wrapper prepends the app dir.
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
                #t))))))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://mango-lychee.nyc3.cdn.digitaloceanspaces.com/LycheeSlicer-"
             version "-x86_64.GLIBC-235.AppImage"))
       (sha256
        (base32 "07l0bc76lwgcjbqmvfrrpk0rs7bzy8n3nbapli3v32dban4201wd"))))))
