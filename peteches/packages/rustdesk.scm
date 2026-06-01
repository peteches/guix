(define-module (peteches packages rustdesk)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gcc))

;;; ── rustdesk-server ──────────────────────────────────────────────────────
;;
;; The release zip contains three statically-linked musl binaries:
;;   amd64/hbbs          — rendezvous / ID server
;;   amd64/hbbr          — relay server
;;   amd64/rustdesk-utils — CLI utility bundled with the server release
;; No ELF patching or glibc wrapper is required.

(define-public rustdesk-server
  (package
    (name "rustdesk-server")
    (version "1.1.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/rustdesk/rustdesk-server/releases/download/"
             version "/rustdesk-server-linux-amd64.zip"))
       (sha256
        (base32 "155k8qmd657v3jg74j27dn602ppf91ppgcwfq55j5h24v0prfly5"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (arguments
     (list
      #:install-plan
      #~'(("amd64/hbbs"          "bin/hbbs")
          ("amd64/hbbr"          "bin/hbbr")
          ("amd64/rustdesk-utils" "bin/rustdesk-utils"))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://rustdesk.com/")
    (synopsis "RustDesk self-hosted relay and ID servers")
    (description
     "RustDesk server provides @command{hbbs} (rendezvous/ID server) and
@command{hbbr} (relay server) for self-hosted RustDesk deployments.
Both are statically-linked musl binaries with no runtime dependencies.")
    (license agpl3)))

;;; ── rustdesk (client) ────────────────────────────────────────────────────
;;
;; Upstream .deb layout (from data.tar.xz):
;;   usr/share/rustdesk/rustdesk   — main Flutter/GTK3 binary
;;   usr/share/rustdesk/lib/       — bundled Flutter plugin .so files + libapp.so
;;   usr/share/rustdesk/data/      — Flutter assets
;;
;; The binary is dynamically linked.  Runtime library dependencies (verified
;; via ldd on lib/librustdesk.so):
;;   bundled (lib/):    libapp.so, libflutter_linux_gtk.so, plugin .so files
;;   glibc:             libdl, libm, libc
;;   gtk+:              libgtk-3, libgdk-3
;;   pango:             libpangocairo-1.0, libpango-1.0
;;   at-spi2-core:      libatk-1.0
;;   cairo:             libcairo, libcairo-gobject
;;   gdk-pixbuf:        libgdk_pixbuf-2.0
;;   glib:              libglib-2.0, libgobject-2.0, libgio-2.0
;;   wayland:           libwayland-client
;;   pulseaudio:        libpulse-simple, libpulse
;;   libxtst:           libXtst
;;   libxcb:            libxcb, libxcb-shm, libxcb-randr
;;   libx11:            libX11
;;   libxfixes:         libXfixes
;;   libxkbcommon:      libxkbcommon
;;   gstreamer:         libgstreamer-1.0
;;   gst-plugins-base:  libgstvideo-1.0, libgstapp-1.0
;;   zlib:              libz
;;   dbus:              libdbus-1
;;   linux-pam:         libpam
;;   gcc:lib:           libstdc++.so.6, libgcc_s.so.1
;;
;; The binary's ELF interpreter is patched to point to the Guix glibc
;; ld-linux so it can be exec'd directly (required because Flutter reads
;; /proc/self/exe to locate libapp.so and lib/).  A wrapper at bin/rustdesk
;; sets LD_LIBRARY_PATH to expose all Guix store libraries.
;;
;; Old-style named inputs are used so the wrapper can look up each entry by a
;; fixed key regardless of which gcc version Guix's `gcc' alias points to.

(define-public rustdesk
  (package
    (name "rustdesk")
    (version "1.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/rustdesk/rustdesk/releases/download/"
             version "/rustdesk-" version "-x86_64.deb"))
       (sha256
        (base32 "014giif43vrxgclkn780f5rgmh8hk8qp8grjspg848i5gdx6v90d"))))
    (build-system copy-build-system)
    (native-inputs (list binutils patchelf))
    (inputs
     `(("glibc"             ,glibc)
       ("gtk+"               ,gtk+)
       ("pango"              ,pango)
       ("at-spi2-core"       ,at-spi2-core)
       ("cairo"              ,cairo)
       ("gdk-pixbuf"         ,gdk-pixbuf)
       ("glib"               ,glib)
       ("wayland"            ,wayland)
       ("pulseaudio"         ,pulseaudio)
       ("libxtst"            ,libxtst)
       ("libxcb"             ,libxcb)
       ("libx11"             ,libx11)
       ("libxfixes"          ,libxfixes)
       ("libxkbcommon"       ,libxkbcommon)
       ("gstreamer"          ,gstreamer)
       ("gst-plugins-base"   ,gst-plugins-base)
       ("zlib"               ,zlib)
       ("dbus"               ,(@@ (gnu packages glib) dbus))
       ("linux-pam"          ,linux-pam)
       ("gcc:lib"            ,gcc "lib")))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "ar" "x" source)
              (invoke "tar" "xJf" "data.tar.xz")))
          (delete 'validate-runpath)
          (delete 'strip)
          (add-after 'install 'patch-elf
            ;; Patch the binary's ELF interpreter so it can be exec'd directly.
            ;; Flutter reads /proc/self/exe to locate lib/libapp.so; running via
            ;; `ld-linux --library-path` makes /proc/self/exe point to ld.so and
            ;; causes FlutterEngineCreateAOTData to fail with kInvalidArguments.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out    (assoc-ref outputs "out"))
                     (libc   (assoc-ref inputs "glibc"))
                     (binary (string-append out "/share/rustdesk/rustdesk"))
                     (loader (string-append libc "/lib/ld-linux-x86-64.so.2")))
                (invoke "patchelf" "--set-interpreter" loader binary)
                ;; $ORIGIN/lib covers the bundled .so files in lib/.
                (invoke "patchelf" "--set-rpath" "$ORIGIN/lib" binary))))
          (add-after 'patch-elf 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (libc    (assoc-ref inputs "glibc"))
                     (gtk     (assoc-ref inputs "gtk+"))
                     (pango   (assoc-ref inputs "pango"))
                     (atk     (assoc-ref inputs "at-spi2-core"))
                     (cairo   (assoc-ref inputs "cairo"))
                     (pixbuf  (assoc-ref inputs "gdk-pixbuf"))
                     (glib    (assoc-ref inputs "glib"))
                     (wl      (assoc-ref inputs "wayland"))
                     (pulse   (assoc-ref inputs "pulseaudio"))
                     (xtst    (assoc-ref inputs "libxtst"))
                     (xcb     (assoc-ref inputs "libxcb"))
                     (x11     (assoc-ref inputs "libx11"))
                     (xfixes  (assoc-ref inputs "libxfixes"))
                     (xkb     (assoc-ref inputs "libxkbcommon"))
                     (gst     (assoc-ref inputs "gstreamer"))
                     (gstbase (assoc-ref inputs "gst-plugins-base"))
                     (zlib    (assoc-ref inputs "zlib"))
                     (dbus    (assoc-ref inputs "dbus"))
                     (pam     (assoc-ref inputs "linux-pam"))
                     (gcclib  (assoc-ref inputs "gcc:lib"))
                     (real    (string-append out  "/share/rustdesk/rustdesk"))
                     (wrapper (string-append out  "/bin/rustdesk"))
                     (libpath (string-join
                               (list (string-append libc    "/lib")
                                     (string-append gtk     "/lib")
                                     (string-append pango   "/lib")
                                     (string-append atk     "/lib")
                                     (string-append cairo   "/lib")
                                     (string-append pixbuf  "/lib")
                                     (string-append glib    "/lib")
                                     (string-append wl      "/lib")
                                     (string-append pulse   "/lib")
                                     (string-append xtst    "/lib")
                                     (string-append xcb     "/lib")
                                     (string-append x11     "/lib")
                                     (string-append xfixes  "/lib")
                                     (string-append xkb     "/lib")
                                     (string-append gst     "/lib")
                                     (string-append gstbase "/lib")
                                     (string-append zlib    "/lib")
                                     (string-append dbus    "/lib")
                                     (string-append pam     "/lib")
                                     (string-append gcclib  "/lib"))
                               ":")))
                (mkdir-p (string-append out "/bin"))
                (call-with-output-file wrapper
                  (lambda (port)
                    (format port
                            "#!/bin/sh\nexport LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"\nexec ~a \"$@\"\n"
                            libpath real)))
                (chmod wrapper #o755)))))
      #:install-plan
      #~'(("usr/share/rustdesk/"    "share/rustdesk/")
          ("usr/share/applications/" "share/applications/")
          ("usr/share/icons/"        "share/icons/"))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://rustdesk.com/")
    (synopsis "Open-source remote desktop software")
    (description
     "RustDesk is an open-source remote desktop application written in Rust.
It supports screen sharing, file transfer, and remote control.  This package
installs the Linux desktop client by patching the ELF interpreter to the Guix
glibc and setting LD_LIBRARY_PATH via a wrapper script.")
    (license agpl3)))
