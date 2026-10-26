;; peteches/packages/zoom.scm
;;
;; Zoom 7.1.5.4332, defined directly (not inherited from the pinned nonguix
;; `zoom', which is 5.17.5.2543).
;;
;; Why 7.x: 5.17.5 has a memory-corruption bug in its SSO URL handling.
;; Confirmed live on dagon (2026-10-24): every 5.17.5 instance overwrites its
;; own process environment block in memory at startup -- /proc/<pid>/environ
;; shows the SSO URL (when launched with one) or a path string (when not) in
;; place of the real environment, with the remainder NUL. After that
;; corruption the SSO token exchange stalls (the app opens a TLS connection to
;; Google, receives the response, then never processes it -- the socket is left
;; in CLOSE-WAIT with unread data) and the client is stuck on the login screen
;; forever, regardless of how it is launched or whether the browser is signed
;; in. 7.1.5 does not exhibit the corruption (environ is intact) and carries
;; the SSO/IPC fixes from the Nix packagers (binary rename, zopen relative-path
;; handling, script rewrites) that the guix build applies.
;;
;; This is the nonguix master definition of `zoom' (7.1.5.4332) as of
;; 2026-10-24, verbatim (source hash included -- it matches the live
;; cdn.zoom.us file). It is defined locally rather than inherited from the
;; pinned nonguix `zoom' (5.17.5.2543) so that only this package moves to 7.x
;; without a channel-wide nonguix pin update.
;;
;; The previous local package (a thin inherit adding the CEF/Qt lib paths to
;; LD_LIBRARY_PATH) is no longer needed: the 7.1.5 definition already adds the
;; package's own output directories (lib/zoom, lib/zoom/cef, lib/zoom/Qt/lib)
;; via its patchelf-plan and script rewrites.

(define-module (peteches packages zoom)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module ((nonguix licenses) :prefix license:)
  #:use-module (nonguix build-system binary))

(define-public zoom
  (package
    (name "zoom")
    (version "7.1.5.4332")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cdn.zoom.us/prod/" version "/zoom_x86_64.tar.xz"))
       (file-name (string-append name "-" version "-x86_64.tar.xz"))
       (sha256
        (base32 "0h21jfjr1nplzsllzavqjzrk67i8g2mz5r33lmi3dqdcbnb37v0p"))))
    (supported-systems '("x86_64-linux"))
    (build-system binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:patchelf-plan
           ;; Note: it seems like some (all?) of these only do anything in
           ;; LD_LIBRARY_PATH, or at least needed there as well.
           #~(let ((libs '("alsa-lib"
                           "at-spi2-core"
                           "cairo"
                           "cups"
                           "dbus"
                           "eudev"
                           "expat"
                           "fontconfig-minimal"
                           "freetype"
                           "gcc"
                           "glib"
                           "gtk+"
                           "libdrm"
                           "libglvnd"
                           "libx11"
                           "libxcb"
                           "libxcomposite"
                           "libxcursor"
                           "libxdamage"
                           "libxext"
                           "libxfixes"
                           "libxi"
                           "libxkbcommon"
                           "libxkbfile"
                           "libxrandr"
                           "libxrender"
                           "libxshmfence"
                           "libxtst"
                           "mesa"
                           "nspr"
                           "pango"
                           "pulseaudio"
                           "xcb-util"
                           "xcb-util-cursor"
                           "xcb-util-image"
                           "xcb-util-keysyms"
                           "xcb-util-renderutil"
                           "xcb-util-wm"
                           "zlib"
                           "zstd"
                           ("out" "/lib/zoom")
                           ("out" "/lib/zoom/cef")
                           ("out" "/lib/zoom/Qt/lib")
                           ("nss" "/lib/nss"))))
               `(("lib/zoom/ZoomLauncher"
                 ,libs)
                ("lib/zoom/zoom"
                 ,libs)
                ("lib/zoom/zopen"
                 ,libs)
                ("lib/zoom/aomhost"
                 ,libs)
                ("lib/zoom/ZoomWebviewHost"
                 ,libs)
                ("lib/zoom/ZoomClips"
                 ,libs)
                ("lib/zoom/cpthost"
                 ,libs)
                ("lib/zoom/libaomagent.so"
                 ,libs)
                ("lib/zoom/cef/libcef.so"
                 ,libs)
                ("lib/zoom/Qt/lib/libQt6Core.so.6"
                 ,libs)
                ("lib/zoom/Qt/lib/libQt6DBus.so.6"
                 ,libs)
                ("lib/zoom/Qt/lib/libQt6Gui.so.6"
                 ,libs)
                ("lib/zoom/Qt/lib/libQt6Network.so.6"
                 ,libs)
                ("lib/zoom/Qt/lib/libQt6Widgets.so.6"
                 ,libs)
                ("lib/zoom/Qt/lib/libQt6XcbQpa.so.6"
                 ,libs)
                ("lib/zoom/Qt/plugins/platforms/libqxcb.so"
                 ,libs)
                ("lib/zoom/Qt/plugins/platforms/libqminimal.so"
                 ,libs)
                ("lib/zoom/Qt/plugins/platforms/libqvkkhrdisplay.so"
                 ,libs)))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (invoke "tar" "xvf" source)
                   ;; Use the more standard lib directory for everything.
                   (mkdir-p "lib")
                   (rename-file "zoom/" "lib/zoom")))
               (add-after 'install 'wrap-where-patchelf-does-not-work
                 (lambda _
                   (wrap-program (string-append #$output "/lib/zoom/zopen")
                     `("LD_LIBRARY_PATH" prefix
                       ,(list #$@(map (lambda (pkg)
                                        (file-append (this-package-input pkg) "/lib"))
                                      '("fontconfig-minimal"
                                        "freetype"
                                        "gcc"
                                        "glib"
                                        "libxcomposite"
                                        "libxdamage"
                                        "libxkbcommon"
                                        "libxkbfile"
                                        "libxrandr"
                                        "libxrender"
                                        "zlib")))))
                   (wrap-program (string-append #$output "/lib/zoom/zoom")
                     '("QML2_IMPORT_PATH" = ())
                     '("QT_PLUGIN_PATH" = ())
                     '("QT_SCREEN_SCALE_FACTORS" = ())
                     `("FONTCONFIG_PATH" ":" prefix
                       (,(string-join
                          (list
                           (string-append #$(this-package-input "fontconfig-minimal") "/etc/fonts")
                           #$output)
                          ":")))
                     `("LD_LIBRARY_PATH" prefix
                       ,(list (string-append #$(this-package-input "nss") "/lib/nss")
                              #$@(map (lambda (pkg)
                                        (file-append (this-package-input pkg) "/lib"))
                                      ;; aomhost inherits this environment.
                                      ;; Do not wrap it: Zoom passes its
                                      ;; resource directory through argv[0].
                                      '("alsa-lib"
                                        "at-spi2-core"
                                        "cairo"
                                        "cups"
                                        "dbus"
                                        "eudev"
                                        "expat"
                                        "gcc"
                                        "glib"
                                        "mesa"
                                        "mit-krb5"
                                        "nspr"
                                        "libxcb"
                                        "libxcomposite"
                                        "libxdamage"
                                        "libxext"
                                        "libxkbcommon"
                                        "libxkbfile"
                                        "libxrandr"
                                        "libxshmfence"
                                        "pango"
                                        "pulseaudio"
                                        "xcb-util"
                                        "xcb-util-image"
                                        "xcb-util-keysyms"
                                        "xcb-util-wm"
                                        "xcb-util-renderutil"
                                        "zlib")))))
                   (let* ((zoom-directory
                           (string-append #$output "/lib/zoom"))
                          (qt-directory
                           (string-append zoom-directory "/Qt"))
                          (qt-library-directory
                           (string-append qt-directory "/lib"))
                          (qt-plugin-directory
                           (string-append qt-directory "/plugins"))
                          (qt-qml-directory
                           (string-append qt-directory "/qml"))
                          (xcb-cursor-library-directory
                           (string-append
                            #$(this-package-input "xcb-util-cursor")
                            "/lib")))
                     (for-each
                      (lambda (program)
                        (substitute* program
                          (("export QML2_IMPORT_PATH=\"\"")
                           (string-append "export QML2_IMPORT_PATH=\""
                                          qt-qml-directory "\""))
                          (("export QT_PLUGIN_PATH=\"\"")
                           (string-append "export QT_PLUGIN_PATH=\""
                                          qt-plugin-directory "\""))
                          (("export LD_LIBRARY_PATH=\"")
                           (string-append
                            "export LD_LIBRARY_PATH=\""
                            qt-library-directory ":" zoom-directory ":"
                            xcb-cursor-library-directory ":"))))
                      (list (string-append zoom-directory "/zoom")
                            (string-append zoom-directory "/zopen")))
                     ;; Zoom starts zopen via a relative path for SSO.
                     (substitute* (string-append zoom-directory "/zoom")
                       (("^exec -a")
                        (string-append "cd \"" zoom-directory
                                       "\"\nexec -a"))))))
               (add-after 'wrap-where-patchelf-does-not-work 'rename-binary
                 ;; IPC (for single sign-on and handling links) fails if the
                 ;; name does not end in "zoom," so rename the real binary.
                 ;; Thanks to the Nix packagers for figuring this out.
                 (lambda _
                   (rename-file (string-append #$output "/lib/zoom/.zoom-real")
                                (string-append #$output "/lib/zoom/.zoom"))
                   (substitute* (string-append #$output "/lib/zoom/zoom")
                     (("zoom-real")
                      "zoom"))))
               (add-after 'rename-binary 'symlink-binaries
                 (lambda _
                   (delete-file (string-append #$output "/environment-variables"))
                   (mkdir-p (string-append #$output "/bin"))
                   (symlink (string-append #$output "/lib/zoom/aomhost")
                            (string-append #$output "/bin/aomhost"))
                   (symlink (string-append #$output "/lib/zoom/zoom")
                            (string-append #$output "/bin/zoom"))
                   (symlink (string-append #$output "/lib/zoom/zopen")
                            (string-append #$output "/bin/zopen"))
                   (symlink (string-append #$output "/lib/zoom/ZoomLauncher")
                            (string-append #$output "/bin/ZoomLauncher"))))
               (add-after 'symlink-binaries 'create-desktop-file
                 (lambda _
                   (let ((apps (string-append #$output "/share/applications")))
                     (mkdir-p apps)
                     (make-desktop-entry-file
                      (string-append apps "/zoom.desktop")
                      #:name "Zoom"
                      #:generic-name "Zoom Client for Linux"
                      #:exec (string-append #$output "/bin/ZoomLauncher %U")
                      #:mime-type (list
                                   "x-scheme-handler/zoommtg"
                                   "x-scheme-handler/zoomus"
                                   "x-scheme-handler/tel"
                                   "x-scheme-handler/callto"
                                   "x-scheme-handler/zoomphonecall"
                                   "application/x-zoom")
                      #:categories '("Network" "InstantMessaging"
                                     "VideoConference" "Telephony")
                      #:startup-w-m-class "zoom"
                      #:comment
                      '(("en" "Zoom Video Conference")
                        (#f "Zoom Video Conference")))))))))
    (native-inputs (list tar))
    (inputs (list alsa-lib
                  at-spi2-core
                  bash-minimal
                  cairo
                  cups
                  dbus
                  eudev
                  expat
                  fontconfig
                  freetype
                  `(,gcc "lib")
                  glib
                  gtk+
                  libdrm
                  libglvnd
                  librsvg
                  libx11
                  libxcb
                  libxcomposite
                  libxdamage
                  libxext
                  libxfixes
                  libxkbcommon
                  libxkbfile
                  libxrandr
                  libxrender
                  libxshmfence
                  mesa
                  mit-krb5
                  nspr
                  nss
                  pango
                  pulseaudio
                  xcb-util
                  xcb-util-cursor
                  xcb-util-image
                  xcb-util-keysyms
                  xcb-util-renderutil
                  xcb-util-wm
                  zlib
                  `(,zstd "lib")))
    (home-page "https://zoom.us/")
    (synopsis "Video conference client")
    (description "The Zoom video conferencing and messaging client.  Zoom must be run via an
app launcher to use its .desktop file, or with @code{ZoomLauncher}.")
    (license (license:nonfree "https://explore.zoom.us/en/terms/"))))
