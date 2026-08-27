(define-module (peteches packages gamescope)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (peteches build mesa-utils)
  #:export (gamescope))

;; Pinned to the commit the upstream nonguix packaging effort
;; (https://gitlab.com/nonguix/nonguix/-/merge_requests/452) verified
;; buildable.  Deliberately not the newest release: 3.14.3+ hits
;; https://github.com/ValveSoftware/gamescope/issues/1218.
(define %version "3.14.2")
(define %revision "0")
(define %commit "d0d23c4c3010c81add1bd90cbe478ce4a386e28d")

(define-public gamescope
  (package
    (name "gamescope")
    (version (git-version %version %revision %commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ValveSoftware/gamescope")
             (commit %commit)
             (recursive? #t)))
       (sha256
        (base32 "1sw2br3g16mird7jc7idbcxf5xxjmiyr6hjw3966s0nsv6bn8vb2"))
       (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (arguments
     (list
      #:modules '((guix build meson-build-system)
                  (guix build utils)
                  (peteches build mesa-utils))
      #:imported-modules `(,@%meson-build-system-modules
                            (peteches build mesa-utils))
      #:configure-flags
      #~(list
         ;; The vendored openvr CMake subproject declares a
         ;; cmake_minimum_required too old for Guix's CMake 4.x
         ;; ("Compatibility with CMake < 3.5 has been removed").  VR
         ;; support isn't needed here, so skip it instead of patching
         ;; openvr's build.
         "-Denable_openvr_support=false"
         ;; guix's meson-build-system passes --wrap-mode=nofallback by
         ;; default.  gamescope's own meson.build sets
         ;; force_fallback_for=wlroots,libliftoff,vkroots as a project
         ;; default, but an explicit --force-fallback-for on the command
         ;; line replaces that list rather than adding to it -- so the
         ;; original three have to be repeated here alongside stb (whose
         ;; wrap is patched in by 'patch-subprojects below).
         "--force-fallback-for=wlroots,libliftoff,vkroots,stb")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-subprojects
            (lambda _
              ;; glm: gamescope's C++ is written against the 0.9.9.8 API;
              ;; vendor that exact release via Meson's WrapDB instead of
              ;; risking the system glm 1.0+ ABI/API break.
              (invoke
               #+(file-append unzip "/bin/unzip")
               #+(origin
                   (method url-fetch)
                   (uri
                    "https://wrapdb.mesonbuild.com/v2/glm_0.9.9.8-2/get_patch")
                   (sha256
                    (base32
                     "0gfqg3j1kfhycg7bygdxxfhp1qarzxqlrk4j9sz893d2sgya2c6r")))
               "-d"
               "subprojects/packagefiles/")
              (copy-recursively "subprojects/packagefiles/glm-0.9.9.8"
                                 "subprojects/packagefiles/glm")
              ;; stb: guix packages each stb single-header release
              ;; separately; union the three gamescope's wrap expects.
              (patch-wrap-file
               "stb"
               #+(directory-union "stb" (list stb-image
                                               stb-image-write
                                               stb-image-resize)))
              (substitute* "subprojects/stb/meson.build"
                (("include_directories\\('\\.'\\)")
                 (string-append "include_directories('./include')")))
              ;; libdisplay-info: relax the '< 0.2.0' upper bound so the
              ;; 0.2.0 in this Guix generation satisfies it.
              (substitute* "src/meson.build"
                (("(version: \\['>= 0\\.0\\.0'), '< 0\\.2\\.0'(\\])"
                  _ left-part right-part)
                 (string-append left-part right-part)))
              ;; hwdata no longer splits a "pnp" output -- pnp.ids lives
              ;; under the single "out" output's share/hwdata.
              (substitute* "meson.build"
                (("warning\\('Building without hwdata pnp id support\\.'\\)")
                 (string-append
                  "add_project_arguments("
                  "'-DHWDATA_PNP_IDS=\"" #$hwdata "/share/hwdata\"',"
                  "language: 'cpp'"
                  ")"))))))))
    (native-inputs
     (list cmake
           pkg-config
           python))
    (inputs
     (list clang
           eudev
           gcc-toolchain-12
           glm
           glslang
           libdecor
           libdisplay-info
           libdrm
           libinput
           libseat
           libx11
           libxcomposite
           libxcursor
           libxdamage
           libxext
           libxkbcommon
           libxmu
           libxres
           libxt
           libxtst
           libxxf86vm
           ;; openvr ;does not build when included
           pipewire
           pixman
           sdl2
           vulkan-headers
           vulkan-loader
           wayland
           wayland-protocols
           xcb-util-wm
           xorg-server-xwayland))
    (home-page "https://github.com/ValveSoftware/gamescope")
    (synopsis "Session compositing window manager")
    (description "Gamescope is a Wayland compositor for running games,
formerly known as steamcompmgr.  It is designed for use in embedded sessions
and as a nested compositor on top of a regular desktop environment through
sandboxed Xwayland sessions.")
    (license license:gpl3+)))
