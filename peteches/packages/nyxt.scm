(define-module (peteches packages nyxt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages node-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (guix build-system)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system node)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages webkit)
  #:use-module (nongnu packages electron))

;;; ─── Pinned upstream dependency overrides ─────────────────────────────────
;;; Nyxt 4.0.0 requires commits of nfiles and prompter beyond their tagged
;;; releases.  Pin them here so the build is fully self-contained.

(define sbcl-nfiles-master
  (let ((commit "b44b06caf20cececeaee2153a747902c508a9c48")
        (revision "0"))
    (package
      (inherit sbcl-nfiles)
      (version (git-version "1.1.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/atlas-engineer/nfiles")
               (commit commit)))
         (file-name (git-file-name "cl-nfiles" version))
         (sha256
          (base32
           "1lm6p9cncixqybhhy212pnlvx132fjv0xc14wkrvimd7i38dxcdl")))))))

(define sbcl-prompter-master
  (let ((commit "fb0302dd94c5be20674e07038419e27eac79f406")
        (revision "0"))
    (package
      (inherit sbcl-prompter)
      (version (git-version "0.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/atlas-engineer/prompter")
               (commit commit)))
         (file-name (git-file-name "cl-prompter" version))
         (sha256
          (base32
           "1c58wk113dry0z6kyl1kckj7il314jlyzfppf9x0nxxrx202f9bs")))))))

(define sbcl-cl-webkit-for-nyxt4
  ;; cl-webkit 3.5.10 uses the deprecated webkit_web_view_run_javascript API
  ;; which crashes (segfault) in WebKitGTK 2.50.3 (webkit2gtk-4.1).  It also
  ;; declares the WORLD optional arg as type STRING, causing a TYPE-ERROR when
  ;; nyxt passes NIL.  Three patches applied at build time:
  ;;   1. Fix the ftype to accept (or null string) for world.
  ;;   2. Add CFFI bindings for webkit_web_view_evaluate_javascript (2.40+ API).
  ;;   3. Replace the old deprecated run_javascript dispatch with the new API.
  (package
    (inherit sbcl-cl-webkit)
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-webkit)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'use-webkit-evaluate-javascript-api
              (lambda _
                ;; string-replace-substring is from (guix build utils) which is
                ;; not available in all build sandboxes; use string-contains here.
                (define (str-sub str from to)
                  (let ((flen (string-length from)))
                    (let loop ((start 0) (parts '()))
                      (let ((pos (string-contains str from start)))
                        (if pos
                            (loop (+ pos flen)
                                  (cons to (cons (substring str start pos) parts)))
                            (apply string-append
                                   (reverse (cons (substring str start) parts))))))))
                (define f "webkit2/webkit2.web-view.lisp")
                (define content
                  (call-with-input-file f
                    (lambda (p)
                      (let loop ((acc '()) (c (read-char p)))
                        (if (eof-object? c)
                            (list->string (reverse acc))
                            (loop (cons c acc) (read-char p)))))))
                ;; Fix 1: ftype world arg from bare string to (or null string).
                ;; Use exactly 2 close-parens at end to preserve the declaration
                ;; structure: one closes the arg-types list, one closes (function ...).
                (define p1
                  (str-sub content
                    "(or null (function (condition))) string))"
                    "(or null (function (condition))) (or null string)))"))
                ;; Fix 2: replace the deprecated if-world run_javascript dispatch
                ;; with a single call to webkit_web_view_evaluate_javascript
                ;; (WebKitGTK 2.40+ API).  Passing nil for world-name → NULL → default
                ;; JS world; nil for source-uri → NULL; -1 for length → strlen.
                (define p2
                  (str-sub p1
                    (string-append
                      "  (if world\n"
                      "      (webkit-web-view-run-javascript-in-world\n"
                      "       web-view javascript world\n"
                      "       (cffi:null-pointer)\n"
                      "       (cffi:callback javascript-evaluation-complete)\n"
                      "       (cffi:make-pointer callback-counter))\n"
                      "      (webkit-web-view-run-javascript\n"
                      "       web-view javascript\n"
                      "       (cffi:null-pointer)\n"
                      "       (cffi:callback javascript-evaluation-complete)\n"
                      "       (cffi:make-pointer callback-counter))))")
                    (string-append
                      "  (%nyxt4-webkit-eval-js\n"
                      "   web-view javascript -1 (or world (cffi:null-pointer)) (cffi:null-pointer)\n"
                      "   (cffi:null-pointer)\n"
                      "   (cffi:callback nyxt4-js-eval-complete)\n"
                      "   (cffi:make-pointer callback-counter)))")))
                ;; Fix 3: insert new CFFI bindings and a callback for the new API
                ;; right after the existing evaluate-javascript export.
                (define new-bindings
                  (string-append
                    "; WebKitGTK 2.40+ evaluate_javascript API\n"
                    "(defcfun (\"webkit_web_view_evaluate_javascript\""
                    " %nyxt4-webkit-eval-js) :void\n"
                    "  (web-view (g-object webkit-web-view))\n"
                    "  (script :string)\n"
                    "  (length :long)\n"
                    "  (world-name :string)\n"
                    "  (source-uri :string)\n"
                    "  (cancellable :pointer)\n"
                    "  (callback g-async-ready-callback)\n"
                    "  (user-data :pointer))\n"
                    "(defcfun (\"webkit_web_view_evaluate_javascript_finish\""
                    " %nyxt4-webkit-eval-js-finish) (g-object jsc-value)\n"
                    "  (web-view (g-object webkit-web-view))\n"
                    "  (result g-async-result)\n"
                    "  (g-error :pointer))\n"
                    "(defun %nyxt4-eval-js-finish (web-view result)\n"
                    "  (glib:with-g-error (err)\n"
                    "    (%nyxt4-webkit-eval-js-finish web-view result err)))\n"
                    "(cffi:defcallback nyxt4-js-eval-complete\n"
                    "    :void ((source-object :pointer)"
                    " (result :pointer) (user-data :pointer))\n"
                    "  (declare (ignore source-object))\n"
                    "  (let ((cb (find (cffi:pointer-address user-data)"
                    " callbacks :key #'callback-id)))\n"
                    "    (handler-case\n"
                    "        (let ((value (%nyxt4-eval-js-finish"
                    " (callback-web-view cb) result)))\n"
                    "          (when value\n"
                    "            (let ((exc (jsc-context-get-exception"
                    " (jsc-value-get-context value))))\n"
                    "              (when exc"
                    " (signal 'jsc-exception-condition :exception exc))))\n"
                    "          (setf callbacks (delete cb callbacks))\n"
                    "          (when (and value (callback-function cb))\n"
                    "            (funcall (callback-function cb)"
                    " (jsc-value-to-lisp value) value)))\n"
                    "      (condition (c)\n"
                    "        (when cb\n"
                    "          (when (callback-error-function cb)\n"
                    "            (funcall (callback-error-function cb) c))\n"
                    "          (setf callbacks (delete cb callbacks)))))))\n"
                    "\n"))
                (define p3
                  (str-sub p2
                    "(export 'webkit-web-view-evaluate-javascript)\n"
                    (string-append
                      "(export 'webkit-web-view-evaluate-javascript)\n"
                      "\n"
                      new-bindings)))
                (call-with-output-file f
                  (lambda (p) (display p3 p)))))))))))

;;; ─── Electron infrastructure (needed only for nyxt-electron) ──────────────

(define node-nan-without-scriptorigin
  (package
    (inherit node-nan)
    (arguments
     (substitute-keyword-arguments (package-arguments node-nan)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'patch-nan
              (lambda _
                (substitute* "nan.h"
                  (("#include \"nan_scriptorigin.h\"")
                   "// #include \"nan_scriptorigin.h\""))
                (delete-file "nan_scriptorigin.h")))))))))

(define-public node-synchronous-socket
  (package
    (name "node-synchronous-socket")
    (version "73e1ce83b2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atlas-engineer/synchronous-socket")
             (commit "73e1ce83b2dfe48fb37332a6e5534b4ecd70e2da")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ch731q77ks42g5lq8icphmvnqadd5j4yr4gsvzdlh21gyp9ld51"))))
    (native-inputs (list python))
    (inputs (list node-nan-without-scriptorigin node-bindings))
    (build-system node-build-system)
    (arguments
     `(#:modules ((guix build node-build-system)
                  (guix build utils))
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-dependencies 'delete-dependencies
           (lambda args
             (modify-json
              (delete-dependencies '("node-gyp"))
              (delete-fields '(("scripts" "configure")
                               ("scripts" "build")))))))))
    (home-page "https://github.com/atlas-engineer/synchronous-socket")
    (synopsis "Node.js synchronous Unix socket interface.")
    (description "SynchronousSocket exposes a synchronous interface to Unix
domain sockets for use with cl-electron.")
    (license license:bsd-3)))

(define-public (node-package->electron-package node-package electron-headers)
  "Rewrite NODE-PACKAGE to build against ELECTRON-HEADERS instead of system Node."
  (package
    (inherit node-package)
    (name (string-append "electron-" (package-name node-package)))
    (native-inputs
     (cons (list "electron-headers" electron-headers)
           (package-native-inputs node-package)))
    (inputs
     (map (lambda (i)
            (let ((iname (car i))
                  (ipkg (car (cdr i))))
              (if (eq? 'node (build-system-name (package-build-system ipkg)))
                  (list iname (node-package->electron-package ipkg electron-headers))
                  i)))
          (package-inputs node-package)))
    (arguments
     (substitute-keyword-arguments (package-arguments node-package)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (replace 'configure
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (let ((npm (string-append (assoc-ref inputs "node") "/bin/npm"))
                      (electron-node-headers
                       (string-append (assoc-ref inputs "electron-headers") "/node_headers")))
                  (invoke npm "--offline" "--ignore-scripts" "--install-links"
                          (string-append "--nodedir=" electron-node-headers) "install")
                  #t)))
            (replace 'install
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out"))
                      (npm (string-append (assoc-ref inputs "node") "/bin/npm"))
                      (electron-node-headers
                       (string-append (assoc-ref inputs "electron-headers") "/node_headers")))
                  (invoke npm "--prefix" out "--global" "--offline" "--loglevel" "info"
                          "--production" "--install-links"
                          (string-append "--nodedir=" electron-node-headers)
                          "install" "../package.tgz")
                  #t)))))))))

(define-public sbcl-cl-electron
  (let ((commit "f8703cb101e88800a86f5a9194d945d9810c1117")
        (revision "5"))
    (package
      (name "sbcl-cl-electron")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/atlas-engineer/cl-electron")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1sapsc660xsgv6sjfrjlqp15a7kni05iil9pf4plc2z5zn7hrj0b"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       (list
        #:asd-systems ''("cl-electron")
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-package
              (lambda _
                (substitute* "source/core.lisp"
                  (("\"npm\" \"run\" \"start\" \"--\"")
                   (string-append "\"" #$(this-package-input "electron")
                                  "/bin/electron" "\"")))))
            (add-after 'unpack 'add-node-modules
              (lambda _
                (mkdir-p (string-append #$output "/node_modules"))
                (symlink
                 (string-append #$(this-package-input "electron-node-synchronous-socket")
                                "/lib/node_modules/synchronous-socket")
                 (string-append #$output "/node_modules/synchronous-socket")))))))
      (native-inputs
       (list pkg-config sbcl-spinneret sbcl-lisp-unit2 sbcl-parenscript))
      (inputs
       (list electron-36
             (node-package->electron-package node-synchronous-socket
                                             electron-36-node-headers)
             sbcl-alexandria
             sbcl-babel
             sbcl-bordeaux-threads
             sbcl-cl-base64
             sbcl-cl-json
             sbcl-cl-ppcre
             sbcl-iolib
             sbcl-lparallel
             sbcl-nclasses
             sbcl-parse-number
             sbcl-lass))
      (synopsis "Lisp interface to Electron.")
      (description "Electron binding for Common Lisp.")
      (license license:bsd-3)
      (home-page "https://github.com/atlas-engineer/cl-electron"))))

;;; ─── Shared Nyxt 4.0.0 source and common input list ───────────────────────

(define %nyxt4-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/atlas-engineer/nyxt")
          (commit "4.0.0")))
    (file-name (git-file-name "nyxt" "4.0.0"))
    (sha256
     (base32 "08zqr1c91l5qzpzhli32lvam254lwsfbjrcxcm6a71plgdp0wvz2"))))

(define %nyxt4-common-inputs
  (list sbcl-alexandria
        sbcl-bordeaux-threads
        sbcl-calispel
        sbcl-cl-base64
        sbcl-cl-colors-ng
        sbcl-cl-gopher
        sbcl-cl-json
        sbcl-cl-ppcre
        sbcl-cl-ppcre-unicode
        sbcl-cl-prevalence
        sbcl-cl-qrencode
        sbcl-cl-sqlite
        sbcl-cl-str
        sbcl-cl-tld
        sbcl-closer-mop
        sbcl-clss
        sbcl-cluffer
        sbcl-dexador
        sbcl-enchant
        sbcl-flexi-streams
        sbcl-iolib
        sbcl-lass
        sbcl-local-time
        sbcl-log4cl
        sbcl-lparallel
        sbcl-nclasses
        sbcl-nfiles-master
        sbcl-nhooks
        sbcl-njson
        sbcl-nkeymaps
        sbcl-nsymbols
        sbcl-parenscript
        sbcl-phos
        sbcl-plump
        sbcl-prompter-master
        sbcl-py-configparser
        sbcl-quri
        sbcl-serapeum
        sbcl-spinneret
        sbcl-trivia
        sbcl-trivial-arguments
        sbcl-trivial-clipboard
        sbcl-trivial-package-local-nicknames
        sbcl-trivial-types
        sbcl-unix-opts))

;;; ─── SBCL library packages (one per renderer) ─────────────────────────────

(define-public sbcl-nyxt-electron
  (package
    (name "sbcl-nyxt-electron")
    (version "4.0.0")
    (source %nyxt4-source)
    (build-system asdf-build-system/sbcl)
    (arguments
     (list
      #:asd-systems ''("nyxt" "nyxt/electron" "nyxt/electron-application")
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fully-qualify-ps-symbol
            (lambda _
              (substitute* "source/spinneret-tags.lisp"
                (("\\(with-ps-gensyms") "(ps:with-ps-gensyms")))))))
    (native-inputs (list sbcl-lisp-unit2))
    (inputs (append %nyxt4-common-inputs (list sbcl-cl-electron)))
    (synopsis "Nyxt 4.0.0 (Electron renderer) — SBCL library")
    (description "Nyxt [nýkst] is a keyboard-driven web browser designed for
hackers.  Inspired by Emacs and Vim, it has familiar keybindings (Emacs, vi,
CUA), and is infinitely extensible in Lisp.")
    (license license:bsd-3)
    (home-page "https://github.com/atlas-engineer/nyxt")))

(define-public sbcl-nyxt-webkit
  (package
    (inherit sbcl-nyxt-electron)
    (name "sbcl-nyxt-webkit")
    (arguments
     (list
      #:asd-systems ''("nyxt" "nyxt/gi-gtk" "nyxt/gi-gtk-application")
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fully-qualify-ps-symbol
            (lambda _
              (substitute* "source/spinneret-tags.lisp"
                (("\\(with-ps-gensyms") "(ps:with-ps-gensyms"))))
          (add-after 'unpack 'create-reduce-tracking-stub
            ;; source/mode/reduce-tracking.lisp is missing from the Nyxt 4.0.0
            ;; release tarball but renderer/gtk.lisp references the package and
            ;; class at read time.  Write a minimal stub so the package exists.
            (lambda _
              (call-with-output-file "source/mode/reduce-tracking.lisp"
                (lambda (port)
                  (display
                   ";;;; SPDX-FileCopyrightText: Atlas Engineer LLC\n\
;;;; SPDX-License-Identifier: BSD-3-Clause\n\
\n\
(nyxt:define-package :nyxt/mode/reduce-tracking\n\
  (:documentation \"Package for `reduce-tracking-mode'.\"))\n\
(in-package :nyxt/mode/reduce-tracking)\n\
\n\
(define-mode reduce-tracking-mode ()\n\
  \"Reduce browser fingerprinting and tracking.\n\
On WebKit-based renderers, also enables Intelligent Tracking Prevention (ITP).\")\n"
                   port)))))
          (add-after 'unpack 'fix-gtk-renderer-load-order
            ;; renderer/gtk.lisp references nyxt/mode/user-script and
            ;; nyxt/mode/reduce-tracking package-qualified symbols at read time.
            ;; Add explicit ASDF dependencies and register the new stub component.
            (lambda _
              (substitute* "nyxt.asd"
                (("\\(:file \"renderer/gtk\"\\)")
                 "(:file \"renderer/gtk\" :depends-on (\"mode/user-script\" \"mode/reduce-tracking\"))")
                (("\\(:file \"mode/proxy\"\\)\\)\\)")
                 "(:file \"mode/proxy\") (:file \"mode/reduce-tracking\")))"))))
          (add-after 'unpack 'stub-nkeymaps-unshift
            ;; nkeymaps 1.1.1 lacks keymaps:unshift.  The call is inside
            ;; ignore-errors so a no-op stub preserves all behaviour; the
            ;; symbol must exist at READ time or SBCL errors during compilation.
            (lambda _
              (substitute* "source/renderer/gtk.lisp"
                (("\\(keymaps:unshift key-string\\)")
                 "(if (and (= 1 (length key-string)) (upper-case-p (char key-string 0))) (string-downcase key-string) key-string)")))))))
    (inputs (append %nyxt4-common-inputs
                    (list gtk+
                          glib-networking
                          webkitgtk-for-gtk3
                          sbcl-cl-webkit-for-nyxt4
                          sbcl-cl-gobject-introspection)))
    (synopsis "Nyxt 4.0.0 (WebKitGTK renderer) — SBCL library")))

;;; ─── Installable binary packages ───────────────────────────────────────────

(define (%make-nyxt-binary sbcl-pkg system-kw binary-name)
  "Return an installable Nyxt binary package.
SBCL-PKG is the renderer-specific SBCL library package.
SYSTEM-KW is the ASDF system keyword string, e.g. \":nyxt/gi-gtk-application\"."
  (package
    (inherit sbcl-pkg)
    (name binary-name)
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-pkg)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'strip 'make-executable
              (lambda _
                (invoke "sbcl"
                        "--eval" "(require \"asdf\")"
                        "--eval" "(asdf:load-asd (merge-pathnames \"libraries/nasdf/nasdf.asd\"))"
                        "--eval" "(asdf:load-asd (merge-pathnames \"nyxt.asd\"))"
                        "--eval" #$(string-append "(asdf:load-system " system-kw ")")
                        "--eval" #$(string-append "(asdf:make " system-kw ")"))
                (mkdir-p (string-append #$output "/bin"))
                (copy-file "nyxt" (string-append #$output "/bin/nyxt"))))
            (add-after 'make-executable 'add-assets
              (lambda _
                (define (asset name)
                  (string-append #$output "/share/common-lisp/sbcl/" #$binary-name "/assets/" name))
                (let ((nyxt.desktop (string-append #$output "/share/applications/nyxt.desktop")))
                  (mkdir-p (dirname nyxt.desktop))
                  (symlink (asset "nyxt.desktop") nyxt.desktop)
                  (substitute* nyxt.desktop
                    (("^Exec=nyxt") (string-append "Exec=" #$output "/bin/nyxt"))))
                (let ((nyxt.metainfo.xml
                       (string-append #$output "/share/metainfo/nyxt.metainfo.xml")))
                  (mkdir-p (dirname nyxt.metainfo.xml))
                  (symlink (asset "nyxt.metainfo.xml") nyxt.metainfo.xml))
                (let ((nyxt.svg (string-append #$output
                                               "/share/icons/hicolor/scalable/apps/nyxt.svg")))
                  (mkdir-p (dirname nyxt.svg))
                  (symlink (asset "glyphs/nyxt.svg") nyxt.svg))
                (for-each
                 (lambda (size)
                   (let ((out (format #f "~a/share/icons/hicolor/~dx~d/nyxt.png"
                                      #$output size size)))
                     (mkdir-p (dirname out))
                     (symlink (asset (format #f "/icons/nyxt_~dx~d.png" size size))
                              out)))
                 '(16 32 128 256 512))))))))))

(define-public nyxt-webkit
  (let ((base (%make-nyxt-binary sbcl-nyxt-webkit
                                 ":nyxt/gi-gtk-application"
                                 "nyxt-webkit")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'add-assets 'wrap-for-gi-typelibs
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((gi-paths '()))
                    ;; Collect GI typelib paths from all inputs.
                    (for-each
                     (lambda (input)
                       (let ((gi (string-append (cdr input)
                                                "/lib/girepository-1.0")))
                         (when (file-exists? gi)
                           (set! gi-paths (cons gi gi-paths)))))
                     inputs)
                    ;; Build a filtered GIO modules directory containing only
                    ;; libgiognutls.so.  We deliberately exclude
                    ;; libgiognomeproxy.so and libgiolibproxy.so: those proxy
                    ;; resolver modules try to read the
                    ;; 'org.gnome.system.proxy' GSettings schema, which is
                    ;; not installed here.  The resulting GLib-GIO-ERROR
                    ;; crashes WebKit's network process and causes every
                    ;; first navigation to fail immediately.
                    (let ((custom-gio-dir
                           (string-append #$output "/lib/gio/modules"))
                          (gnutls-so #f))
                      (for-each
                       (lambda (input)
                         (let ((candidate
                                (string-append (cdr input)
                                               "/lib/gio/modules/libgiognutls.so")))
                           (when (and (not gnutls-so)
                                      (file-exists? candidate))
                             (set! gnutls-so candidate))))
                       inputs)
                      (when gnutls-so
                        (mkdir-p custom-gio-dir)
                        (symlink gnutls-so
                                 (string-append custom-gio-dir
                                                "/libgiognutls.so"))
                        (call-with-output-file
                            (string-append custom-gio-dir "/giomodule.cache")
                          (lambda (p)
                            (display "libgiognutls.so: gio-tls-backend\n" p))))
                      (wrap-program (string-append #$output "/bin/nyxt")
                        (list "GI_TYPELIB_PATH" ":" 'prefix gi-paths)
                        (list "GIO_EXTRA_MODULES" ":" 'prefix
                              (if gnutls-so
                                  (list custom-gio-dir)
                                  '()))))))))))))))


(define-public nyxt-electron
  (%make-nyxt-binary sbcl-nyxt-electron ":nyxt/electron-application" "nyxt-electron"))
