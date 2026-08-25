;; peteches/packages/gnupg.scm
;;
;; At the guix channel commit pinned by peteches/channels/*.scm
;; (b13c7c02b5f6d635e123f863227aa32ac64e3498), upstream Guix's `gnupg'
;; package only has two versions: 1.4.23 (the ancient 1.x line) and 2.5.20 --
;; an explicit development/test snapshot per its own startup banner, with no
;; stable 2.4.x available at all.
;;
;; 2.5.20's `gpg-agent --supervised' socket-activation mode does not
;; correctly serve connections through the socket Guix Shepherd's
;; home-gpg-agent-service-type binds for it (confirmed live on dagon: the
;; listening socket is held by shepherd, but the supervised agent process
;; never answers on it), so every `gpg'/`pass' call times out against the
;; real agent and silently falls back to spawning its own disposable
;; classic-mode agent -- meaning passphrase caching never actually works.
;;
;; This pins gnupg back to 2.4.8, the exact version dagon was already
;; running successfully (with working caching) before a `guix pull' moved
;; it onto 2.5.20. Definition lifted from upstream Guix commit dd3e59a
;; (gnu/packages/gnupg.scm), the last commit known to carry 2.4.8, adapted
;; to build against whatever gnutls/libassuan/etc. the current channel pin
;; provides.

(define-module (peteches packages gnupg)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages gnupg) #:hide (gnupg))
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages compression)
  #:use-module ((gnu packages emacs-xyz) #:prefix upstream:)
  #:use-module ((gnu packages password-utils) #:prefix upstream:))

(define-public gnupg
  (package
    (name "gnupg")
    (version "2.4.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (patches (search-patches "gnupg-default-pinentry.patch"))
              (sha256
               (base32
                "05l666aha1nxpiiras446zmkhcgqnp33y74wyhzj9lq4kgbq135m"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list gnutls
           libassuan
           libgcrypt
           libgpg-error
           libksba
           npth
           openldap
           pcsc-lite
           readline
           sqlite
           zlib))
    (arguments
     (list
      #:configure-flags
      #~(quote ("--enable-gnupg-builddir-envvar"
                "--enable-all-tests"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libpcsclite.so (search-input-file inputs
                                                       "lib/libpcsclite.so")))
                (substitute* "scd/scdaemon.c"
                  (("libpcsclite\\.so")
                   libpcsclite.so)))))
          (add-after 'build 'patch-scheme-tests
            (lambda _
              (substitute* (find-files "tests" ".\\.scm$")
                (("/usr/bin/env gpgscm")
                 (string-append (getcwd) "/tests/gpgscm/gpgscm")))))
          (add-before 'build 'patch-test-paths
            (lambda _
              (substitute* '("tests/pkits/inittests"
                             "tests/pkits/common.sh"
                             "tests/pkits/Makefile")
                (("/bin/pwd") (which "pwd")))
              (substitute* "common/t-exectool.c"
                (("/bin/cat") (which "cat"))
                (("/bin/true") (which "true"))
                (("/bin/false") (which "false"))))))))
    (home-page "https://gnupg.org/")
    (synopsis "GNU Privacy Guard")
    (description
     "The GNU Privacy Guard is a complete implementation of the OpenPGP
standard.  It is used to encrypt and sign data and communication.  It
features powerful key management and the ability to access public key
servers.  It includes several libraries: libassuan (IPC between GnuPG
components), libgpg-error (centralized GnuPG error values), and
libskba (working with X.509 certificates and CMS data).")
    (license license:gpl3+)))

;; emacs-pinentry propagates upstream's `gnupg' (2.5.20), which conflicts
;; with the 2.4.8 pin above the moment both land in the same profile --
;; "profile contains conflicting entries for gnupg" (confirmed live via
;; `guix home reconfigure' on dagon).  Swap just that one propagated input.
(define-public emacs-pinentry
  (package/inherit upstream:emacs-pinentry
    (propagated-inputs (list gnupg))))

;; password-store's `pass' wrapper runs `wrap-program' at build time,
;; prefixing PATH with the `gpg' from *its own* gnupg input -- ahead of
;; whatever gnupg is on the caller's profile PATH.  Left unoverridden,
;; `pass show ...' always invokes upstream's 2.5.20 dev snapshot
;; regardless of the 2.4.8 pin above, producing "Wrong secret key used"
;; against a 2.4.8 gpg-agent (confirmed live on nyarlothotep).  Swap just
;; the gnupg input so the wrapper bakes in the pinned 2.4.8 gpg instead.
(define-public password-store
  (package/inherit upstream:password-store
    (inputs (modify-inputs (package-inputs upstream:password-store)
              (replace "gnupg" gnupg)))))
