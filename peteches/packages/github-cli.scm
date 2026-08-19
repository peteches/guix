(define-module (peteches packages github-cli)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp))

;; Upstream ships a fully static Go binary (no dynamic section, no ELF
;; interpreter -- verified with `readelf -d`), so unlike koboldcpp-bin this
;; needs no patchelf pass.
;;
;; copy-build-system's generic #:install-plan-driven 'install phase is
;; broken on the guix commit this repo is currently pinned to -- any
;; #:install-plan value (gexp-wrapped or plain data, one entry or several)
;; fails with "Wrong type to apply" from inside its own for-each/apply
;; loop (confirmed by reproducing the same failure against
;; peteches/packages/rclone.scm's #:install-plan, which uses the identical
;; shape). Sidestep the shared `install' procedure entirely by replacing
;; the phase with a plain copy, the same way koboldcpp.scm avoids it.
(define %version
  "2.97.0")

(define-public github-cli
  (package
    (name "github-cli")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/cli/cli/releases/download/v" version
             "/gh_" version "_linux_amd64.tar.gz"))
       (sha256
        ;; guix hash gh_2.97.0_linux_amd64.tar.gz
        (base32 "04l104py27lfx1cy8qg4p00qh29fc9d8pdzw1nnv318zgr4vijd2"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (use-modules (guix build utils))
              ;; gnu-build-system's default 'unpack chdirs into the
              ;; tarball's single top-level directory, so paths here are
              ;; already relative to gh_<version>_linux_amd64/.
              (let ((out (assoc-ref outputs "out")))
                (install-file "bin/gh" (string-append out "/bin"))
                (copy-recursively "share/man"
                                   (string-append out "/share/man"))))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://cli.github.com/")
    (synopsis "GitHub's official command line tool")
    (description
     "The GitHub CLI (@code{gh}) brings pull requests, issues, and other
GitHub concepts to the terminal, next to where you are already working with
@command{git} and code.")
    (license license:expat)))
