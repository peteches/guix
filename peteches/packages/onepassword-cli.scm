(define-module (peteches packages onepassword-cli)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (gnu packages compression))

;; Upstream ships a fully static Go binary (no dynamic section, no ELF
;; interpreter -- verified with `readelf -d`), so no patchelf pass needed.
;; Same shape as (peteches packages github-cli); see that module's
;; commentary for why the 'install phase bypasses copy-build-system's
;; generic #:install-plan machinery (broken on this repo's currently
;; pinned guix commit).
(define %version
  "2.39.0")

(define-public onepassword-cli
  (package
    (name "onepassword-cli")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cache.agilebits.com/dist/1P/op2/pkg/v" version
             "/op_linux_amd64_v" version ".zip"))
       (sha256
        ;; guix hash op_linux_amd64_v2.39.0.zip
        (base32 "0d34m09h8gavrsicc0z1rijd0fm4624l01hvyi4yqvbcdcvpzfkg"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (use-modules (guix build utils))
              ;; The zip is flat (op, op.sig at the archive root) -- no
              ;; versioned subdirectory to chdir into, unlike github-cli's
              ;; tarball.
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "op" bin)
                (chmod (string-append bin "/op") #o755)))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://developer.1password.com/docs/cli/")
    (synopsis "1Password command line tool")
    (description
     "@command{op} is 1Password's official command line tool.  It gives
scripts and automation access to items, secrets and vaults stored in
1Password, and can inject secrets into a process's environment without
writing them to disk.")
    ;; Proprietary freeware -- 1Password's End User License Agreement, not
    ;; an open-source license (unlike github-cli, which is Expat).
    (license #f)))
