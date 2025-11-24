(define-module (peteches packages aws)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gexp)                 ; for #~ gexps
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages golang))      ; for `go`

(define-public aws-session-manager-plugin
  (let* ((version "1.2.707.0")
         (tarball (string-append
                   "https://github.com/aws/session-manager-plugin/archive/refs/tags/"
                   version ".tar.gz")))
    (package
      (name "aws-session-manager-plugin")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri tarball)
         ;; Use your verified base32:
         (sha256 (base32 "16rql1vc25a6m0bzrhj69hrwpywfn1i1wmdrpnx244wb5wax38gp"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'build
              (lambda _
                (use-modules (guix build utils))
                (let* ((pwd (getcwd))
                       (cache (string-append pwd "/.cache/go-build"))
                       (gopath (string-append pwd "/gopath"))
                       (ghroot (string-append gopath "/src/github.com/aws"))
                       (proj (string-append ghroot "/session-manager-plugin"))
                       (vendorg (string-append proj "/vendor")))
                  ;; Fix Go cache/tmp in sandbox:
                  (setenv "HOME" pwd)
                  (setenv "XDG_CACHE_HOME" (string-append pwd "/.cache"))
                  (mkdir-p (getenv "XDG_CACHE_HOME"))
                  (setenv "GOCACHE" cache)
                  (mkdir-p cache)
                  (setenv "GOTMPDIR" (string-append pwd "/.gotmp"))
                  (mkdir-p (getenv "GOTMPDIR"))
                  ;; Build in GOPATH mode with vendored deps:
                  (setenv "GO111MODULE" "off")
                  (mkdir-p ghroot)
                  (symlink pwd proj)
                  (setenv "GOPATH" (string-append gopath ":" vendorg))
                  (with-directory-excursion proj
                    (invoke "go" "build" "-o" "session-manager-plugin"
                            "./src/sessionmanagerplugin-main")))))
            (replace 'install
              (lambda _
                (use-modules (guix build utils))
                (let* ((out   #$output)
                       (bin   (string-append out "/bin"))
		       (binary (string-append bin "/session-manager-plugin"))
                       (share (string-append out "/share/sessionmanagerplugin")))
                  (mkdir-p bin)
                  (mkdir-p share)
                  (install-file
                   "gopath/src/github.com/aws/session-manager-plugin/session-manager-plugin"
                   bin)
		  (chmod binary #o755)
                  (when (file-exists? "VERSION")
                    (install-file "VERSION" share))
                  (when (file-exists? "seelog_unix.xml")
                    (install-file "seelog_unix.xml" share))))))))
      (supported-systems '("x86_64-linux" "aarch64-linux"))
      (native-inputs (list go))
      (home-page "https://github.com/aws/session-manager-plugin")
      (synopsis "AWS Session Manager Plugin (built from source)")
      (description "Client plugin the AWS CLI invokes for Systems Manager sessions, including port forwarding.")
      (license asl2.0))))
aws-session-manager-plugin
