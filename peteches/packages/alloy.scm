;; peteches/packages/alloy.scm — Grafana Alloy pre-built binary package.
;;
;; Alloy is a CGO-enabled Go binary (dynamically linked against glibc).
;; patchelf corrupts it due to segment restructuring at 356 MiB, so we
;; instead install the unmodified binary to libexec/ and create a shell
;; wrapper in bin/ that invokes it via the Guix glibc ELF loader directly.

(define-module (peteches packages alloy)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression))

(define-public alloy
  (package
    (name "alloy")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/grafana/alloy/releases/download/v"
             version "/alloy-linux-amd64.zip"))
       (sha256 (base32 "1bfasi56mbmy4vrhf33zm3h90pdm9676v67rrk3qldwvi1fz5wsm"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (inputs (list glibc))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; The 356 MiB binary exhausts Guix's in-process ELF analyser.
          (delete 'validate-runpath)
          (delete 'strip)
          (delete 'make-dynamic-linker-cache)
          (add-after 'install 'install-to-libexec
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (bin     (string-append out "/bin/alloy"))
                     (libexec (string-append out "/libexec")))
                (mkdir-p libexec)
                (rename-file bin (string-append libexec "/alloy"))
                (chmod (string-append libexec "/alloy") #o755))))
          (add-after 'install-to-libexec 'create-wrapper
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (libc    (assoc-ref inputs "glibc"))
                     (loader  (string-append libc "/lib/ld-linux-x86-64.so.2"))
                     (libpath (string-append libc "/lib"))
                     (real    (string-append out "/libexec/alloy"))
                     (wrapper (string-append out "/bin/alloy")))
                (mkdir-p (string-append out "/bin"))
                (call-with-output-file wrapper
                  (lambda (port)
                    (format port "#!/bin/sh\nexec ~a --library-path ~a ~a \"$@\"\n"
                            loader libpath real)))
                (chmod wrapper #o755)))))
      #:install-plan
      #~'(("alloy-linux-amd64" "bin/alloy"))))
    (home-page "https://grafana.com/oss/alloy/")
    (synopsis "OpenTelemetry collector distribution for metrics, logs, and traces")
    (description
     "Grafana Alloy is a vendor-neutral distribution of the OpenTelemetry
Collector.  It supports collecting and forwarding metrics, logs, and traces
to Prometheus, Loki, Tempo, and other backends.")
    (license asl2.0)))
