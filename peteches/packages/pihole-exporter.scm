(define-module (peteches packages pihole-exporter)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages base))

(define-public pihole-exporter
  (let* ((version "1.2.0")
         (url (string-append
               "https://github.com/eko/pihole-exporter/releases/download/v"
               version "/pihole_exporter-linux-amd64")))
    (package
      (name "pihole-exporter")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri url)
         (sha256 (base32 "1fyvrp9lxn7c26cikj721n248xgy2vpb681kvl8mn4iva55p0r4d"))))
      (build-system copy-build-system)
      (inputs (list glibc))
      (arguments
       (list
        #:install-plan
        #~(list (list "pihole_exporter-linux-amd64" "lib/pihole-exporter-bin"))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'strip)
            (delete 'validate-runpath)
            (add-before 'install 'make-executable
              (lambda _
                (chmod "pihole_exporter-linux-amd64" #o755)))
            (add-after 'install 'make-wrapper
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out  (assoc-ref outputs "out"))
                       (bin  (string-append out "/bin/pihole-exporter"))
                       (real (string-append out "/lib/pihole-exporter-bin")))
                  (mkdir-p (string-append out "/bin"))
                  (call-with-output-file bin
                    (lambda (port)
                      (format port "#!/bin/sh\nexec ~a/lib/ld-linux-x86-64.so.2 --library-path ~a/lib ~a \"$@\"\n"
                              #$(file-append glibc)
                              #$(file-append glibc)
                              real)))
                  (chmod bin #o755)))))))
      (supported-systems '("x86_64-linux"))
      (home-page "https://github.com/eko/pihole-exporter")
      (synopsis "Prometheus exporter for Pi-hole statistics")
      (description "Exports Pi-hole DNS ad-blocking metrics to Prometheus.")
      (license license:expat))))
