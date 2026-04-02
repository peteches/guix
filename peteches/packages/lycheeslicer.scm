(define-module (peteches packages lycheeslicer)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (nongnu packages engineering))

(define-public lycheeslicer-7.6.2
  (package
    (inherit lycheeslicer)
    (version "7.6.2")
    (arguments
     (substitute-keyword-arguments (package-arguments lycheeslicer)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install-wrapper 'strip-mesa-from-bin-wrapper
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out     (assoc-ref outputs "out"))
                       (wrapper (string-append out "/bin/lycheeslicer")))
                  (when (file-exists? wrapper)
                    (substitute* wrapper
                      (("export LD_LIBRARY_PATH=\"([^\"]*)\""
                        all value)
                       (let* ((parts (string-split value #\:))
                              (filtered
                               (filter (lambda (p)
                                         (not (string-contains p "-mesa-")))
                                       parts)))
                         (string-append
                          "export LD_LIBRARY_PATH=\""
                          (string-join filtered ":")
                          "\"")))))
                  #t)))))))
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://mango-lychee.nyc3.cdn.digitaloceanspaces.com/LycheeSlicer-"
         version ".deb"))
       (sha256
        (base32 "0vlr2mxb0h957ww6rifh84d880lpn12nhxid78ljk26ks371h6il"))))))

lycheeslicer-7.6.2
