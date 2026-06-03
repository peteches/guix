;; peteches/packages/nzbget.scm — NZBGet with webui config template.
;;
;; The upstream Guix nzbget package omits the nzbget.conf template that the
;; web UI settings page reads via the configtemplates RPC.  cmake generates
;; it from nzbget.conf.in during the build but does not install it into the
;; package output.  This package adds a post-install phase to copy it into
;; share/nzbget/webui/ so that WebDir and the template are co-located and
;; the settings page loads correctly.

(define-module (peteches packages nzbget)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages networking))

(define-public nzbget
  (package
    (inherit (@ (gnu packages networking) nzbget))
    (arguments
     (substitute-keyword-arguments
       (package-arguments (@ (gnu packages networking) nzbget))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'install-config-template
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out   (assoc-ref outputs "out"))
                       (webui (string-append out "/share/nzbget/webui")))
                  (copy-file "nzbget.conf"
                             (string-append webui "/nzbget.conf")))))))))))
