(define-module (peteches packages hyprland-glvnd)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system)
  #:use-module (gnu packages wm)     ; hyprland
  #:use-module (gnu packages gl)     ; libglvnd
  #:use-module (gnu packages base))  ; glibc (for ldconfig)

(define-public hyprland-glvnd
  (package
    (inherit hyprland)
    (name "hyprland-glvnd")
    (arguments
     (substitute-keyword-arguments (package-arguments hyprland)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; Hyprland ships $out/etc/ld.so.cache. Regenerate it so that
            ;; libEGL.so.1 and libGL.so.1 resolve to libglvnd, not Mesa.
            (add-after 'install 'regenerate-ld-so-cache-for-glvnd
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (let* ((out      (assoc-ref outputs "out"))
                       (etc      (string-append out "/etc"))
                       (cache    (string-append etc "/ld.so.cache"))
                       (conf     (string-append etc "/ld.so.conf"))
                       (glibc    (assoc-ref inputs "glibc"))
                       (ldconfig (string-append glibc "/sbin/ldconfig"))
                       (glvnd    (assoc-ref inputs "libglvnd")))
                  (mkdir-p etc)

                  ;; Only list GLVND's lib dir so Mesa's libEGL/libGL do not
                  ;; appear in Hyprland's private cache.
                  (call-with-output-file conf
                    (lambda (p)
                      (format p "~a~%" (string-append glvnd "/lib"))))

                  (when (file-exists? cache)
                    (delete-file cache))

                  ;; Generate the cache using our conf.
                  (invoke ldconfig "-C" cache "-f" conf)

                  ;; Optional: sanity check during build logs
                  (invoke ldconfig "-p" "-C" cache)
                  #t))))))))
    ;; Ensure inputs exist under stable names.
    (inputs
     (modify-inputs (package-inputs hyprland)
       (prepend libglvnd)))
    ;; glibc is usually already around, but make it explicit for ldconfig.
    (native-inputs
     (modify-inputs (package-native-inputs hyprland)
       (prepend glibc))))
hyprland-glvnd
