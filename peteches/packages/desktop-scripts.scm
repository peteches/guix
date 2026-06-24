(define-module (peteches packages desktop-scripts)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (peteches-desktop-scripts))

(define (source-path path)
  (or (search-path %load-path path)
      (error "could not find file in %load-path" %load-path path)))

(define-public peteches-desktop-scripts
  (package
    (name "peteches-desktop-scripts")
    (version "0.1")
    (source (local-file (source-path "configs/bin")
                        #:recursive? #t))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("dms-random-wallpaper" "bin/")
                         ("setup-phinger-cursors" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'make-executable
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (for-each
                 (lambda (file)
                   (chmod file #o555))
                 (find-files bin)))
              #t)))))
    (home-page "https://peteches.co.uk")
    (synopsis "Peteches desktop helper scripts")
    (description
     "Small desktop helper scripts for the Peteches Guix Home configuration.")
    (license license:gpl3+)))
