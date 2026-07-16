;;; peteches/packages/desktop-scripts.scm — the desktop helper scripts.
;;;
;;; The only package defined in this repo's peteches/packages/ directory —
;;; everything else under (peteches packages …) comes from the external
;;; peteches guix channel.  It lives here because its source is configs/bin/
;;; in this repo.
;;;
;;; Consumed by (peteches home modules base) [base-packages] and by
;;; (peteches home modules theming), which file-appends the cursor scripts
;;; and runs dms-random-wallpaper hourly from mcron.
;;;
;;; The #:install-plan lists scripts EXPLICITLY.  Adding a file to
;;; configs/bin/ is not enough — it must be named here or it will not be
;;; installed, silently.  The `make-executable' phase then chmods everything
;;; in bin/, since copy-build-system does not preserve the +x bit.

(define-module (peteches packages desktop-scripts)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (peteches repository)
  #:export (peteches-desktop-scripts))

(define-public peteches-desktop-scripts
  (package
    (name "peteches-desktop-scripts")
    (version "0.1")
    (source (local-file (repo-directory "configs/bin")
                        #:recursive? #t))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("dms-random-wallpaper" "bin/")
			 ("wofi-dms-random-wallpaper" "bin/")
                         ("peteches-apply-matugen" "bin/")
                         ("setup-phinger-cursors" "bin/")
                         ("colorize-phinger-cursors" "bin/"))
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
