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
  #:use-module (gnu packages curl)
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
                         ("colorize-phinger-cursors" "bin/")
                         ;; CLI for the fleet WhisperX service (comfyui VM,
                         ;; transcribe.ts.peteches.co.uk) — speaker-labelled
                         ;; transcripts from any audio file.
                         ("transcribe" "bin/"))
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
    ;; `transcribe' shells out to curl, and these scripts get installed into
    ;; profiles that do not otherwise carry curl — running the script with no curl
    ;; on PATH fails with "curl: command not found" (hit live on a headless box).
    ;; Propagating, not merely inputting, is what matters here: the consumer is the
    ;; profile, and only propagated inputs land in it.
    (propagated-inputs (list curl))
    (home-page "https://peteches.co.uk")
    (synopsis "Peteches desktop helper scripts")
    (description
     "Small desktop helper scripts for the Peteches Guix Home configuration.")
    (license license:gpl3+)))
