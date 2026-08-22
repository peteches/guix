(define-module (peteches packages rclone)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (gnu packages compression))

(define %version
  "1.75.0")

(define-public rclone
  (package
    (name "rclone")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/rclone/rclone/releases/download/v" version
             "/rclone-v" version "-linux-amd64.zip"))
       (sha256
        ;; guix download https://github.com/rclone/rclone/releases/download/v1.75.0/rclone-v1.75.0-linux-amd64.zip
        (base32 "1apvpb566ini16d4m035hhld071lnqj72wlw01qhw9a8izh08a5a"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (arguments
     (list
      #:install-plan
      #~'(("rclone" "bin/rclone"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'strip-version-dir
            (lambda _
              (rename-file (string-append "rclone-v"
                                          #$%version "-linux-amd64/rclone")
                           "rclone")
              (chmod "rclone" #o755))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://rclone.org")
    (synopsis "Rsync for cloud storage")
    (description
     "Rclone is a command-line program to manage files on cloud storage.
It supports Amazon S3, Dropbox, Google Drive, Backblaze B2, and many
other providers.")
    (license license:expat)))
