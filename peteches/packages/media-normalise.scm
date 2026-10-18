;;; peteches/packages/media-normalise.scm — the offline library normaliser.
;;;
;;; Packages peteches/packages/media-normalise.sh as a store package and
;;; wraps it with everything it needs at run time (jellyfin-ffmpeg for NVENC,
;;; curl + jq for the Arr APIs, and the usual text tools), because none of
;;; those are on the target VMs' system profiles.
;;;
;;; Consumed by (peteches services media-normalise).

(define-module (peteches packages media-normalise)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages web)
  #:use-module (peteches packages jellyfin)
  #:export (media-normalise))

(define-public media-normalise
  (package
    (name "media-normalise")
    (version "0.1")
    (source (local-file "media-normalise.sh"))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("media-normalise.sh" "bin/media-normalise"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'make-executable-and-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (use-modules (srfi srfi-13))
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin/media-normalise"))
                     (deps (map (lambda (n)
                                  (string-append (assoc-ref inputs n) "/bin"))
                                '("jellyfin-ffmpeg" "curl" "jq" "coreutils"
                                  "findutils" "grep" "gawk"))))
                (chmod bin #o555)
                (wrap-program bin
                  `("PATH" prefix (,(string-join deps ":"))))
                #t))))))
    (inputs (list jellyfin-ffmpeg curl jq coreutils findutils grep gawk))
    (home-page "https://peteches.co.uk")
    (synopsis "Offline re-encode + Arr re-import for the media library")
    (description
     "Re-encodes library video to the house format (4K HEVC Main 10 HDR10,
AAC 5.1 audio, image subtitles dropped, MP4 faststart, capped bitrate) using
NVENC, then imports the result through the owning Arr app's Manual Import API
so it lands in the correct folder and the replaced original is removed.")
    (license license:gpl3+)))
