;;; peteches/packages/media-normalise.scm — the offline library normaliser.
;;;
;;; Packages peteches/packages/media-normalise.sh as a store package and
;;; wraps it with everything it needs at run time (jellyfin-ffmpeg for NVENC,
;;; curl + jq for the Arr APIs, and the text tools the script calls — sed, which
;;; verify() uses to escape the sidecar glob and which lives in (gnu packages
;;; base) as its own package rather than in coreutils, and awk, which verify()
;;; uses for the duration check — plus bash itself, because the copy-build-system
;;; leaves the script's `#!/usr/bin/env bash' shebang alone, so the interpreter
;;; has to be found on the wrapped PATH), because none of those are on the target
;;; VMs' system profiles.
;;;
;;; Consumed by (peteches services media-normalise).

(define-module (peteches packages media-normalise)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
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
                                  "findutils" "grep" "gawk" "sed" "bash"))))
                (chmod bin #o555)
                (wrap-program bin
                  `("PATH" prefix (,(string-join deps ":"))))
                #t))))))
    (inputs (list jellyfin-ffmpeg curl jq coreutils findutils grep gawk sed bash))
    (home-page "https://peteches.co.uk")
    (synopsis "Offline house-format rewrite + Arr re-import for the media library")
    (description
     "Re-writes library video to the house format (HEVC/H.264 stream-copied with its
 colour metadata and HDR intact, WebOS-decodable audio tracks copied and the rest
dropped, no embedded subtitles with text ones exported as sidecar .srt files, MP4
faststart; only a source codec that cannot be stream-copied at all is re-encoded)
and then asks the owning Radarr or Sonarr instance to rescan the folder, so the
rewrite is registered in place and the replaced original can be removed.")
    (license license:gpl3+)))
