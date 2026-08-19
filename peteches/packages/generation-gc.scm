(define-module (peteches packages generation-gc)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define-public generation-gc
  (package
    (name "generation-gc")
    (version "0.1.0")
    (synopsis "Delete old system generations and run guix gc")
    (description
     "A script that deletes all but the most recent N system generations
and runs guix gc to reclaim the freed store items.  Written to fix the
exact failure mode diagnosed on jellyfin: unchecked Guix generations
accumulate indefinitely, eventually exhausting a VM's inode table and
blocking every guix operation, including the fix itself.  Intended to run
locally as root via a weekly system mcron job -- see
%vm-generation-gc-service in (peteches systems vm-base).")
    (license gpl2)
    (home-page "https://git.peteches.co.uk")
    (source
     (local-file "../../scripts" "generation-gc-scripts"
                 #:recursive? #t))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("generation-gc" "bin/"))))))
