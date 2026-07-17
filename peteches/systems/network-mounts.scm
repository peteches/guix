;;; peteches/systems/network-mounts.scm — shared network filesystem mounts.
;;;
;;; `scoreplay-cifs-mount' is included in the #:file-systems list of both
;;; desktops (nug.scm, nyarlothotep.scm).  It is NOT auto-mounted:
;;; (mount? #f) means the entry is declared in /etc/fstab but left for
;;; `mount /media/ScorePlay' on demand.  mount-may-fail? and (check? #f)
;;; keep a missing NAS or absent credentials from blocking boot.
;;;
;;; The credentials file (/etc/samba/creds/nas-peteches-scoreplay) is
;;; managed OUTSIDE Guix — it is not a SOPS secret and not in this repo, so
;;; it must be placed by hand on a rebuilt machine or the mount fails with
;;; EACCES.  This differs from the VMs' media mounts (jellyfin, arr,
;;; downloads), which take credentials from /run/secrets via sops.
;;;
;;; uid/gid 1000 hard-codes the peteches user; `noperm' disables client-side
;;; permission checks in favour of those uid/gid + mode options.

(define-module (peteches systems network-mounts)
  #:use-module (gnu))

(define-public scoreplay-cifs-mount
    (file-system
     (device "//nas.peteches.co.uk/ScorePlayMedia")
     (mount? #f)
     (check? #f)
     (mount-may-fail? #t)
     (create-mount-point? #t)
     (mount-point "/media/ScorePlay")
     (type "cifs")
     ;; Mount at boot; remove 'no-auto if you want to mount manually
     (flags '())  ; e.g., '(no-auto) to disable auto-mount
     (options
      ;; Performance + usability:
      ;; - vers=3.1.1: modern SMB
      ;; - credentials=…: you manage this file outside Guix
      ;; - uid/gid + file_mode/dir_mode: give peteches read/write access
      ;; - serverino: correct inode handling with SMB3
      ;; - cache=loose + actimeo=30: speed (looser coherency)
      ;; - iocharset=utf8: filenames
      ;; - noperm: client-side permission checks off (use uid/gid + modes)
      "credentials=/etc/samba/creds/nas-peteches-scoreplay,vers=3.1.1,uid=1000,gid=1000,file_mode=0664,dir_mode=0775,serverino,cache=loose,actimeo=30,iocharset=utf8,noperm")))
