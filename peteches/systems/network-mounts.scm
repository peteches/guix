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
