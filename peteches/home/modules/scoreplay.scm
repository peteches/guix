;;; peteches/home/modules/scoreplay.scm — work (ScorePlay) SSH hosts.
;;;
;;; STATUS: unused.  `base-ssh-service' in (peteches home modules ssh) does
;;; not splice %scoreplay-ssh-hosts into its hosts list, and nothing else
;;; imports this module, so these entries never reach ~/.ssh/config.
;;; To activate, import this module from ssh.scm and append the list.
;;;
;;; The work CIFS mount (scoreplay-cifs-mount) is separate and *is* live —
;;; see (peteches systems network-mounts).

(define-module (peteches home modules scoreplay)
  #:use-module (gnu home services ssh))


(define-public %scoreplay-ssh-hosts
  (list
   (openssh-host
    (name "scoreplay-fft-01")
    (host-name "10.109.102.21")
    (user "scoreplay"))
   (openssh-host
    (name "scoreplay-fft-02")
    (host-name "10.109.102.22")
    (user "scoreplay"))
   (openssh-host
    (name "scoreplay-fft-03")
    (host-name "10.109.102.23")
    (user "scoreplay"))))
