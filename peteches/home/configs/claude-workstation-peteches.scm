;;; Home config for the `peteches' account on claude-workstation.
;;;
;;; Run ON THE VM, as the peteches user:
;;;   guix home -L ~/area_51/guix reconfigure \
;;;     ~/area_51/guix/peteches/home/configs/claude-workstation-peteches.scm
;;;
;;; Bootstrap (first time only — this config clones the OTHER repos, but the
;;; guix checkout it lives in must already exist): clone the guix repo by hand
;;; into ~/area_51/guix, then run the command above.
;;;
;;; Evaluates to a bare `home-environment' as its final expression, which is
;;; what `guix home' consumes.

(define-module (peteches home configs claude-workstation-peteches)
  #:use-module (peteches home modules claude-workstation))

;; Repos pre-cloned into ~/area_51/<name>.  Each entry is (NAME URL).
;; EDIT ME: add the repos this account works on, with real clone URLs.
;; SSH URLs need this user's key + known_hosts on the VM at reconfigure time;
;; a clone that can't authenticate just warns and retries next reconfigure.
(define %peteches-repos
  '(("guix"         "git@github.com:peteches/guix")
    ("guix-channel" "ssh://git@codeberg.org/peteches/guix-channel.git")))

(make-claude-workstation-home
 #:git-name "Pete 'Peteches' McCabe"
 #:git-email "claude@peteches.co.uk"
 #:repos %peteches-repos)
