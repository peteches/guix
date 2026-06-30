;;; peteches-tramp --- my tramp setup

;;; Commentary:
;;; TRAMP setup for Guix / Guix Home hosts.

;;; Code:

(require 'tramp)

(message "loading tramp config")

(setq tramp-remote-path
      '(tramp-own-remote-path
        "~/.guix-home/profile/bin"
        "~/.guix-home/profile/sbin"
        "~/.guix-profile/bin"
        "~/.guix-profile/sbin"
        "~/.config/guix/current/bin"
        "/run/current-system/profile/bin"
        "/run/current-system/profile/sbin"
        "/run/privileged/bin"
        tramp-default-remote-path))

;; Prevent local Guix store paths from leaking into remote Git processes.
;; These are set locally in early-init.el, but they should be recomputed
;; by the remote Git/Guix environment, not inherited from local Emacs.
(dolist (env '("GIT_EXEC_PATH="
               "GIT_SSL_CAINFO="
               "GIT_SSL_CAPATH="
               "CURL_CA_BUNDLE="
               "SSL_CERT_FILE="
               "SSL_CERT_DIR="))
  (add-to-list 'tramp-remote-process-environment env))

(setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
       "-o ControlMaster=auto -o ControlPersist=yes"))

(provide 'peteches-tramp)

;;; peteches-tramp.el ends here
