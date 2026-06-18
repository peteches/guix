;;; peteches-tramp --- my tramp setup

;;; Commentary:


;;; Code:
(require 'tramp)
(message "loading tramp config")

(setq tramp-remote-path
      (append tramp-remote-path
              (list
	       "tramp-own-remote-path"
	       "/run/privileged/bin"
               "~/.config/guix/current/bin"
               "~/.guix-home/profile/bin"
               "~/.guix-home/profile/sbin"
               "/run/current-system/profile/bin"
               "/run/current-system/profile/sbin")))

;; TRAMP does not use default ssh ControlPath
;; TRAMP overwrites ControlPath settings when initiating ssh sessions. TRAMP does this to fend off a stall if a master session opened outside the Emacs session is no longer open. That is why TRAMP prompts for the password again even if there is an ssh already open.
;; Some ssh versions support a ControlPersist option, which allows to set the ControlPath provided the variable tramp-ssh-controlmaster-options is customized as follows:

(setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
       "-o ControlMaster=auto -o ControlPersist=yes"))

(provide 'peteches-tramp)
;;; peteches-tramp.el ends here
