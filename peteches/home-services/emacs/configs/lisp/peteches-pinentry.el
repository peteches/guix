(require 'cl-lib)
(require 'pinentry)

(message "Loading pinentry config")

;; Advertise Emacs pinentry globally (works in shells/term, etc.)
(setenv "PINENTRY_USER_DATA" "USE_EMACS 1")
(pinentry-start)
(setq epg-pinentry-mode 'loopback)  ;; force loopback here

(provide 'peteches-pinentry)
