(require 'cl-lib)
(require 'pinentry)

(message "Loading pinentry config")

(setq epg-pinentry-mode 'loopback)  ;; force loopback here

(provide 'peteches-pinentry)
