(require 'auth-source)
(require 'peteches-pinentry)

(message "Loading auth-sources configs")

;; Your pass store dir (keep both env var and var for consistency)
(setenv "PASSWORD_STORE_DIR" "/home/peteches/.local/share/password-store")

;; Use your desired GnuPG from Guix (adjust if different)
(setq epg-gpg-program (expand-file-name "~/.guix-home/profile/bin/gpg"))
(setq epg-gpgconf-program (expand-file-name "~/.guix-home/profile/bin/gpgconf"))

(setq auth-sources '())
(setq auth-source-pass-dir (expand-file-name "~/.local/share/password-store"))
(setenv "PASSWORD_STORE_DIR" auth-source-pass-dir)
(setq auth-source-pass-filename auth-source-pass-dir)

(require 'epa-file)  ;; so insert-file-contents can decrypt .gpg
(epa-file-enable)

(require 'auth-source-pass)
(auth-source-pass-enable)
(provide 'peteches-auth-sources)
