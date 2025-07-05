(provide 'peteches-auth-sources)
(require 'auth-source)
(require 'password-store)
(require 'password-store-otp)

(message "Loading auth-sources configs")

(setq auth-sources '())
(auth-source-pass-enable)
