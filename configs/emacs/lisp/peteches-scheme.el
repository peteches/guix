;;; peteches-scheme.el --- Scheme configuration -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Scheme editing, Geiser, and Guile LSP integration.  `guile-lsp-server' is
;;; expected to be installed by Guix.
;;;
;;; Code:

(require 'peteches-lsp)

(message "loading scheme config")

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/area_51/guix"))

(defun peteches/scheme-maybe-start-lsp ()
  "Start Guile LSP when `guile-lsp-server' is available."
  (peteches/lsp-maybe-start 'scheme))

(dolist (hook '(scheme-mode-hook scheme-ts-mode-hook))
  (add-hook hook #'peteches/scheme-maybe-start-lsp))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(".*\\.scm$" . "scheme"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda () (peteches/lsp-server-command 'scheme)))
    :activation-fn (lsp-activate-on "scheme")
    :server-id 'guile-lsp-server)))

(provide 'peteches-scheme)
;;; peteches-scheme.el ends here
