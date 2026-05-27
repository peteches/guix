;;; peteches-sops --- SOPS transparent encrypt/decrypt for Emacs

;;; Commentary:
;;; Enables sops-mode globally so Emacs auto-decrypts SOPS-encrypted
;;; files on open and re-encrypts on save.
;;; Requires: sops binary in PATH, GPG key 5FFB7006A36B0AEDE8D08E3753B1E002546A642A

;;; Code:
(use-package sops
  :straight (sops :host github :repo "djgoku/sops")
  :config
  (global-sops-mode 1))

(provide 'peteches-sops)
;;; peteches-sops.el ends here
