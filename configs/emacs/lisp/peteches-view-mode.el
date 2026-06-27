;;; peteches-view-mode --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Author: Pete "Peteches" McCabe
;;; Maintainer: Pete "Peteches" McCabe
;;; Version: 0.1.0
;;; Package-Requires: ((Emacs "30.2"))
;;; Keywords: convenience
;;; URL: https://github.com/peteches/guix
;; Description: Description.

;;; Code:

(defun peteches/view-gzip-files ()
  "Use `view-mode' when visiting gzip-compressed files."
  (when (and buffer-file-name
             (string-match-p "\\.gz\\'" buffer-file-name))
    (view-mode 1)))

(add-hook 'find-file-hook #'peteches/view-gzip-files)

(provide 'peteches-view-mode)
;;; conf emac lisp peteches-view-mode.el ends here
