;;; peteches-abbrev --- my abbrevs
;;; Commentary:

;;; Code:
(require 'abbrev)

(dolist (hook '(prog-mode-hook
		text-mode-hook))
  (add-hook hook #'abbrev-mode))

;;; global abbrevs
(dolist (abbrevs '())
  (define-abbrev global-abbrev-table (car abbrevs) (cdr abbrevs)))
(provide 'peteches-abbrev)
;;; peteches-abbrev.el ends here.
