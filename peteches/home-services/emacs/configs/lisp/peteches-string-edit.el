;;; peteches-string-edit --- my string edit wrappers
;;;
;;; Commentary:
;;;
;;;
;;; Code:


(defun peteches/replace-str-at-point (new-str)
  "Replace contents of string at point with NEW-STR, keeping the quotes."
  (let ((node (treesit-node-at (point))))
    (unless (and node
		 (member (treesit-node-type node)
			 '("raw_string_literal_content"
			   "interpolated_string_content")))
      (user-error "Not inside a string"))
    (delete-region (treesit-node-start node) (treesit-node-end node))
    (insert new-str)))


(defun peteches/edit-string-at-point ()
  "Use `string-edit` to edit string contents at point."
  (interactive)
  (let ((node (treesit-node-at (point))))
    (unless (and node
		 (member (treesit-node-type node)
			 '("raw_string_literal_content"
			   "interpolated_string_content")))
      (user-error "Not inside a string"))

    (string-edit
     "String at point:"
     (treesit-node-text node)
     #'peteches/replace-str-at-point
     :abort-callback
     (lambda ()
       (exit-recursive-edit)
       (message "Aborted edit")))))


(provide 'peteches-string-edit)
;;; peteches-string-edit.el ends here
