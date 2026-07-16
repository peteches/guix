;;; peteches-dired --- my dired configs
;;; Commentary:

;;; Code:

;; Make Dired copy/move default to another visible Dired buffer
(setq dired-dwim-target t)

(with-eval-after-load 'image-dired
  (make-directory image-dired-dir t))

(defun peteches/image-dired-find-images-recursive (dir)
  "Browse all images found recursively under DIR with image-dired."
  (interactive "DDirectory: ")
  (let* ((regexp (image-file-name-regexp))
         (files (directory-files-recursively dir regexp)))
    (if (null files)
        (message "No images found under %s" dir)
      (dired (cons (expand-file-name dir) files))
      (dired-mark-files-regexp ".")
      (image-dired-display-thumbs))))

(provide 'peteches-dired)
;;; peteches-dired.el ends here
