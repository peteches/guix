;;; tsmark-core.el --- Multi-backend Tree-sitter marking core -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'treesit)

(defgroup tsmark nil
  "Tree-sitter based region-marking commands with pluggable backends."
  :group 'editing)

;;;; Backend registry --------------------------------------------------------

(cl-defstruct tsmark-backend
  name                   ; symbol identifying the backend
  predicate              ; fn () -> non-nil if backend applies in current buffer
  range-func             ; fn (inside) -> (cons BEG END) for enclosing function
  range-call             ; fn (inside) -> (cons BEG END) for enclosing call
  range-delimited)       ; fn (inside) -> (cons BEG END) for string/paren/brace

(defvar tsmark--backends nil
  "Registered tsmark backends.")

(defun tsmark-register-backend (backend)
  "Register BACKEND (a `tsmark-backend' struct)."
  (setq tsmark--backends (cons backend tsmark--backends)))

(defun tsmark--active-backend ()
  "Return the first backend whose PREDICATE matches, or nil."
  (cl-find-if (lambda (b)
                (condition-case _
                    (funcall (tsmark-backend-predicate b))
                  (error nil)))
              tsmark--backends))

;;;; Helpers ----------------------------------------------------------------

(defun tsmark--treesit-ready-p ()
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-at (point))))

(defun tsmark--set-region (beg end)
  (goto-char beg)
  (push-mark end nil t)
  (activate-mark))

(defun tsmark--inside-p (prefix)
  "Return non-nil if we should mark the inside (exclude delimiters/header)."
  (and prefix (or (eq prefix t) (listp prefix) (numberp prefix))))

;;;; Public commands ---------------------------------------------------------

;;;###autoload
(defun tsmark-mark-enclosing-function (arg)
  "Mark the enclosing function. With C-u ARG, select inside only."
  (interactive "P")
  (unless (tsmark--treesit-ready-p)
    (user-error "Tree-sitter not active in this buffer"))
  (let* ((backend (or (tsmark--active-backend)
                      (user-error "No tsmark backend available for this buffer")))
         (fn (tsmark-backend-range-func backend))
         (range (funcall fn (tsmark--inside-p arg))))
    (tsmark--set-region (car range) (cdr range))))

;;;###autoload
(defun tsmark-mark-call (arg)
  "Mark the enclosing function call (incl. name + args). With C-u ARG, select args only."
  (interactive "P")
  (unless (tsmark--treesit-ready-p)
    (user-error "Tree-sitter not active in this buffer"))
  (let* ((backend (or (tsmark--active-backend)
                      (user-error "No tsmark backend available for this buffer")))
         (fn (tsmark-backend-range-call backend))
         (range (funcall fn (tsmark--inside-p arg))))
    (tsmark--set-region (car range) (cdr range))))

;;;###autoload
(defun tsmark-mark-delimited (arg)
  "Mark nearest enclosing string/paren/brace/bracket. With C-u ARG, inside only."
  (interactive "P")
  (unless (tsmark--treesit-ready-p)
    (user-error "Tree-sitter not active in this buffer"))
  (let* ((backend (or (tsmark--active-backend)
                      (user-error "No tsmark backend available for this buffer")))
         (fn (tsmark-backend-range-delimited backend))
         (range (funcall fn (tsmark--inside-p arg))))
    (tsmark--set-region (car range) (cdr range))))

;;;; Optional minor mode -----------------------------------------------------

(defvar tsmark-prefix-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") (lambda () (interactive) (tsmark-mark-enclosing-function t)))
    (define-key m (kbd "c") (lambda () (interactive) (tsmark-mark-call t)))
    (define-key m (kbd "d") (lambda () (interactive) (tsmark-mark-delimited t)))
    m)
  "Prefix map for inside-only tsmark commands.")

;;;###autoload
(define-minor-mode tsmark-mode
  "Minor mode providing tsmark commands and a C-c u prefix."
  :init-value nil
  :lighter " tsmark"
  :keymap (let ((m (make-sparse-keymap)))
            (define-key m (kbd "C-c u") tsmark-prefix-map)
            ;; Direct mark bindings on C-c m
            (define-key m (kbd "C-c m f") #'tsmark-mark-enclosing-function)
            (define-key m (kbd "C-c m c") #'tsmark-mark-call)
            (define-key m (kbd "C-c m d") #'tsmark-mark-delimited)
            m) )

(provide 'tsmark-core)
;;; tsmark-core.el ends here



