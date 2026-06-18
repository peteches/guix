;;; first-frame-ready.el --- run once after frame is really ready -*- lexical-binding: t; -*-

(defvar first-frame-ready--ran nil
  "Non-nil once all first-frame-ready hook functions have run successfully.")

(defvar first-frame-ready-hook nil
  "Normal hook run after the first usable frame is ready (exactly once).
Each function is called with one arg: the selected FRAME.")

(defcustom first-frame-ready-require-gui nil
  "If non-nil, wait for a GUI frame; otherwise TTY is OK."
  :type 'boolean)

;; Internal bookkeeping:
(defvar first-frame-ready--pending nil
  "Functions from `first-frame-ready-hook' that haven't succeeded yet.")
(defvar first-frame-ready--completed nil
  "Functions from `first-frame-ready-hook' that have already succeeded.")

(defun first-frame-ready--acceptable-p (f)
  (and (frame-live-p f)
       (or (not first-frame-ready-require-gui) (display-graphic-p f))))

(defun first-frame-ready--init-pending ()
  "Initialize the pending list from `first-frame-ready-hook' if needed."
  (unless first-frame-ready--pending
    ;; Hook variables are lists (or nil). Copy to avoid mutation surprises.
    (setq first-frame-ready--pending (copy-sequence first-frame-ready-hook))
    (setq first-frame-ready--completed nil)))

(defun first-frame-ready--done ()
  "Mark completed and remove any retry hooks."
  (setq first-frame-ready--ran t)
  (remove-hook 'after-make-frame-functions #'first-frame-ready--on-new-frame))

(defun first-frame-ready--attempt (frame)
  "Attempt to run any remaining pending hook functions on FRAME.
Failures are kept in `first-frame-ready--pending' for retry on the next acceptable frame."
  (when (and (not first-frame-ready--ran)
             (first-frame-ready--acceptable-p frame))
    ;; Defer one event-loop turn so minibuffer etc. are usable.
    (run-with-idle-timer
     0 nil
     (let ((f frame))
       (lambda ()
         (when (and (not first-frame-ready--ran)
                    (first-frame-ready--acceptable-p f))
           (with-selected-frame f
             ;; Your one-off setup (safe if repeated)
             (ignore-errors (require 'pinentry) (pinentry-start))
             (let ((tok (auth-source-pass-get "token" "github.com/peteches")))
               (when tok (message "[auth] token retrieved (hidden)")))

             ;; Now run user hooks with per-function retry
             (first-frame-ready--init-pending)
             (let ((still '()))
               (dolist (fn first-frame-ready--pending)
                 (condition-case err
                     (progn
                       (funcall fn f)
                       (push fn first-frame-ready--completed))
                   (error
                    (message "first-frame-ready: %S failed: %S" fn err)
                    (push fn still))))
               (setq first-frame-ready--pending (nreverse still)))

             (when (null first-frame-ready--pending)
               (first-frame-ready--done)))))))))

(defun first-frame-ready--on-new-frame (frame)
  "Hook for `after-make-frame-functions' to retry pending work on FRAME."
  (first-frame-ready--attempt frame))

(defun first-frame-ready--bootstrap ()
  "Kick off the initial attempt and, if needed, enable retries on new frames."
  (first-frame-ready--attempt (selected-frame))
  ;; If we didn't finish yet, keep listening for future frames to retry.
  (unless first-frame-ready--ran
    (add-hook 'after-make-frame-functions #'first-frame-ready--on-new-frame)))

;; Daemon: wait for first client frame; non-daemon: run after initial window setup.
(if (daemonp)
    (add-hook 'after-make-frame-functions #'first-frame-ready--on-new-frame)
  (add-hook 'window-setup-hook #'first-frame-ready--bootstrap))

(provide 'peteches-first-frame)
