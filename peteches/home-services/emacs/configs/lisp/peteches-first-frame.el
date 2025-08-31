;;; first-frame-ready.el --- run once after frame is really ready -*- lexical-binding: t; -*-

(defvar first-frame-ready--ran nil)
(defvar first-frame-ready-hook nil
  "Runs exactly once after the first frame is fully usable.
Each function is called with one arg: the selected FRAME.")

(defcustom first-frame-ready-require-gui nil
  "If non-nil, wait for a GUI frame; otherwise TTY is OK."
  :type 'boolean)

(defun first-frame-ready--acceptable-p (f)
  (and (frame-live-p f)
       (or (not first-frame-ready-require-gui) (display-graphic-p f))))


(defun first-frame-ready--schedule (frame)
  (unless first-frame-ready--ran
    (setq first-frame-ready--ran t)
    (with-selected-frame (or (and (framep frame) frame) (selected-frame))
      ;; Defer one turn of the event loop so minibuffer is usable.
      (run-with-idle-timer
       0 nil
       (lambda ()
         ;; (require 'pinentry) is fine to call again
         (ignore-errors (require 'pinentry) (pinentry-start))
         ;; Now GPG will ask via Emacs' minibuffer.
         (let ((tok (auth-source-pass-get "token" "github.com/peteches")))
           (when tok
             (message "[auth] token retrieved (hidden)"))))))))

;; Daemon: wait for first client frame; non-daemon: run after initial window setup.
(if (daemonp)
    (add-hook 'after-make-frame-functions #'first-frame-ready--schedule)
  (add-hook 'window-setup-hook (lambda () (first-frame-ready--schedule (selected-frame)))))

(provide 'peteches-first-frame)
