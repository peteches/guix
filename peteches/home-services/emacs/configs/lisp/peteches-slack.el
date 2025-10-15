;;; peteches-slack.el -*- lexical-binding: t; -*-

(require 'slack)   ;; not 'emacs-slack
(require 'alert)
(require 'peteches-first-frame) ;; your first-frame-ready.el

(defconst peteches/slack-name  "ScorePlay")
(defconst peteches/slack-entry "scoreplaytalk.slack.com/peter.mccabe@scoreplay.io")
(defconst peteches/slack-chans '("production-team" "internal-liverpool" "external-liverpool" "tech"))

(defun peteches/slack--team-registered-p (name)
  (and (boundp 'slack-teams)
       (seq-some (lambda (t) (string= name (oref t name))) slack-teams)))

(defun peteches/slack-register-from-pass (frame)
  "Idempotently register the Slack team from pass/auth-source."
  (unless (peteches/slack--team-registered-p peteches/slack-name)
    (let* ((tok (auth-source-pass-get "api_token" peteches/slack-entry))
           (ck  (auth-source-pass-get "cookie"    peteches/slack-entry)))
      (when (and (stringp tok) (> (length tok) 0))
        (apply #'slack-register-team
               (append (list :name peteches/slack-name
                             :token tok
                             :subscribed-channels peteches/slack-chans
                             :default t)
                       (when (and (stringp ck) (> (length ck) 0))
                         (list :cookie ck)))))
      (message "[slack] %s"
               (if (peteches/slack--team-registered-p peteches/slack-name)
                   (format "Registered %s" peteches/slack-name)
                 (format "No token yet for %s" peteches/slack-entry))))))

;; IMPORTANT: run after your frame is truly ready (idle), not on first-frame-hook
(add-hook 'first-frame-ready-hook #'peteches/slack-register-from-pass)

;; Keymap: ensure we always register before starting
(define-prefix-command 'peteches/slack-map)
(global-set-key (kbd "C-c s") 'peteches/slack-map)

(define-key peteches/slack-map (kbd "s") #'slack-start)
(define-key peteches/slack-map (kbd "r") #'slack-reconnect)
(define-key peteches/slack-map (kbd "q") #'slack-ws-close)
(define-key peteches/slack-map (kbd "c") #'slack-channel-select)
(define-key peteches/slack-map (kbd "i") #'slack-im-select)
(define-key peteches/slack-map (kbd "g") #'slack-group-select)
(define-key peteches/slack-map (kbd "u") #'slack-select-unread-rooms)
(define-key peteches/slack-map (kbd "t") #'slack-thread-select)
(define-key peteches/slack-map (kbd "j") #'slack-join)
(define-key peteches/slack-map (kbd "o") #'slack-room-open)
(define-key peteches/slack-map (kbd "m") #'slack-buffer-kill)

;; Usual prefs
(setq slack-buffer-emojify t
      slack-prefer-current-team t
      alert-default-style 'notifications)
(provide 'peteches-slack)
