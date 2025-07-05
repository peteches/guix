(provide 'peteches-slack)
(require 'peteches-auth-sources)
(require 'emacs-slack)
(message "Loading slack configs")

(defun peteches/start-slack ()
  "Wrap starting slack in a function to ensure I can input the gpg passphrase"
  (interactive)
  (slack-register-team
   :name "ScorePlay"
   :token (password-store-get-field "scoreplaytalk.slack.com/peter.mccabe@scoreplay.io" "api_token")
   :cookie (password-store-get-field "scoreplaytalk.slack.com/peter.mccabe@scoreplay.io" "cookie")
   :full-and-display-names t
   :default t
   :modeline-enabled t
   :animate-image t
   :subscribed-channels '("agent-dev", "tech"))
  (slack-start))

;(add-to-list 'after-make-frame-functions 'peteches/start-slack)

(defvar peteches/slack/keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "K" 'slack-stop)
    (define-key map "S" 'peteches/start-slack)
    (define-key map "c" 'slack-select-rooms)
    (define-key map "u" 'slack-select-unread-rooms)
    (define-key map "U" 'slack-user-select)
    (define-key map "s" 'slack-search-from-messages)
    (define-key map "J" 'slack-jump-to-browser)
    (define-key map "j" 'slack-jump-to-app)
    (define-key map "e" 'slack-insert-emoji)
    (define-key map "E" 'slack-message-edit)
    (define-key map "r" 'slack-message-add-reaction)
    (define-key map "t" 'slack-thread-show-or-create)
    (define-key map "g" 'slack-message-redisplay)
    (define-key map "G" 'slack-conversations-list-update-quick)
    (define-key map "q" 'slack-quote-and-reply)
    (define-key map "Q" 'slack-quote-and-reply-with-link)
    map)
  "Peteches Slack Keymap")

(defvar slack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "@" 'slack-message-embed-mention)
    (define-key map "#" 'slack-message-embed-channel)
    map)
  "My keybinds for slack-mode")

(defvar slack-thread-message-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C-c '"  'slack-message-write-another-buffer)
    (define-key map "@" 'slack-message-embed-mention)
    (define-key map "#" 'slack-message-embed-channel)
    map)
  "My keybinds for slack threads")

(defvar slack-message-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C-c '"  'slack-message-write-another-buffer)
    map)
  "My Keybinds for Slack messages")

(defvar slack-message-compose-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C-c '" 'slack-message-send-from-buffer)
    map)
  "My keybinds for slack compose")

(global-set-key (kbd "C-c S") peteches/slack/keymap)
