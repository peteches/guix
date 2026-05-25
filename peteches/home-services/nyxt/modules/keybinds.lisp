(define-configuration buffer
  ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

(define-mode peteches-mode nil
  "Custom keybindings."
  ((keyscheme-map
    (define-keyscheme-map "peteches-mode" ()
      nyxt/keyscheme:emacs
      (list
       "M-x" 'execute-command
       "C-Spc" 'nothing
       "C-o" 'follow-hint
       "M-o" 'follow-hint-new-buffer-focus
       "C-c p" 'set-buffer-proxy)))))

(define-configuration buffer
  ((default-modes (append '(peteches-mode) %slot-value%))))
