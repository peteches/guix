;;; peteches-agent-shell --- Summary -*- lexical-binding: t; -*-
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


(straight-use-package '(agent-shell :host github :repo "xenodium/agent-shell"))

(setenv "CLAUDE_CODE_EXECUTABLE" "/home/peteches/.guix-home/profile/bin/claude")

(setq
 agent-shell-anthropic-claude-acp-command (list  "/home/peteches/.guix-home/profile/bin/claude-agent-acp")
 agent-shell-anthropic-claude-command nil)

(setq agent-shell-mcp-servers
      '(((name . "anvil")
         (command . "/gnu/store/vhkg4avy9zf0kj70dcsmfpymnllkjq1y-bash-5.2.37/bin/bash")
         (args . ("/home/peteches/.config/emacs/straight/repos/anvil.el/anvil-stdio.sh"
                  "--server-id=anvil"
                  "--init-function=anvil-enable"
                  "--stop-function=anvil-disable"))
         (env . ()))
        ((name . "anvil-emacs-eval")
         (command . "/gnu/store/vhkg4avy9zf0kj70dcsmfpymnllkjq1y-bash-5.2.37/bin/bash")
         (args . ("/home/peteches/.config/emacs/straight/repos/anvil.el/anvil-stdio.sh"
                  "--server-id=emacs-eval"))
         (env . ()))))

(provide 'peteches-agent-shell)
;;; peteches-agent-shell.el ends here
