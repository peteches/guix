;;; peteches-anvil.el --- Anvil MCP server (Emacs → Claude) -*- lexical-binding: t; -*-

;;; Code:

(straight-use-package '(anvil :host github :repo "zawatton/anvil.el" :tag "v1.3.0"))
(straight-use-package '(anvil-ide :host github :repo "zawatton/anvil-ide.el" :branch "main"))

(require 'anvil)

(let ((script (expand-file-name "straight/repos/anvil.el/anvil-stdio.sh"
                                user-emacs-directory)))
  (when (file-exists-p script)
    (set-file-modes script (logior (file-modes script) #o111))))

(setq anvil-modules
      '(worker eval org file host git proc fs emacs text clipboard data net))

(anvil-server-start)
(anvil-enable)


(provide 'peteches-anvil)
;;; peteches-anvil.el ends here
