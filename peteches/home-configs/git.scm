(define-module (peteches home-configs git)
  #:use-module (peteches home-services git))

(define-public git-config
  (list (git-section
	 (name "user")
	 (config '(("name" . "Pete 'Peteches' McCabe")
		   ("email" . "pete@peteches.co.uk")
		   ("signingkey" . "A6E8150FED0029D7"))))
	(git-section
	 (name "core")
	 (config '(("compression" . "6")
		   ("editor" . "emacsclient --create-frame")
		   ("hooksPath" . "~/.config/git/hooks"))))
	(git-section
	 (name "url \"git@github.com:\"")
	 (config '(("insteadOf" . "https://github.com/"))))
	(git-section
	 (name "pull")
	 (config '(("ff" . "true")
		   ("rebase" . "true"))))
	(git-section
	 (name "rerere")
	 (config '(("autoUpdate" . "true")
		   ("enabled" . "true"))))
	(git-section
	 (name "init")
	 (config '(("defaultBranch" . "main"))))
	(git-section
	 (name "commit")
	 (config '(("gpgSign" . "true"))))))
