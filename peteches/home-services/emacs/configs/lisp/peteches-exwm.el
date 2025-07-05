(require 'exwm)
(message "loading exwm config")

;; set default workspaces
(setq exwm-workspace-number 5)

(add-hook 'exwm-update-class-hook
	  (lambda ()
	    (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
			(string= "gimp" exwm-instance-name))
	      (exwm-workspace-rename-buffer exwm-class-name))))

(add-hook 'exwm-update-title-hook
	  (lambda ()
	    (when (or (not exwm-instance-name)
		      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
		      (string= "gimp" exwm-instance-name))
	      (exwm-workspace-rename-buffer exwm-title))))

(setq exwm-input-global-keys
      '(
	;; bind s-r to exit char-mode and fullscreen-mode
	([?\s-r] . exwm-reset)

	;; bind s-w to switch workspace interactively
	([?\s-w] . exwm-workspace-switch)

	;; bind s-0 to s-9 to switch to a workspace by it's index
	,@(mapcar (lambda (i)
		    '(,(kbd (format "s-%d" i)) .
		      (lambda ()
			(interactive)
			(exwm-workspace-switch-create ,i))))
		  (number-sequence 0 9))

	;; bind s-d to launch applications
	([?\s-d] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))

	;; bind s-l to slock a simple X display locker
	([?\s-l] . (lambda ()
		     (interactive)
		     (start-process "" nil "/usr/bin/slock")))))

(setq exwm-input-simulation-keys
      '(
	;; movement
	([?\C-b]  . [left])
	([?\M-b] . [C-left])
	([?\C-f] . [right])
	([?\M-f] . [C-right])
	([?\C-p] . [up])
	([?\C-n] . [down])
	([?\C-a] . [home])
	([?\C-e] . [end])
	([?\M-v] . [prior])
	([?\C-v] . [next])
	([?\C-d] . [delete])
	([?\M-k] . [S-end delete])
	;; cut / paste
	([?\C-w] . [?\C-x])
	([?\M-w] . [?\C-c])
	([?\C-y] . [?\C-v])
	;; search
	([?\C-s] . [?\C-f])))

(exwm-enable)
