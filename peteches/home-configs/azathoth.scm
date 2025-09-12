(define-module (peteches home-configs azathoth)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages password-utils)
  #:use-module (nongnu packages mozilla)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages video)
  #:use-module (gnu packages qt)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (peteches home-services aws)
  #:use-module (peteches home-services desktop)
  #:use-module (peteches home-services emacs base)
  #:use-module (peteches home-services git)
  #:use-module (peteches home-services firefox)
  #:use-module (peteches home-services hyprland)
  #:use-module (peteches home-services mako)
  #:use-module (peteches home-services nyxt)
  #:use-module (peteches home-services password-store)
  #:use-module (peteches home-services waybar)
  #:use-module (peteches home-services wofi)
  #:use-module (peteches packages scripts)
  #:use-module (peteches home-configs hyprland)
  #:use-module (peteches home-configs mako)
  #:use-module (peteches home-configs waybar)
  #:use-module (peteches home-configs firefox))


(define (get-ssh-host-key hosts)
  (let* ((port (open-input-pipe (string-append "ssh-keyscan " (string-join hosts))))
	 (str  (get-string-all port)))
    (plain-file "known_hosts" str)))

(home-environment
 (packages
  (list
   alacritty
   eza
   git
   gnupg
   jq
   ripgrep
   pinentry-qt
   qtwayland
   ;; shell-scripts
   wofi gawk grimblast clipman zbar pass-otp wf-recorder ;; these should be deps of the shell-scripts package but doesn't work
   btop))
 
 (services
  (append (list
	   
	   (service home-mako-service-type
		    (mako-config))
	   (service home-git-service-type
	     (home-git-configuration
	      (applypatch-msg-hook (list (plain-file "applypatch-msg-hook" "echo applying patch")))
	      (config 
	       (list (git-section
		      (name "user")
		      (config '(("name" . "Pete 'Peteches' McCabe")
				("email" . "pete@peteches.co.uk")
				("signingkey" . "A6E8150FED0029D7"))))
		     (git-section
		      (name "core")
		      (config '(("compression" . "6"))))
		     (git-section
		      (name "init")
		      (config '(("defaultBranch" . "main"))))
		     (git-section
		      (name "commit")
		      (config '(("gpgSign" . "true"))))))))
	   
	   (service home-password-store-service-type
		    (home-password-store-configuration
		     (repo-uri "git@github.com:peteches/password-store.git")
		     (password-store-dir "${HOME}/.local/share/password-store")))
	   
	   (service nyxt-service-type)
	   
	   (service home-openssh-service-type
		    (home-openssh-configuration
		     (known-hosts `(,(get-ssh-host-key '("github.com" "nug.peteches.co.uk"))))
		     (add-keys-to-agent "yes")
		     (hosts
		      (list
		       (openssh-host (name "*")
				     (extra-content "  ServerAliveInterval 5"))
		       (openssh-host (name "github.com")
				     (user "git"))
		       (openssh-host (name "nug.peteches.co.uk")
				     (user "peteches"))))))
	   
	   (service home-gpg-agent-service-type
		    (home-gpg-agent-configuration
		     (pinentry-program (file-append pinentry-qt "/bin/pinentry-qt"))
		     (extra-content "allow-emacs-pinentry")
		     (ssh-support? #t)))
	   
	   (service waybar-service-type
		    (waybar-configuration
		     (config
		      (waybar-config
		       ;; Bar geometry & layout (HiDPI-friendly without custom CSS)
		       (position "top")
		       (height 46)
		       (spacing 10)
		       (margin-top 6)
		       (margin-bottom 6)
		       (margin-left 10)
		       (margin-right 10)
		       (fixed-center #t)
		       (exclusive #t)
		       (reload_style_on_change #t)
		       (output "")

		       ;; Hyprland modules
		       (modules-left  #("hyprland/workspaces" "hyprland/window"))
		       (modules-center #("clock"))
		       (modules-right #("backlight" "wireplumber#source" "wireplumber#sink" "cpu" "memory" "temperature" "network" "battery" "tray"))

		       ;; Module configs
		       (modules-config base-waybar-modules-config)))))
	   
	   (service wofi-service-type)
	   (service home-desktop-service-type)
	   (service home-aws-service-type)
	   (service home-bash-service-type
		    (home-bash-configuration
		     (guix-defaults? #t)
		     (environment-variables '(("CGO_ENABLED" . "1")))))

	     (service firefox-service-type
		    (firefox-configuration
		     ;; ----- two profiles
		     (profiles base-firefox-profiles)

		     ;; ----- global prefs (merged into each profile; profile prefs override)
		     (global-prefs base-firefox-global-prefs)

		     ;; ----- global extensions (same XPI used for all profiles)
		     (global-extensions base-firefox-global-extensions)))
	   
	   (service home-emacs-base-service-type)
	     (service home-hyprland-service-type (home-hyprland-configuration
					       (monitors (list
	     						  (monitor
							   (scale 1.5))))
					       (env-vars (append
							  '(("XCURSOR_THEME" . "Bibata-Modern-Classic")
							    ("XCURSOR_SIZE" . "64"))
							  base-hyprland-env-vars))
					       (variables base-hyprland-variables)
					       (binds (append
						       base-hyprland-default-application-launcher-binds
						       (base-hyprland-window-workspace-binds 9)))
					       (command-execution						 
						(hyprland-execs
						 (exec-once '("waybar" "alacritty" "mako")))))))
	   %base-home-services)))
