(define-module (peteches home-configs nug)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages pcre)
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
  #:use-module (guix packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages video)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (peteches home-services ai)
  #:use-module (peteches home-services agixt)
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
  #:use-module (peteches home-configs waybar))

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
   pinentry-qt5
   pre-commit
   ;; shell-scripts
   wofi gawk grimblast clipman zbar pass-otp wf-recorder ;; these should be deps of the shell-scripts package but doesn't work
   btop))

 (services
  (append (list
	   (service firefox-service-type
		    (firefox-configuration
		     (profiles
		      (list
                       (firefox-profile-decl
			(name "Work") (id "work")
			(prefs `(("browser.startup.homepage" . "about:blank"))))
		       (firefox-profile-decl
			(name "Other") (id "other"))
                       (firefox-profile-decl
			(name "Personal") (id "personal")
			(prefs `(("dom.security.https_only_mode" . #t)))))
		      )
		     (global-extensions
		      `(("any@darkreader.org" .
			 ,(local-file "./firefox-extensions/darkreader-firefox-v4.9.110.xpi"))
			("uBlock0@raymondhill.net" .
			 ,(local-file "./firefox-extensions/uBlock0_1.65.0.firefox.signed.xpi"))))))
	   (service ai-service-type '())
	   (service home-mako-service-type
		    base-mako-config)
	   (service home-git-service-type
		    (home-git-configuration
		     (applypatch-msg-hook (list (plain-file "applypatch-msg-hook" "echo applying patch updated")))
		     (pre-commit-hook (list (plain-file "pre-commit" "pre-commit run --hook-stage pre-commit \"$@\"")))
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
	   
					;	   (service nyxt-service-type)

	   (service home-agixt-backend-service-type
		    (agixt-backend-configuration
		     (instance-name "default")
		     (base-uri "http://localhost:7437")
		     (credentials-file ".config/agixt/cred-backend")
		     (respawn? #t)))

	   (service home-agixt-telegram-bots-service-type
		    (agixt-telegram-bots-configuration
		     (bots
		      (list
		       (agixt-telegram-bot-configuration
			(name "leah")
			(backend-instance-name "default")
			(credentials-file ".config/agixt/cred-bot-leah")
			(base-uri "http://localhost:7437")
			(agent "AGiXT")
			(chain "")
			(allowed-user-ids '("7642100300")))
		       (agixt-telegram-bot-configuration
			(name "kim")
			(backend-instance-name "default")
			(credentials-file ".config/agixt/cred-bot-kim")
			(base-uri "http://localhost:7437")
			(agent "Secretary")
			(chain "morning-briefing")
			(allowed-user-ids '("7642100300")))))
		     (respawn? #t)))	   
	   
	   (service home-openssh-service-type
		    (home-openssh-configuration
					;(add-keys-to-agent "yes")
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
		     (pinentry-program  (file-append pinentry-qt5 "/bin/pinentry"))
		     (extra-content
		      (string-append
		       "allow-emacs-pinentry\n"
		       "allow-loopback-pinentry\n"))
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
	   (service home-bash-service-type
		    (home-bash-configuration
		     (guix-defaults? #t)
		     (environment-variables '(("CGO_ENABLED" . "1")))))

	   (service home-emacs-base-service-type)
	   (service home-hyprland-service-type (home-hyprland-configuration
						(monitors (list
	     						   (monitor
							    (name "DP-3")
							    (position "0x0")
							    (scale 1))
							   (monitor
							    (name "DP-2")
							    (position "auto-up"))))
						(env-vars base-hyprland-env-vars)
						(variables base-hyprland-variables)
						(binds (append
							base-hyprland-default-application-launcher-binds
							(base-hyprland-window-workspace-binds 9)))
						(command-execution						 
						 (hyprland-execs
						  (exec-once '("waybar" "mako")))))))
	  %base-home-services)))
