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
  #:use-module (peteches packages scripts))


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
			(name "Personal") (id "personal")
			(prefs `(("dom.security.https_only_mode" . #t)))))
		      )
		     (global-extensions
		      `(("any@darkreader.org" .
			 ,(local-file "./firefox-extensions/darkreader-firefox-v4.9.110.xpi"))
			("uBlock0@raymondhill.net" .
			 ,(local-file "./firefox-extensions/uBlock0_1.65.0.firefox.signed.xpi"))))))
	   (service home-xdg-base-directories-service-type)
	   (service ai-service-type '())
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
	   
					;	   (service nyxt-service-type)
	   
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

	   (service waybar-service-type (waybar-configuration
					 (config (waybar-config
						  (modules-left #("hyprland/workspaces"))
						  (modules-center #("hyprland/window"))
						  (modules-right #("battery" "clock"))
						  (modules-config '(("clock" ("tooltip-format" . "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>"))))))))
	   
	   (service wofi-service-type)
	   (service home-desktop-service-type)
	   (service home-bash-service-type
		    (home-bash-configuration
		     (guix-defaults? #t)
		     (environment-variables '(("CGO_ENABLED" . "1")))))

	   (service home-emacs-base-service-type)
	   (service home-hyprland-service-type (home-hyprland-configuration
						(monitors (list
							   (monitor)
	     						   (monitor
							    (name "DP-3")
							    (scale 1))
							   (monitor
							    (name "DP-2")
							    (position "auto-up"))))
						(env-vars '(("XCURSOR_SIZE" . "36")

						 ("GBM_BACKEND" . "nvidia-drm")
						 ("WLR_RENDERER_ALLOW_SOFTWARE" . "1")		
						 ("WLR_NO_HARDWARE_CURSORS" . "1")		
						

							    ("QT_QPA_PLATFORMTHEME" . "qt5ct")
							    ("NVD_BACKEND" . "direct+")
							    ("LIBVA_DRIVER_NAME" . "nvidia")
							    ("__GLX_VENDOR_LIBRARY_NAME" . "nvidia")
							    ("XDG_CURRENT_DESKTOP" . "Hyprland")
							    ("WLR_DRM_DEVICES" . "/dev/dri/card1")
							    ("WLR_BACKENDS" . "drm")
							    ("XDG_SESSION_TYPE" . "wayland")
							    ("XDG_SESSION_DESKTOP" . "Hyprland")))
						(variables
						 (home-hyprland-variable-configuration
						  (general (general-category
							    (gaps_in 5)
							    (gaps_out 20)
							    (border_size 2)
							    (col.active_border "rgba(33ccffee) rgba(00ff99ee) 45deg")
							    (col.inactive_border "rgba(595959aa)")
							    (layout "dwindle")))
						  (decoration (decoration-category
							       (rounding 10)
							       (blur (decoration-blur-category
								      (enabled #t)
								      (size 3)
								      (passes 1)))))
						  (input (input-category
							  ;; (touchpad (input-touchpad-category
							  ;; 	      natural_scroll true))
							  (kb_layout "us")
							  (kb_options "ctrl:nocaps")))))
						(binds (list
							(bind
							 (mods "SUPER")
							 (key "Return")
							 (dispatcher "exec")
							 (params "alacritty"))
							(bind
							 (mods "SUPER")
							 (key "d")
							 (dispatcher "exec")
							 (params "wofi --show-run drun"))
							(bind
							 (mods "SUPER")
							 (key "e")
							 (dispatcher "exec")
							 (params "emacsclient -c"))

							(bind
							 (mods "SUPER")
							 (key "d")
							 (dispatcher "exec")
							 (params "wofi --show drun"))

							(bind
							 (mods "SUPER")
							 (key "b")
							 (dispatcher "exec")
							 (params "wofi-firefox.sh"))

							(bind
							 (mods "SUPER")
							 (key "p")
							 (dispatcher "exec")
							 (params "wofi-password.sh"))
							
							(bind
							 (mods "SUPER SHIFT")
							 (key "q")
							 (dispatcher "exit")
							 (params ""))

							(bind
							 (mods "SUPER")
							 (key "q")
							 (dispatcher "killactive")
							 (params ""))

							(bind
							 (mods "SUPER")
							 (key "f")
							 (dispatcher "fullscreen")
							 (params ""))

							(bind
							 (mods "SUPER")
							 (key "t")
							 (dispatcher "togglesplit")
							 (params ""))
							
							(bind
							 (mods "SUPER SHIFT")
							 (key "p")
							 (dispatcher "exec")
							 (params "wofi-screenshot.sh"))

							(bind
							 (mods "SUPER")
							 (key "left")
							 (dispatcher "movefocus")
							 (params "l"))

							(bind
							 (mods "SUPER")
							 (key "right")
							 (dispatcher "movefocus")
							 (params "r"))

							(bind
							 (mods "SUPER")
							 (key "up")
							 (dispatcher "movefocus")
							 (params "u"))

							(bind
							 (mods "SUPER")
							 (key "down")
							 (dispatcher "movefocus")
							 (params "d"))

							(bind
							 (mods "SUPER")
							 (key "1")
							 (dispatcher "workspace")
							 (params "1"))
							(bind
							 (mods "SUPER SHIFT")
							 (key "1")
							 (dispatcher "movetoworkspace")
							 (params "1"))
							(bind
							 (mods "SUPER")
							 (key "2")
							 (dispatcher "workspace")
							 (params "2"))
							(bind
							 (mods "SUPER SHIFT")
							 (key "2")
							 (dispatcher "movetoworkspace")
							 (params "2"))
							(bind
							 (mods "SUPER")
							 (key "3")
							 (dispatcher "workspace")
							 (params "3"))
							(bind
							 (mods "SUPER SHIFT")
							 (key "3")
							 (dispatcher "movetoworkspace")
							 (params "3"))
							(bind
							 (mods "SUPER")
							 (key "4")
							 (dispatcher "workspace")
							 (params "4"))
							(bind
							 (mods "SUPER SHIFT")
							 (key "4")
							 (dispatcher "movetoworkspace")
							 (params "4"))
							(bind
							 (mods "SUPER")
							 (key "5")
							 (dispatcher "workspace")
							 (params "5"))
							(bind
							 (mods "SUPER SHIFT")
							 (key "5")
							 (dispatcher "movetoworkspace")
							 (params "5"))
							(bind
							 (mods "SUPER")
							 (key "6")
							 (dispatcher "workspace")
							 (params "6"))
							(bind
							 (mods "SUPER SHIFT")
							 (key "6")
							 (dispatcher "movetoworkspace")
							 (params "6"))))
						(command-execution						 
						 (hyprland-execs
						  (exec-once '("waybar" "mako")))))))
	  %base-home-services)))
