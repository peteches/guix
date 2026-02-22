(define-module (peteches home-configs base)
  ;; Guix / Home
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  ;; Packages
  #:use-module (gnu packages admin)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages video)
  #:use-module (nongnu packages mozilla)
  #:use-module (peteches packages gpg)
  #:use-module (peteches packages go-tools)
  #:use-module (peteches packages terraform)
  ;; Your feature modules
  #:use-module (peteches home-services aws)
  #:use-module (peteches home-services desktop)
  #:use-module (peteches home-services emacs base)
  #:use-module (peteches home-services git)
  #:use-module (peteches home-services password-store)
  #:use-module (peteches home-services firefox)
  #:use-module (peteches home-services hyprland)
  #:use-module (peteches home-services mako)
  #:use-module (peteches home-services waybar)
  #:use-module (peteches home-services wofi)
  #:use-module (peteches home-services nyxt)
  ;; Your config fragments
  #:use-module (peteches home-configs scoreplay)
  #:use-module (peteches home-configs git)
  #:use-module (peteches home-configs mako)
  #:use-module (peteches home-configs waybar)
  #:use-module (peteches home-configs firefox)
  #:use-module (peteches home-configs hyprland)
  #:use-module (peteches packages dank-material-shell)
  #:use-module (peteches packages gurps)
  ;; utilities
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  ;; Export
  #:export (base-packages
            base-services
            make-host-home))

;; 1) Shared package set for all machines.
(define-public base-packages
  (list
   alacritty
   dank-material-shell
   recutils
   eza
   tcpdump
   netcat
   (list isc-bind "utils")
   git
   gurpscharactersheet
   gnupg
   jq
   terragrunt
   go-golangci-lint
   pgcli
   ripgrep
   pinentry-qt
   pinentry-tty
   peteches-pinentry-switch
   bibata-cursor-theme
   qtwayland
   unzip
   btop))

;; 2) Shared services (with your existing configs).
(define-public base-services
  (list

   (simple-service 'general-env  home-environment-variables-service-type
		   `(("PATH" . "$HOME/.local/bin:$PATH")
		     ("GOMODCACHE" . "$HOME/.cache/go/mod")
		     ("GOCACHE" . "$HOME/.cache/go/build")
		     ("GOPATH" . "$HOME/state/go")
		     ("GOBIN" . "$HOME/.local/bin")))

   ;; Dbus
   (service home-dbus-service-type)

   (service home-aws-service-type)

   (service home-syncthing-service-type
	    (for-home
	     (syncthing-configuration
	      (config-file
	       (syncthing-config-file
		(gui-address "127.0.0.1:8384")

		;; Define the folder you want to sync.
		;; Use a stable folder ID ("org") across your machines.
		(folders
		 (list (syncthing-folder
			(id "org")
			(label "Org")
			(path (string-append (getenv "HOME") "/area_51/org"))
			(type 'sendreceive) ;default; can be 'sendonly or 'receiveonly
			;; Start paused until you share it with peers (optional):
			(paused? #f)
			(devices
			 (list
			  (syncthing-device
			   (name "nug")
			   (id "END7B5S-OJ73D3S-CCPWFE3-Q45OGLZ-BAGJ63I-CF4YBQ6-6QE2U36-YFLGZA2")
			   (auto-accept-folders? #t))
			  (syncthing-device
			   (name "nyarlothotep")
			   (id "NONO6A6-UEOXJWK-JI5TWRF-5NDMD6H-N3BDTWI-JKRNQ5D-PHO4SSW-UDTHFAL")
			   (auto-accept-folders? #t))
			  (syncthing-device
			   (name "bhiyaki")
			   (id "KPF5CNJ-CE2XGRC-VDDKARM-GIM47XB-UMS63Y7-JY3WX7V-I6PLMS5-GGQB2A7")
			   (auto-accept-folders? #t))))))))))))

   ;; Notifications
   (service home-mako-service-type
            base-mako-config)

   ;; Git config
   (service home-git-service-type
            (home-git-configuration
	     (pre-commit-hook (list (local-file "./git-hooks/pre-commit")))
	     (config git-config)
	     (global-ignore (map gitignore-file
				 `("Global/Backup"
				   "Global/Diff"
				   "Global/Emacs"
				   "Global/Linux"
				   "Go"
				   "CommonLisp"
				   "Elisp"
				   "Scheme")))))

   ;; Password store
   (service home-password-store-service-type
	    (home-password-store-configuration
	     (repo-uri "git@github.com:peteches/password-store.git")
	     (password-store-dir "${HOME}/.local/share/password-store")))

   ;; Browsers (Firefox profiles + Nyxt)
   (service firefox-service-type
            (firefox-configuration (profiles base-firefox-profiles)))
   (service nyxt-service-type)

   (simple-service 'ssh-proxy-fragment
                   home-files-service-type
                   `((".ssh/config.d/peteches-ts-proxy.conf"   ,(mixed-text-file "guix-proxy.conf"
								      "Host *.tailb21dfe.ts.net\n"
								      "  ProxyCommand netns-peteches "
								      (file-append netcat "/bin/nc")
								      " -w 10 %h %p\n"))))
   ;; SSH
   (service home-openssh-service-type
            (home-openssh-configuration
	     (add-keys-to-agent "yes")
	     (authorized-keys (list (local-file "ssh-authorized-keys")))
             (hosts (append
		     (list
		      (openssh-host
		       (name "*")
		       (extra-content (string-append
				       "    ControlMaster auto\n"
				       "    ControlPath ~/.ssh/ctrl-%C\n"
				       "    ControlPersist 10m\n"
				       "    CanonicalizeHostname always\n"
				       "    Include ~/.ssh/config.d/peteches-ts-proxy.conf\n")))
		      (openssh-host
		       (name "nyarlothotep.ts")
		       (host-name "nyarlothotep.tailb21dfe.ts.net"))
		      (openssh-host
		       (name "nug.ts")
		       (host-name "nug.tailb21dfe.ts.net")))
		     %scoreplay-ssh-hosts))))

   ;; GPG Agent
   (service home-gpg-agent-service-type
	    (home-gpg-agent-configuration
	     (pinentry-program (file-append peteches-pinentry-switch "/bin/pinentry-switch"))
	     (extra-content (string-append
			     "log-file ${HOME}/.local/var/log/gpg-agent.log\n"
			     "verbose\n"
			     "disable-scdaemon\n"
			     "allow-emacs-pinentry\n"))
	     (ssh-support? #t)))


   ;; HyprLand
   (service home-hyprland-service-type (home-hyprland-configuration
					(monitors (list
	     					   (monitor
						    (name "DP-3")
						    (position "0x0")
						    (scale 1))
						   (monitor
						    (name "DP-2")
						    (position "auto-up"))))
					(window-rules-v2
					 '("float, class:^(xdg-desktop-portal-.*)$, title:^(File Upload)$"))
					(env-vars base-hyprland-env-vars)
					(variables base-hyprland-variables)
					(binds (append
						base-hyprland-default-application-launcher-binds
						(base-hyprland-window-workspace-binds 9)))
					(command-execution
					 (hyprland-execs
					  (exec-once '("dms run"  "emacs --daemon" "mako" "canberra-gtk-play -i desktop-login"))))))

   ;; Waybar / Wofi
   (service waybar-service-type
            (waybar-configuration
	     (config
	      (waybar-config
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

               (modules-config base-waybar-modules-config)))))
   (service wofi-service-type
	    (wofi-config
	     (matching "multi-contains")
	     (insensitive "true")
	     (close_on_focus_loss "true")
	     (key_up "Ctrl-p")
	     (key_down "Ctrl-n")))
   (service home-emacs-base-service-type)

   (service home-bash-service-type
	    (home-bash-configuration
	     (aliases `(("grhome" . "guix home -L ~/area_51/guix reconfigure ~/area_51/guix/peteches/home-configs/$(hostname).scm")
			("grsys" . "sudo guix system -L ~/area_51/guix reconfigure ~/area_51/guix/peteches/systems/$(hostname).scm")))
	     (guix-defaults? #t)
	     (environment-variables '(("CGO_ENABLED" . "1")))))

   ;; Desktop conveniences (terminals/aliases/env, etc.).  You already have a
   ;; home-desktop-service; keep it as the place to set common env/aliases.
   (service home-desktop-service-type)))
