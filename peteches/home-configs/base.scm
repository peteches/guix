(define-module (peteches home-configs base)
  ;; Guix / Home
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  ;; Packages
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages image)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages video)
  #:use-module (nongnu packages mozilla)
  #:use-module (peteches packages go-tools)
  #:use-module (peteches packages terraform)
  #:use-module (gnu packages libcanberra)
  ;; Your feature modules
  #:use-module (peteches home services aws)
  #:use-module (peteches home services desktop)
  #:use-module (peteches home services emacs)
  #:use-module (peteches home services git)
  #:use-module (peteches home services password-store)
  #:use-module (peteches home services firefox)
  #:use-module (peteches home services hyprland)
  #:use-module (peteches home services wofi)
  #:use-module (peteches home services nyxt)
  ;; Your config fragments
  #:use-module (peteches home-configs scoreplay)
  #:use-module (peteches home-configs git)
  #:use-module (peteches home-configs firefox)
  #:use-module (peteches home-configs mako)
  #:use-module (peteches packages gurps)
  #:use-module (peteches packages claude-code)
  #:use-module (peteches packages rustdesk)
  #:use-module (peteches packages concourse)
  #:use-module (peteches packages proxmox-scripts)
  #:use-module (peteches packages desktop-scripts)

  #:use-module (peteches repository)
  ;; utilities
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  ;; Export
  #:export (base-packages
            base-services))

;; 1) Shared package set for all machines.
(define-public base-packages
  (list
   peteches-desktop-scripts
   alacritty
   claude-code
   rustdesk
   dank-material-shell
   mako
   (specification->package "gsettings-desktop-schemas")
   (specification->package "beeper-bin")
   (specification->package "matugen")
   (specification->package "hyprcursor")
   (specification->package "xcur2png")
   (specification->package "curl")
   (specification->package "bzip2")
   (specification->package "dconf")
   (specification->package "qt5ct")
   (specification->package "qt6ct")
   (specification->package "adw-gtk3-theme")
   (specification->package "adwaita-icon-theme")
   (specification->package "font-nerd-symbols")
   (specification->package "font-nerd-jetbrains-mono")
   (specification->package "wl-clipboard")
   (specification->package "playerctl")
   (specification->package "ddcutil")
   (specification->package "pavucontrol-qt")
   recutils
   eza
   fly
   tcpdump
   netcat
   ncdu
   (list isc-bind "utils")
   bluez
   git
   peteches-gpg-for-git
   gurpscharactersheet
   gnupg
   jq
   slurp
   wf-recorder
   terragrunt
   go-golangci-lint
   pgcli
   sops
   ripgrep
   pinentry-qt
   pinentry-tty
   bibata-cursor-theme
   qtwayland
   unzip
   btop
   proxmox-scripts))

;; 2) Shared services (with your existing configs).
(define-public base-services
  (list

   (simple-service 'general-env  home-environment-variables-service-type
		   `(("PATH" . "$HOME/.local/bin:$PATH")
		     ("GOMODCACHE" . "$HOME/.cache/go/mod")
		     ("GOCACHE" . "$HOME/.cache/go/build")
		     ("GOPATH" . "$HOME/state/go")
		     ("GOBIN" . "$HOME/.local/bin")
                     ("HYPRCURSOR_THEME" . "phinger-cursors-dark")
                     ("HYPRCURSOR_SIZE" . "32")
                     ("XCURSOR_THEME" . "phinger-cursors-dark")
                     ("XCURSOR_SIZE" . "32")))
   (simple-service 'peteches-guile-load-path home-environment-variables-service-type
		   `(("GUILE_LOAD_PATH" . "$HOME/area_51/guix:${HOME}/area_51/codeberg.org/peteches/guix-channel.git_main:${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}")))

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
			   (auto-accept-folders? #t))))))))))))

   ;; Notifications are handled by Mako. DMS still provides the shell,
   ;; wallpaper IPC, and the Hyprland submap hint plugin.
   base-mako-service

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

   ;; SSH
   (service home-openssh-service-type
            (home-openssh-configuration
	     (add-keys-to-agent "yes")
	     (authorized-keys (list (local-file "ssh-authorized-keys")))
             (hosts (list
		     (openssh-host
		      (name "*")
		      (extra-content (string-append
				      "    ControlMaster auto\n"
				      "    ControlPath ~/.ssh/ctrl-%C\n"
				      "    ControlPersist 10m\n"
				      "    CanonicalizeHostname always\n")))
		     (openssh-host
		      (name "proxmox1")
		      (host-name "proxmox1.spaniel-cordylus.ts.net")
		      (user "root"))
		     (openssh-host
		      (name "nyarlothotep.ts")
		      (host-name "nyarlothotep.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "nug.ts")
		      (host-name "nug.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "prometheus.ts")
		      (host-name "prometheus.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "grafana.ts")
		      (host-name "grafana.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "prometheus")
		      (host-name "192.168.51.187")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "grafana")
		      (host-name "192.168.51.188")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "pihole")
		      (host-name "192.168.51.189")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "loki.ts")
		      (host-name "loki.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "loki")
		      (host-name "192.168.51.190")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "git.ts")
		      (host-name "git.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "git")
		      (host-name "192.168.51.191")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "jellyfin.ts")
		      (host-name "jellyfin.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "jellyfin")
		      (host-name "192.168.51.192")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "caddy.ts")
		      (host-name "caddy.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "caddy")
		      (host-name "192.168.51.193")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "prowlarr.ts")
		      (host-name "prowlarr.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "prowlarr")
		      (host-name "192.168.51.194")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "arr.ts")
		      (host-name "arr.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "arr")
		      (host-name "192.168.51.195")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "downloads.ts")
		      (host-name "downloads.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "downloads")
		      (host-name "192.168.51.196")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "rustdesk.ts")
		      (host-name "rustdesk.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "rustdesk")
		      (host-name "192.168.51.197")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "concourse-db.ts")
		      (host-name "concourse-db.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "concourse-db")
		      (host-name "192.168.51.198")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "concourse-web01.ts")
		      (host-name "concourse-web01.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "concourse-web01")
		      (host-name "192.168.51.199")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "concourse-worker01.ts")
		      (host-name "concourse-worker01.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "concourse-worker01")
		      (host-name "192.168.51.200")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "vault")
		      (host-name "192.168.51.201")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "vault.ts")
		      (host-name "vault.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "critical-grind-outline")
		      (host-name "192.168.51.203")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))
		     (openssh-host
		      (name "plane.ts")
		      (host-name "plane.spaniel-cordylus.ts.net"))
		     (openssh-host
		      (name "plane")
		      (host-name "192.168.51.204")
		      (user "peteches")
		      (identity-file "~/.ssh/id_ed25519"))))))

   ;; GPG Agent
   (service home-gpg-agent-service-type
	    (home-gpg-agent-configuration
	     ;; Pick a pinentry implementation using PINENTRY_USER_DATA.  Local
	     ;; Emacs sets USE_EMACS=1.  Magit/TRAMP signing uses loopback mode
	     ;; through git's gpg.program wrapper, so this pinentry wrapper remains
	     ;; for direct/local GPG operations.
	     (pinentry-program
	      (program-file
	       "pinentry-peteches"
	       #~(begin
		   (use-modules (srfi srfi-13))

		   (define user-data
		     (or (getenv "PINENTRY_USER_DATA") ""))

		   (define have-gui?
		     (or (getenv "WAYLAND_DISPLAY")
			 (getenv "DISPLAY")))

		   (define-values (program argv0)
		     (cond
		      ((or (string-contains user-data "MAGIT_TRAMP=1")
			   (string-contains user-data "USE_TTY=1"))
		       (values #$(file-append pinentry-tty "/bin/pinentry-tty")
			       "pinentry-tty"))

		      ((string-contains user-data "USE_EMACS=1")
		       (values #$(file-append pinentry-emacs "/bin/pinentry-emacs")
			       "pinentry-emacs"))

		      (have-gui?
		       (values #$(file-append pinentry-qt "/bin/pinentry-qt")
			       "pinentry-qt"))

		      (else
		       (values #$(file-append pinentry-tty "/bin/pinentry-tty")
			       "pinentry-tty"))))

		   (apply execl program argv0 (cdr (command-line))))))
	     (extra-content (string-append
			     "log-file ${HOME}/.local/var/log/gpg-agent.log\n"
			     "verbose\n"
			     "disable-scdaemon\n"
			     "allow-emacs-pinentry\n"
			     "allow-loopback-pinentry\n"))
	     (ssh-support? #t)))


   ;; HyprLand
   (service home-hyprland-service-type
       	    (home-hyprland-configuration
	     (extra-packages '())
	     (config-directory (repo-directory "configs/hypr"))))

   (service wofi-service-type
	    (wofi-config
	     (matching "multi-contains")
	     (insensitive "true")
	     (close_on_focus_loss "true")
	     (key_up "Ctrl-p")
	     (key_down "Ctrl-n")))
   (service home-emacs-base-service-type
	    (home-emacs-base-configuration
	     (config-directory (repo-directory "configs/emacs"))
	     (extra-packages (map specification->package
				  '("emacs-forge"
				    "emacs-olivetti"
				    "emacs-string-inflection"
				    "emacs-ob-go"
				    "emacs-modus-themes"
				    "emacs-gruvbox-theme"
				    "emacs-paredit"
				    "emacs-geiser"
				    "emacs-emojify"
				    "emacs-geiser-guile"
				    "emacs-org-present"
				    "gcc-toolchain" ; to compile vterm / tree-sitter grammars
				    "emacs-vterm"
				    "emacs-eat"

				    "yarn" ; for mcp servers
				    "curl"
				    "emacs-compat"
				    "postgresql"
					;	emacs-go-playground

				    "emacs-orderless"

				    "emacs-show-font"
				    "emacs-projectile"
				    "ripgrep"

				    "emacs-gnus-desktop-notify"

				    "emacs-tramp"

				    "emacs-password-store"
				    "emacs-password-store-otp"
				    "emacs-auth-source-pass"

				    "emacs-marginalia"
				    "emacs-vertico"

				    "emacs-slack"

				    "emacs-which-key"

				    "emacs-magit"

				    "emacs-rainbow-delimiters"

				    ;; Language support.  Keep native tree-sitter grammars
				    ;; Guix-managed; use straight.el for faster-moving
				    ;; Emacs Lisp packages.
				    "tree-sitter"

				    "tree-sitter-yaml"

				    "tree-sitter-go"
				    "tree-sitter-gomod"

				    "tree-sitter-lua"
				    "tree-sitter-luadoc"

				    "tree-sitter-scheme"

				    "tree-sitter-python"

				    "tree-sitter-org"

				    "tree-sitter-markdown"

				    "tree-sitter-json"

				    "tree-sitter-html"

				    "tree-sitter-hcl"

				    "tree-sitter-dockerfile"

				    "tree-sitter-c"

				    "tree-sitter-bash"
				    "tree-sitter-awk"


				    "emacs-go-mode"

				    "emacs-pinentry"

				    "emacs-atom-one-dark-theme"

				    "emacs-company"
				    "emacs-company-quickhelp"
				    "emacs-company-org-block"
				    "emacs-company-emoji"

				    ;; Language server binaries.  Keep these Guix-managed;
				    ;; Emacs only starts servers already present on exec-path.
				    "guile-lsp-server"
				    "lua-language-server"
				    "go"
				    "gopls"
				    "sqls"

				    "emacs-yaml-mode"

				    ;; mcp stuff
				    "mcp-server-filesystem-go"
				    "go-github-com-sonirico-mcp-shell"

				    "emacs-mcp"

				    "emacs-all-the-icons"
				    "emacs-all-the-icons-dired"
				    "emacs-all-the-icons-ibuffer"
				    "emacs-all-the-icons-completion"

				    "font-google-noto"
				    "font-google-noto-emoji"
				    "font-iosevka@33.3.0")))))


   (service home-bash-service-type
	    (home-bash-configuration
	     (aliases `(("grhome" . "guix home -L ~/area_51/guix reconfigure ~/area_51/guix/peteches/home-configs/$(hostname).scm")
			("grsys" . "sudo guix system -L ~/area_51/guix reconfigure ~/area_51/guix/peteches/systems/$(hostname).scm")))
	     (guix-defaults? #t)
	     (environment-variables '(("CGO_ENABLED" . "1")))))

   ;; Desktop conveniences (terminals/aliases/env, etc.).  You already have a
   ;; home-desktop-service; keep it as the place to set common env/aliases.
   (service home-desktop-service-type)

   (simple-service 'phinger-cursor-theme
                   home-activation-service-type
                   #~(begin
                       (use-modules (guix build utils))
                       (let* ((home (getenv "HOME"))
                              (state-dir (string-append home "/.local/state/peteches"))
                              (script #$(file-append peteches-desktop-scripts "/bin/setup-phinger-cursors")))
                         (mkdir-p state-dir)
                         (system* script "--variant" "dark" "--size" "32" "--best-effort"))))

   (simple-service 'random-wallpaper-hourly
                   home-mcron-service-type
                   (list
                    #~(job '(next-hour '(0 1 2 3 4 5 6 7 8 9 10 11
                                           12 13 14 15 16 17 18 19 20 21 22 23))
                           "state_dir=\"${XDG_STATE_HOME:-$HOME/.local/state}/peteches\"; mkdir -p \"$state_dir\"; script=\"$HOME/.guix-home/profile/bin/dms-random-wallpaper\"; test -x \"$script\" && \"$script\" >> \"$state_dir/dms-random-wallpaper-hourly.log\" 2>&1 || true")))

   (simple-service 'matugen-material-style
                   home-xdg-configuration-files-service-type
                   `(("alacritty/alacritty.toml"
                      ,(local-file (source-path "configs/alacritty/alacritty.toml")))
                     ("wofi/style.css"
                      ,(local-file (source-path "configs/wofi/style.css")))
                     ("matugen/templates/wofi-colors.css"
                      ,(local-file (source-path "configs/matugen/templates/wofi-colors.css")))
                     ("matugen/templates/hypr-colors.lua"
                      ,(local-file (source-path "configs/matugen/templates/hypr-colors.lua")))
                     ("matugen/templates/emacs-theme.el"
                      ,(local-file (source-path "configs/matugen/templates/emacs-theme.el")))
                     ("matugen/templates/alacritty-colors.toml"
                      ,(local-file (source-path "configs/matugen/templates/alacritty-colors.toml")))
                     ("matugen/templates/mako-colors.conf"
                      ,(local-file (source-path "configs/matugen/templates/mako-colors.conf")))))

   (simple-service 'matugen-dms-custom-templates
                   home-activation-service-type
                   #~(begin
                       (use-modules (guix build utils))
                       (let* ((home (getenv "HOME"))
                              (matugen-dir (string-append home "/.config/matugen"))
                              (wofi-dir (string-append home "/.config/wofi"))
                              (hypr-cache-dir (string-append home "/.cache/matugen"))
                              (emacs-theme-dir (string-append home "/.cache/matugen/emacs"))
                              (alacritty-theme-dir (string-append home "/.cache/matugen/alacritty"))
                              (mako-theme-dir (string-append home "/.cache/matugen/mako"))
                              (config-file (string-append matugen-dir "/config.toml"))
                              (wofi-colors (string-append wofi-dir "/matugen-colors.css"))
                              (hypr-colors (string-append hypr-cache-dir "/hypr-colors.lua"))
                              (emacs-theme (string-append emacs-theme-dir "/matugen-theme.el"))
                              (alacritty-colors (string-append alacritty-theme-dir "/colors.toml"))
                              (mako-colors (string-append mako-theme-dir "/colors.conf")))
			 (mkdir-p matugen-dir)
			 (mkdir-p wofi-dir)
			 (mkdir-p hypr-cache-dir)
			 (mkdir-p emacs-theme-dir)
			 (mkdir-p alacritty-theme-dir)
			 (mkdir-p mako-theme-dir)
			 (call-with-output-file config-file
                           (lambda (port)
                             (display "[config]\n\n" port)
                             (display "[templates.wofi]\n" port)
                             (display (string-append "input_path = '" home "/.config/matugen/templates/wofi-colors.css'\n") port)
                             (display (string-append "output_path = '" home "/.config/wofi/matugen-colors.css'\n\n") port)
                             (display "[templates.hyprland_lua]\n" port)
                             (display (string-append "input_path = '" home "/.config/matugen/templates/hypr-colors.lua'\n") port)
                             (display (string-append "output_path = '" home "/.cache/matugen/hypr-colors.lua'\n\n") port)
                             (display "[templates.emacs]\n" port)
                             (display (string-append "input_path = '" home "/.config/matugen/templates/emacs-theme.el'\n") port)
                             (display (string-append "output_path = '" home "/.cache/matugen/emacs/matugen-theme.el'\n\n") port)
                             (display "[templates.alacritty]\n" port)
                             (display (string-append "input_path = '" home "/.config/matugen/templates/alacritty-colors.toml'\n") port)
                             (display (string-append "output_path = '" home "/.cache/matugen/alacritty/colors.toml'\n\n") port)
                             (display "[templates.mako]\n" port)
                             (display (string-append "input_path = '" home "/.config/matugen/templates/mako-colors.conf'\n") port)
                             (display (string-append "output_path = '" home "/.cache/matugen/mako/colors.conf'\n") port)))
			 (unless (file-exists? wofi-colors)
                           (call-with-output-file wofi-colors
                             (lambda (port)
                               (display "@define-color background #1e1e2e;\n" port)
                               (display "@define-color on_background #cdd6f4;\n" port)
                               (display "@define-color surface #1e1e2e;\n" port)
                               (display "@define-color on_surface #cdd6f4;\n" port)
                               (display "@define-color surface_container #313244;\n" port)
                               (display "@define-color surface_container_high #45475a;\n" port)
                               (display "@define-color surface_container_highest #585b70;\n" port)
                               (display "@define-color primary #89b4fa;\n" port)
                               (display "@define-color on_primary #11111b;\n" port)
                               (display "@define-color secondary #94e2d5;\n" port)
                               (display "@define-color tertiary #cba6f7;\n" port)
                               (display "@define-color outline #6c7086;\n" port))))
			 (unless (file-exists? hypr-colors)
                           (call-with-output-file hypr-colors
                             (lambda (port)
                               (display "return {\n" port)
                               (display "  active_border = \"rgba(89b4faff)\",\n" port)
                               (display "  inactive_border = \"rgba(313244aa)\",\n" port)
                               (display "  group_active = \"rgba(cba6f7ff)\",\n" port)
                               (display "  group_inactive = \"rgba(313244aa)\",\n" port)
                               (display "  locked_active = \"rgba(f38ba8ff)\",\n" port)
                               (display "  locked_inactive = \"rgba(45475aaa)\",\n" port)
                               (display "  background = \"rgba(11111bff)\",\n" port)
                               (display "}\n" port))))
			 (unless (file-exists? emacs-theme)
                           (call-with-output-file emacs-theme
                             (lambda (port)
                               (display ";;; matugen-theme.el --- fallback generated by Guix activation -*- lexical-binding: t; -*-\n" port)
                               (display "(deftheme matugen \"Fallback theme used before matugen generates wallpaper colours.\")\n" port)
                               (display "(let ((class '((class color) (min-colors 89))))\n" port)
                               (display "  (custom-theme-set-faces\n" port)
                               (display "   'matugen\n" port)
                               (display "   `(default ((,class (:background \"#1e1e2e\" :foreground \"#cdd6f4\"))))\n" port)
                               (display "   `(cursor ((,class (:background \"#89b4fa\"))))\n" port)
                               (display "   `(region ((,class (:background \"#313244\" :foreground \"#cdd6f4\"))))\n" port)
                               (display "   `(mode-line ((,class (:background \"#45475a\" :foreground \"#cdd6f4\"))))\n" port)
                               (display "   `(mode-line-inactive ((,class (:background \"#313244\" :foreground \"#9399b2\"))))\n" port)
                               (display "   `(font-lock-keyword-face ((,class (:foreground \"#89b4fa\" :weight bold))))\n" port)
                               (display "   `(font-lock-string-face ((,class (:foreground \"#94e2d5\"))))\n" port)
                               (display "   `(font-lock-comment-face ((,class (:foreground \"#6c7086\" :slant italic))))\n" port)
                               (display "   `(font-lock-function-name-face ((,class (:foreground \"#cba6f7\"))))))\n" port)
                               (display "(provide-theme 'matugen)\n" port))))
			 (unless (file-exists? alacritty-colors)
                           (call-with-output-file alacritty-colors
                             (lambda (port)
                               (display "# Fallback Alacritty colours used before matugen runs.\n" port)
                               (display "[colors]\n" port)
                               (display "draw_bold_text_with_bright_colors = true\n" port)
                               (display "transparent_background_colors = false\n\n" port)
                               (display "[colors.primary]\n" port)
                               (display "background = \"#1e1e2e\"\n" port)
                               (display "foreground = \"#cdd6f4\"\n" port)
                               (display "dim_foreground = \"#6c7086\"\n" port)
                               (display "bright_foreground = \"#ffffff\"\n\n" port)
                               (display "[colors.cursor]\n" port)
                               (display "text = \"#1e1e2e\"\n" port)
                               (display "cursor = \"#89b4fa\"\n\n" port)
                               (display "[colors.vi_mode_cursor]\n" port)
                               (display "text = \"#1e1e2e\"\n" port)
                               (display "cursor = \"#cba6f7\"\n\n" port)
                               (display "[colors.selection]\n" port)
                               (display "text = \"#cdd6f4\"\n" port)
                               (display "background = \"#313244\"\n\n" port)
                               (display "[colors.normal]\n" port)
                               (display "black = \"#45475a\"\nred = \"#f38ba8\"\ngreen = \"#94e2d5\"\nyellow = \"#f9e2af\"\nblue = \"#89b4fa\"\nmagenta = \"#cba6f7\"\ncyan = \"#94e2d5\"\nwhite = \"#cdd6f4\"\n\n" port)
                               (display "[colors.bright]\n" port)
                               (display "black = \"#6c7086\"\nred = \"#f38ba8\"\ngreen = \"#94e2d5\"\nyellow = \"#f9e2af\"\nblue = \"#89b4fa\"\nmagenta = \"#cba6f7\"\ncyan = \"#94e2d5\"\nwhite = \"#ffffff\"\n" port))))
			 (unless (file-exists? mako-colors)
                           (call-with-output-file mako-colors
                             (lambda (port)
                               (display "# Fallback Mako colours used before matugen runs.\n" port)
                               (display "background-color=#1e1e2eF2\n" port)
                               (display "text-color=#cdd6f4FF\n" port)
                               (display "border-color=#89b4faFF\n" port)
                               (display "progress-color=over #89b4faFF\n\n" port)
                               (display "[actionable]\n" port)
                               (display "border-color=#cba6f7FF\n\n" port)
                               (display "[urgency=critical]\n" port)
                               (display "background-color=#f38ba8F2\n" port)
                               (display "text-color=#11111bFF\n" port)
                               (display "border-color=#f38ba8FF\n" port)))))))
   (simple-service 'dms-submap-plugin
		   home-xdg-configuration-files-service-type
		   `(("DankMaterialShell/plugins/hyprSubmapHint"
		      ,(local-file (repo-directory "configs/dms/plugins/hyprSubmapHint")
				   #:recursive? #t))))

   ;; Configure MCP servers for both Claude Code CLI and ECA.
   ;; The MCP server JSON block is built once and reused for both
   ;; ~/.claude/settings.json and ~/.config/eca/config.json.
   (simple-service 'ai-mcp-config
                   home-activation-service-type
                   #~(begin
                       (use-modules (ice-9 popen) (ice-9 textual-ports))
                       (let* ((home        (getenv "HOME"))
                              (bash-path   #$(file-append bash "/bin/bash"))
                              (script-path (string-append home
							  "/.config/emacs/straight/repos/anvil.el/anvil-stdio.sh"))
                              (mcp-json
                               (string-append
                                "{"
                                "\"anvil\":{"
                                "\"type\":\"stdio\","
                                "\"command\":\"" bash-path "\","
                                "\"args\":[\"" script-path "\","
                                "\"--server-id=anvil\","
                                "\"--init-function=anvil-enable\","
                                "\"--stop-function=anvil-disable\"],"
                                "\"env\":{}},"
                                "\"anvil-emacs-eval\":{"
                                "\"type\":\"stdio\","
                                "\"command\":\"" bash-path "\","
                                "\"args\":[\"" script-path "\","
                                "\"--server-id=emacs-eval\"],"
                                "\"env\":{}}"
                                "}"))
                              (claude-file (string-append home "/.claude/settings.json"))
                              (claude-tmp  (string-append claude-file ".guix-tmp"))
                              (eca-dir     (string-append home "/.config/eca"))
                              (eca-file    (string-append eca-dir "/config.json")))
                         (when (file-exists? claude-file)
                           (let* ((jq-filter (string-append ".mcpServers = " mcp-json))
                                  (pipe (open-pipe* OPEN_READ
                                                    #$(file-append jq "/bin/jq")
                                                    jq-filter
                                                    claude-file))
                                  (output (get-string-all pipe)))
                             (close-pipe pipe)
                             (call-with-output-file claude-tmp
                               (lambda (port) (display output port)))
                             (rename-file claude-tmp claude-file)))
                         (unless (file-exists? eca-dir)
                           (mkdir eca-dir))
                         (call-with-output-file eca-file
                           (lambda (port)
                             (display
                              (string-append
                               "{"
                               "\"providers\":{"
                               "\"koboldcpp\":{"
                               "\"api\":\"openai-chat\","
                               "\"url\":\"https://nug.peteches.co.uk:5001\","
                               "\"key\":\"local\","
                               "\"completionUrlRelativePath\":\"/v1/chat/completions\","
                               "\"models\":{\"default\":{}}"
                               "}},"
                               "\"agents\":{"
                               "\"code\":{\"provider\":\"koboldcpp\",\"model\":\"default\"},"
                               "\"chat\":{\"provider\":\"koboldcpp\",\"model\":\"default\"}"
                               "},"
                               "\"mcpServers\":" mcp-json
                               "}")
                              port))))))
   ))
