(define-module (peteches home-configs base)
  ;; Guix / Home
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  ;; Packages
  #:use-module (gnu packages admin)
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
  ;; Your feature modules
  #:use-module (peteches home-services desktop)
  #:use-module (peteches home-services emacs base)
  #:use-module (peteches home-services git)
  #:use-module (peteches home-services guix)
  #:use-module (peteches home-services password-store)
  #:use-module (peteches home-services firefox)
  #:use-module (peteches home-services hyprland)
  #:use-module (peteches home-services mako)
  #:use-module (peteches home-services waybar)
  #:use-module (peteches home-services wofi)
  #:use-module (peteches home-services nyxt)
  ;; Your config fragments
  #:use-module (peteches home-configs git)
  #:use-module (peteches home-configs mako)
  #:use-module (peteches home-configs waybar)
  #:use-module (peteches home-configs firefox)
  #:use-module (peteches home-configs hyprland)
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
   eza
   git
   gnupg
   jq
   ripgrep
   pinentry-qt
   pinentry-tty
   peteches-pinentry-switch
   bibata-cursor-theme
   qtwayland
   btop))

(define (get-ssh-host-key hosts)
  (let* ((port (open-input-pipe (string-append "ssh-keyscan " (string-join hosts))))
	 (str  (get-string-all port)))
    (plain-file "known_hosts" str)))

;; 2) Shared services (with your existing configs).
(define-public base-services
  (list

   ;; Dbus
   (service home-dbus-service-type)

   ;; guix
   (service home-guix-service-type)
   
   ;; Notifications
   (service home-mako-service-type
            base-mako-config)

   ;; Git config
   (service home-git-service-type
            (home-git-configuration (config git-config)))

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
	     (known-hosts `(,(get-ssh-host-key '("github.com" "nug.peteches.co.uk"))))
	     (add-keys-to-agent "yes")
             ;; Keep the permissive default host block; host-specific entries
             ;; can be appended per-machine via extras.
             (hosts '())))

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
					  (exec-once '("waybar" "mako"))))))
   
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
	       (modules-right #("wireplumber#source" "wireplumber#sink" "cpu" "memory" "temperature" "network" "tray"))

               (modules-config base-waybar-modules-config)))))
   (service wofi-service-type)
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

;; 3) Helper to compose a host home without repeating the base.
(define* (make-host-home #:key
                         (host-packages '())
                         (host-services '())
                         (extra-env '()))
  ;; Let the desktop service carry extra env vars host-by-host without
  ;; duplicating everything.
  (home-environment
   (packages (append base-packages host-packages))
   (services
    (append
     base-services
     (list
      (simple-service 'base-vars home-environment-variables-service-type
               extra-env))
     host-services))))
