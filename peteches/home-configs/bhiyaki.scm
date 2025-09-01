(define-module (peteches home-configs bhiyaki)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages gnupg)
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
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (peteches home-services desktop)
  #:use-module (peteches home-services emacs base)
 ;; #:use-module (peteches home-services firefox)
  #:use-module (peteches home-services git)
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
   v4l-utils
   ;; shell-scripts
   wofi gawk grimblast clipman zbar pass-otp firefox wf-recorder ;; these should be deps of the shell-scripts package but doesn't work
   btop))
 
 (services
  (append (list
	   ;; Example service usage with Modus Vivendi colors
	   (service home-mako-service-type
		    (mako-config
		     (max-history 10)
		     (sort "-time")
		     (on-button-left "invoke-default-action")
		     (on-button-middle "none")
		     (on-button-right "dismiss")
		     (on-touch "dismiss")
		     (on-notify "none")
		     (font "JetBrains Mono 11")
		     ;; Core colors from Modus Vivendi
		     (background-color "#1e1e1eFF")   ; bg-dim
		     (text-color "#FFFFFFFF")          ; fg-main
		     (border-color "#646464FF")        ; border
		     (progress-color "over #2FAFFFEE") ; blue accent (slight translucency)
		     ;; Layout
		     (width 360)
		     (height 160)
		     (outer-margin 0)
		     (margin 12)
		     (padding 10)
		     (border-size 2)
		     (border-radius 6)
		     ;; Behavior
		     (icons 1)
		     (max-icon-size 64)
		     (icon-path "")
		     (icon-location "left")
		     (icon-border-radius 4)
		     (markup 1)
		     (actions 1)
		     (history 1)
		     (format "<b>%s</b>\\n%b")
		     (text-alignment "left")
		     (default-timeout 8000)
		     (ignore-timeout 0)
		     (group-by "")
		     (max-visible 5)
		     ;; Placement
		     (output "")
		     (layer "top")
		     (anchor "top-right")))

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
		       (modules-config
			(list
			 ;; Workspaces (Hyprland)
			 (cons "hyprland/workspaces"
			       (list
				;; show only current monitor’s workspaces; set to true to see all
				(cons "all-outputs" #f)
				(cons "format" "{name}")
				(cons "on-click" "hyprctl dispatch workspace {name}")
				(cons "sort-by-number" #t)))

			 ;; Focused window title (Hyprland)
			 (cons "hyprland/window"
			       (list
				(cons "max-length" 80)
				(cons "separate-outputs" #f)))

			 ;; Center clock
			 (cons "clock"
			       (list
				(cons "format" "{:%a %d %b  %H:%M}")
				(cons "tooltip-format" "{:%Y-%m-%d  %H:%M:%S}")
				(cons "interval" 1)))

			 ;; Speakers / outputs (SINK)
			 (cons "wireplumber#sink"
			       (list
				;; format
				(cons "format" "{volume}% {icon}")
				(cons "format-muted" "")
				(cons "format-icons" (vector "" "" ""))
				(cons "tooltip" #t)
				(cons "tooltip-format" "{node_name}")

				;; actions
				(cons "scroll-step" 5) ; 5% per wheel tick
				(cons "on-click" "helvum") ; or "helvum"
				(cons "on-click-middle" "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")

				;; choose sink on right-click
				(cons "on-click-right"
				      "sh -c 'wpctl status | awk \"/Sinks:/{f=1;next} f&&/^\\t\\t[0-9]+/ {gsub(\\\"\\..*\\\",\\\"\\\",$1); print $1,\\\"\\t\\\",$3}\" \
                                                           | wofi --dmenu | awk \"{print \\$1}\" | xargs -r wpctl set-default'")))

			 ;; Microphones / inputs (SOURCE)
			 (cons "wireplumber#source"
			       (list
				(cons "node-type" "Audio/Source")
				(cons "format" "{volume}% ")
				(cons "format-muted" "")
				(cons "tooltip" #t)
				(cons "tooltip-format" "{node_name}")

				(cons "scroll-step" 5)
				(cons "on-click" "helvum")
				(cons "on-click-middle" "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle")

				;; choose source on right-click
     				(cons "on-click-right"
				      "sh -c 'wpctl status \
        | awk \"/Sources:/{f=1;next} f&&/^\\t\\t[0-9]+/ {gsub(/\\..*/,\"\",$1); print $1\"\\t\"$3} \
        | wofi --dmenu --prompt \"Select mic:\" \
        | awk \"{print $1}\" \
        | xargs -r wpctl set-default'")))

			 ;; Network
			 (cons "network"
			       (list
				(cons "format-wifi" "{essid} {signalStrength}% ")
				(cons "format-ethernet" " {ifname}")
				(cons "format-disconnected" "")

				;; tooltips
				(cons "tooltip" #t) ; default, but explicit is fine
				(cons "tooltip-format" "{ifname} • {ipaddr}/{cidr}") ; fallback for any case not covered below
				(cons "tooltip-format-wifi"
				      "{ifname} • {essid}\nIP: {ipaddr}/{cidr}\nFreq: {frequency} MHz • Signal: {signalStrength}%")
				(cons "tooltip-format-ethernet"
				      "{ifname}\nIP: {ipaddr}/{cidr}\nGW: {gwaddr}")
				(cons "tooltip-format-linked" "{ifname} (link up, no IP)")
				(cons "tooltip-format-disconnected" "No connection")
				(cons "tooltip-format-disabled" "Wi-Fi disabled (rfkill)")
				

				(cons "on-click" "nm-connection-editor")))

			 ;; Battery
			 (cons "battery"
			       (list
				(cons "format" "{capacity}% {icon}")
				(cons "format-charging" "{capacity}% ")
				(cons "format-plugged" "{capacity}% ")
				(cons "format-icons" (vector "" "" "" "" ""))
				(cons "states" (list (cons "good" 80)
						     (cons "warning" 30)
						     (cons "critical" 15)))
				(cons "tooltip" #t)))

			 ;; Brightness (requires brightnessctl)
			 (cons "backlight"
			       (list
				(cons "format" "{percent}% ")
				(cons "on-scroll-up" "brightnessctl s +5%")
				(cons "on-scroll-down" "brightnessctl s 5%-")))

			 ;; CPU / RAM / Temp
			 (cons "cpu"         (list (cons "format" "{usage}% ")
						   (cons "interval" 5)))
			 (cons "memory"      (list (cons "format" "{used:0.1f}G")
						   (cons "interval" 5)))
			 (cons "temperature" (list (cons "format" "{temperatureC}°C ")
						   (cons "critical-threshold" 80)))

			 ;; Tray
			 (cons "tray" (list (cons "icon-size" 22)
					    (cons "spacing" 8)))))))))
	   
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
							    (scale 1.5))))
						(env-vars '(("XCURSOR_SIZE" . "36")
							    ("QT_QPA_PLATFORMTHEME" . "qt6ct")
							    ("NVD_BACKEND" . "direct+")
							    ("XDG_CURRENT_DESKTOP" . "Hyprland")
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
							 (mods "SUPER SHIFT")
							 (key "d")
							 (dispatcher "exec")
							 (params "makoctl dismiss -a"))

							(bind
							 (mods "SUPER SHIFT")
							 (key "Return")
							 (dispatcher "exec")
							 (params "makoctl invoke"))
							
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
							 (params "4"))))
						(command-execution						 
						 (hyprland-execs
						  (exec-once '("waybar" "alacritty" "mako")))))))
	  %base-home-services)))
