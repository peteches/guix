(define-module (peteches home-configs waybar)
  #:use-module (peteches home-services waybar))

(define-public base-waybar-modules-config
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
					    (cons "spacing" 8)))))
