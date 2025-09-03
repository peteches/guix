(define-module (peteches home-configs hyprland)
  #:use-module (srfi srfi-1)
  #:use-module (peteches home-services hyprland))

(define-public base-hyprland-env-vars
  `(("XCURSOR_SIZE" . "36")
    ("QT_QPA_PLATFORMTHEME" . "qt6ct")
    ("NVD_BACKEND" . "direct+")
    ("XDG_CURRENT_DESKTOP" . "Hyprland")
    ("XDG_SESSION_TYPE" . "wayland")
    ("XDG_SESSION_DESKTOP" . "Hyprland")))

(define-public base-hyprland-variables
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

(define-public base-hyprland-default-application-launcher-binds
  (list
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
    (key "p")
    (dispatcher "exec")
    (params "wofi-screenshot.sh"))
   
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
    (params "wofi-password.sh"))))

(define-public (base-hyprland-window-workspace-binds workspaces)
  (append (list
	   (bind
	    (mods "SUPER")
	    (key "left")
	    (dispatcher "movefocus")
	    (params "l"))

	   (bind
	    (mods "SUPER")
	    (key "t")
	    (dispatcher "togglesplit")
	    (params ""))
	   
	   (bind
	    (mods "SUPER")
	    (key "q")
	    (dispatcher "killactive")
	    (params ""))
	   
	   (bind
	    (mods "SUPER SHIFT")
	    (key "q")
	    (dispatcher "exit")
	    (params ""))
	   
	   (bind
	    (mods "SUPER")
	    (key "f")
	    (dispatcher "fullscreen")
	    (params ""))
	   
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
	    (params "d")))

	  (make-binds workspaces)))

(define (make-binds n)
  (append-map
   (λ (i)
     (let ((s (number->string i)))
       (list (bind (mods "SUPER")        (key s) (dispatcher "workspace")       (params s))
             (bind (mods "SUPER SHIFT")  (key s) (dispatcher "movetoworkspace") (params s)))))
   (iota n 1)))
