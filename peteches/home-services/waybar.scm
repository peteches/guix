(define-module (peteches home-services waybar)
  #:use-module (gnu packages wm)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:export (waybar-configuration
	    waybar-config
	    waybar-style))

(define (waybar-profile config)
  (list waybar))

(define (serialize-string field-name config)
  (scm->json-string '((field-name .  config))))

(define (serialize-number field-name config)
  (scm->json-string '((field-name . config))))

(define (serialize-boolean field-name config)
  (scm->json-string '((field-name . config))))

(define (serialize-vector-of-strings field-name lst)
  (scm->json-string (list->vector lst)))

(define (serialize-list field-name lst)
  (scm->json-string lst))

(define (vector-of-strings? lst)
  (and
   (vector? lst)
   (every string? (vector->list lst))))

(define-configuration waybar-config
  (layer
   (string "bottom")
   "decide if the bar is displayed in front  (top) of windows or behind (bottom) them")
  (output
   (string "")
   "Specifies on which screen this bar will be displayed")
  (position
   (string "top")
   "Bar can be `top`, `bottom`, `left`, `right`")
  (height
   (number -1)
   "Height to be used by the bar if possible, leave -1 for a dynamic value (default)")
  (width
   (number -1)
   "Width to be used by the bar, if possible, leave -1 for a dynamic value.")
  (margin
   (string "")
   "Margins value using css format without units")
  (margin-top
   (number 10)
   "Margins value without units")
  (margin-bottom
   (number 10)
   "Margins value without units")
  (margin-left
   (number 10)
   "Margins value without units")
  (margin-right
   (number 10)
   "Margins value without units")
  (spacing
   (number 4)
   "Size of gaps in between the different modules")
  (name
   (string "")
   "Optional name added as a CSS class, fir styling multiple waybars")
  (mode
      (string "")
    "Selects one of the preconfigured display modes. This is an equivalent of the `sway-bar(5)` mode command and supporrts the same values: `dock`, `hide`, `invisible`, `overlay`. Note: `hide` and `invisible` modes may not be as useful without Sway IPC.")
  (start_hidden
   (boolean #f)
   "Option to start the bar hidden")
  (modifier-reset
   (string "press")
   "Defines the timing of modifier key to reset the visibility. To reset the visibility of the bar with the press of the modifier key use `press`. Use `release` to reset the visibility uppon the release of the modifier key and only if no other action happened while the key wsa pressed. this prevents hiding the bar when the modifier is used to switch a workspace, change binding mode or start a keybinding.")
  (exclusive
   (boolean #t)
   "Option to request an exclusive zone from the compositor. Disable this to allow drawing application windows underneath or on top of the bar. Disabled by default for `overlay` layer.")
  (fixed-center
   (boolean #t)
   "Prefer fixed centre position for the `modules-center` block. The centre block will stay in the middle of the bar whenever possible. It can still be pushed around if other blocks need more space. When false the centre block is centred in the space between the left and right block.")
  (passthrough
   (boolean #f)
   "Option to pass any pointer events to the window under the bar. Intended to be used with either `top` or `overlay` layers and without exclusive zone. Enabled by default for `overlay` layer.")
  (ipc
   (boolean #f)
   "Option to to subscribe to the Sway IPC bar configuration and visibility events and control waybar with swaymsg bar commands. Requires `bar_id` value from sway configuration to be either passed with the `-b` commandline argument or specified with the `id` option.")
  (id
   (string "")
   "`bar_id` for the Sway IPC. Use this if you need to override the value passed with the `-b bar_id` commandline arguments for the specific bar instance.")
  (included-files
   (vector-of-strings #())
   "Paths to additional configuration files. Each file can contain a single object with any of the bar configuration options. In case of duplicate options, the first defined value takes precedence, i.e. including file -> first included file -> etc. Nested includes are permitted, but make sure to avoid circular imports. For a multi-bar config, the `include` directive affects only the current bar configuration object.")
  (reload_style_on_change
   (boolean #f)
   "Option to enable reloading the css style if a modification is detected on the style sheet file or any imported css files.")
  (modules-left
   (vector-of-strings #())
   "Modules that will be displayed on the left")
  (modules-center
   (vector-of-strings #())
   "Modules that will be displayed in the centre")
  (modules-right
   (vector-of-strings #())
   "Modules that will be displayed on the right")
  (modules-config
   (list '())
   "Module list is a flexible list to allow arbitrary config of modules. It is json encoded with guile-json"))

(define (serialize-waybar-config config)
  (scm->json-string
   (let ((h (waybar-config-height config))
	 (w (waybar-config-width config))
	 (module-config (waybar-config-modules-config config))
	 (base (list
		(cons "layer" (waybar-config-layer config))
		(cons "output" (waybar-config-output config))
		(cons "position" (waybar-config-position config))
		(cons "margin" (waybar-config-margin config))
		(cons "margin-top" (waybar-config-margin-top config))
		(cons "margin-bottom" (waybar-config-margin-bottom config))
		(cons "spacing" (waybar-config-spacing config))
		(cons "name" (waybar-config-name config))
		(cons "mode" (waybar-config-mode config))
		(cons "start_hidden" (waybar-config-start_hidden config))
		(cons "modifier-reset" (waybar-config-modifier-reset config))
		(cons "exclusive" (waybar-config-exclusive config))
		(cons "fixed-center" (waybar-config-fixed-center config))
		(cons "passthrough" (waybar-config-passthrough config))
		(cons "ipc" (waybar-config-ipc config))
		(cons "id" (waybar-config-id config))
		(cons "include" (waybar-config-included-files config))
		(cons "reload_style_on_change" (waybar-config-reload_style_on_change config))
		(cons "margin-left" (waybar-config-margin-left config))
		(cons "margin-right" (waybar-config-margin-right config))		
		(cons "modules-left" (waybar-config-modules-left config))
		(cons "modules-center" (waybar-config-modules-center config))
		(cons "modules-right" (waybar-config-modules-right config)))))
     (if (< 0 h)
	 (append! base (list (cons "height" (waybar-config-height config)))))
     (if (< 0 w)
	 (append! base (list (cons "width" (waybar-config-width config)))))
     (if (< 0 (length module-config))
	 (append! base (waybar-config-modules-config config)))
     base)))


(define-configuration waybar-style)

(define (serialize-waybar-style config)
  #~(string-append "" ""))

(define-configuration waybar-configuration
  (config
   (waybar-config (waybar-config))
   "Waybar configuration")
  (style
      (waybar-style (waybar-style))
    "Waybar Styles"))

(define (waybar-files-service config)
  (list
   `("waybar/config.jsonc" ,(mixed-text-file "config.jsonc"
					     (serialize-waybar-config
					      (waybar-configuration-config config))))
   `("waybar/styles.css" ,(mixed-text-file "style.css"
					   (serialize-waybar-style
					    (waybar-configuration-style config))))))

(define-public waybar-service-type
  (service-type (name 'waybar-config)
		(extensions
		 (list
		  (service-extension
		   home-profile-service-type
		   waybar-profile)
		  (service-extension
		   home-xdg-configuration-files-service-type
		   waybar-files-service)))
		(default-value (waybar-configuration))
		(description "Applies my personal Waybar configuration")))
