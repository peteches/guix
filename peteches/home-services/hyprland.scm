(define-module (peteches home-services hyprland)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages glib)          ; dbus (dbus-update-activation-environment)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (home-hyprland-service-type
	    home-hyprland-configuration
	    colour-management
            general-category
	    general-snap-category
	    decoration-category
	    decoration-blur-category
	    input-category
	    input-touchpad-category
	    hyprland-execs
	    monitor
	    reservation
            bind
	    submap
            home-hyprland-variable-configuration))

(define (home-hyprland-profile-service config)
       (list hyprland
	     hyprcursor
	     xdg-desktop-portal
	     xdg-desktop-portal-gtk
	     xdg-desktop-portal-hyprland))

(define-maybe integer)

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    ;; field? -> is-field
    (if (string-suffix? "?" str)
        (string-append "is-" (string-drop-right str 1))
        str)))

(define (serialize-string field-name value)
  #~(string-append "\t"  #$(uglify-field-name field-name) " = " #$value "\n"))

(define (serialize-integer field-name value)
  (serialize-string field-name (number->string value)))

(define (serialize-boolean field-name value)
  (serialize-string field-name (if value "true" "false")))

(define (serialize-hex-rgba field-name value)
  (serialize-string field-name value))

(define (serialize-real field-name value)
  (serialize-string field-name (number->string value)))

(define hex-rgba-regexp  "0x[0-9a-fA-F]{8}")
(define rgb-regexp  "rgb\\(([0-9a-fA-F]{6}|([0-9]{,3}, *){2}[0-9]{,3})\\)")
(define rgba-regexp "rgba\\(([0-9a-fA-F]{8}|(0-9]{,3}, *){3}[01]\\.[0-9]{,3})\\)")
(define colour-regexp (string-append "(" rgb-regexp "|" rgba-regexp "|" hex-rgba-regexp ")" ))
(define gradient-regexp (string-append
			 colour-regexp
			 "( *"
			 colour-regexp
			 "( *[0-9]{,3}deg)?)?"))			 

(define (hex-rgba? colour)
  (string-match (string-append "^" hex-rgba-regexp "$") colour))

(define (rgb? colour)
  (string-match (string-append "^" rgb-regexp "$") colour))

(define (rgba? colour)
  (string-match (string-append "^" rgba-regexp "$") colour))

(define (gradient? colour)
  (string-match (string-append "^" gradient-regexp "$") colour))

(define (colour? colour)
  (cond
   ((hex-rgba? colour) #t)
   ((rgb? colour) #t)
   ((rgba? colour) #t)
   ((string=? "" colour) #t)
   (#t #f)))


(define (serialize-gradient field-name gradient)
  (if (string=? "" gradient)
      #~(string-append "" "")
      (serialize-string field-name gradient)))

(define (serialize-colour field-name colour)
  (if (string=? "" colour)
      #~(string-append "" "")
      (serialize-string field-name colour)))

(define (vec2? v)
  (if (pair? v)
      (if (and (real? (car v))
	       (real? (cdr v)))
	  #t)
      #f))

(define (serialize-vec2 field-name vec)
  #~(string-append
     "\t" #$(uglify-field-name field-name) " = "
     #$(number->string (car vec))
     " "
     #$(number->string (car (cdr vec)))
     "\n"))

(define-configuration general-snap-category
  (enabled
   (boolean #f)
   "enable snapping for floating windows")
  (window_gap
   (integer 10)
   "minimum gap in pixels between windows before snapping")
  (monitor_gap
   (integer 10)
   "minimum gap in pixels between window and monitor edges before snapping")
  (border_overlap
   (boolean #f)
   "if true, windows snap such that only one border's worth of space is between them"))

(define (serialize-general-snap-category field-name config)
  #~(string-append "\n\tsnap {\n"
		   #$(serialize-configuration
		      config
		      general-snap-category-fields)
		   "\t}\n"))

(define-configuration general-category
  (border_size
   (integer 1)
   "size of the border around windows")
  (no_border_on_floating
   (boolean #f)
   "disable borders for floating windows")
  (gaps_in
   (integer 5)
   "gaps betewen windows, also supports css style gaps (top, right, bottom, left -> 5,10,15,20)")
  (gaps_out
   (integer 20)
   "gaps between windows and monitor edges, also supports css style gaps (top, right, bottom, left -> 5,10,15,20)")
  (gaps_workspaces
   (integer 0)
   "gaps between workspaces. Stacks with gaps_out.")
  (col.inactive_border
   (colour "0xff444444")
   "border colour for inactive windows")
  (col.active_border
   (gradient "0xffffffff")
   "border colour for the active window")
  (col.nogroup_border
   (gradient "0xffffaaff")
   "inactive border colour for window that cannot be added to a group (see `denywindowfromgroup` dispatcher)")
  (col.nogroup_border_active
   (gradient "0xffff00ff")
   "active border colour for window that cannot be added to a group")
  (layout
   (string "dwindle")
   "which layout to use [dwindle/master]")
  (no_focus_fallback
   (boolean #f)
   "if true, will not fall back to the next available window when moving focus in a direction where no window was found")
  (resize_on_border
   (boolean #f)
   "enables resizing windows by clicking and dragging on borders and gaps")
  (extend_border_grab_area
   (integer 15)
   "extends the area around the border where you can click and drag on, only used when `general:resize_on_border` is on.")
  (hover_icon_on_border
   (boolean #t)
   "show a cursor icon when hovering over borders, only used when `general:resize_on_border` is on")
  (allow_tearing
   (boolean #f)
   "master switch for allowing tearing to occur. See https://wiki.hyprland.org/Configuring/Tearing.")
  (resize_corner
   (integer 0)
   "force floating windows to use a specific corner when being resized (1-4 going clockwise from top left, 0 to disable)")
  (snap
   (general-snap-category (general-snap-category))
   "Configure window snapping options"))

(define (serialize-general-category field-name config)
  #~(string-append "general {\n"
                   #$(serialize-configuration
		      config
		      general-category-fields)
		   "\n}\n"))

(define-configuration decoration-blur-category
  (enabled
   (boolean #t)
   "enable kawase window background blur")
  (size
   (integer 8)
   "blur size (distance)")
  (passes
   (integer 1)
   "the amount of passes to perform")
  (ignore_opacity
   (boolean #t)
   "make the blur layer ignore the opacity of the window")
  (new_optimizations
   (boolean #t)
   "whether to enable further optimisations to the blur. Recommended to leave on, as it will massively improve performance")
  (xray
   (boolean #f)
   "if enabled, floating windows will ignore tiled windows in their blur. Only available if new_optimizations is true. Will reduce overhead on floating blur significantly")
  (noise
   (real 0.0117)
   "how much noise to apply")
  (contrast
   (real 0.8916)
   "contrast modulation for blur [0.0 - 2.0]")
  (brightness
   (real 0.8172)
   "brightness for modulation blur [0.0 - 2.0]")
  (vibrancy
   (real 0.1696)
   "Increase saturation of blurred colours. [0.0 - 1.0]")
  (vibrancy_darkness
   (real 0.0)
   "How strong the effect of `vibrancy` is on dark areas. [0.0 - 1.0]")
  (special
   (boolean #f)
   "whether to blur behind the special workspace (note: expensive)")
  (popups
   (boolean #f)
   "whether to blur popups (e.g. right-click menus)")
  (popups_ignorealpha
   (real 0.2)
   "works like `ignorealpha` is layer rules. If pixel opacity is below set value, will not blur [0.0 - 1.0]")
  (input_methods
   (boolean #f)
   "whether to blur input methods (e.g. fcitx5)")
  (input_methods_ignorealpha
   (real 0.2)
   "works like `ignorealpha` in layer rules. If pixel opacity is below set value, will not blur [0.0 - 1.0]"))

(define (serialize-decoration-blur-category field-name config)
  #~(string-append "\n\tblur {\n"
		   #$(serialize-configuration
		      config
		      decoration-blur-category-fields)
		   "\n\t}\n"))

(define-configuration decoration-shadow-category
  (enabled
   (boolean #t)
   "enable drop shadows on windows")
  (range
   (integer 4)
   "Show range (\"size\") in layout px")
  (render_power
   (integer 3)
   "in what power to render the falloff (more power the faster the falloff [1 - 4]")
  (sharp
   (boolean #f)
   "if enabled, will make the shadows sharp, akin to an infinite render power")
  (ignore_window
   (boolean #t)
   "if true, the shadow will not be rendered behind the window itself, only around it")
  (color
   (colour "0xee1a1a1a")
   "shadows colour. Alpha dictates shadows opacity")
  (color_inactive
   (colour "")
   "inactive shadow colour. (if not set, will fall back to `color`)")
  (offset
   (vec2 '(0.0 0.0))
   "shadow's rendering offset")
  (scale
   (real 1.0)
   "shadows scale. [0.0 - 1.0]"))
  
(define (serialize-decoration-shadow-category field-name config)
  #~(string-append "\n\tshadow {\n"
		   #$(serialize-configuration
		      config
		      decoration-shadow-category-fields)
		   "\n\t}\n"))

(define-configuration decoration-category
  (rounding
   (integer 0)
   "rounded corners' radius (in layout px)")
  (rounding_power
   (real 2.0)
   "adjusts the curve used for rounding corners, larger is smoother, 2.0 is a circle, 4.0 is a squircle. [2.0 - 10]")
  (active_opacity
   (real 1.0)
   "opacity of active windows. [0.0 - 1.0]")
  (inactive_opacity
   (real 1.0)
   "opacity of inactive windows. [0.0 - 1.0]")
  (fullscreen_opacity
   (real 1.0)
   "opacity of fullscreen windows")
  (dim_inactive
   (boolean #f)
   "enables dimming of inactive windows")
  (dim_strength
   (real 0.5)
   "how much inactive windows should be dimmed [0.0 - 1.0]")
  (dim_special
   (real 0.2)
   "how much to dim the rest of the screen by whe a special workspace is open. [0.0 - 1.0")
  (dim_around
   (real 0.4)
   "how much the `dimaround` window rule should dim by. [0.0 - 1.0]")
  (screen_shader
   (string "")
   "a path to a custom shader to be applied at the end of rendering. See `examples/screenShader.frag` for an example/")
  (blur
   (decoration-blur-category (decoration-blur-category))
   "Blur configuration")
  (shadow
   (decoration-shadow-category (decoration-shadow-category))
   "Shadow configurations"))

(define (serialize-decoration-category field-name config)
  #~(string-append "decoration {\n"
		   #$(serialize-configuration
		      config
		      decoration-category-fields)
		   "}\n"))


(define (serialize-animations-category field-name config)
  #~(string-append "animations {\n"
		   #$(serialize-configuration
		       config
		       animations-category-fields)
		   "}\n"))

(define-configuration animations-category
  (enabled
   (boolean #t)
   "enable animations")
  (first_launch_animation
   (boolean #t)
   "enable first launch animation."))

(define (serialize-input-touchpad-category field-name config)
  #~(string-append "\n\ttouchpad {\n"
		   #$(serialize-configuration
		      config
		      input-touchpad-category-fields)
		   "\t}\n"))

(define-configuration input-touchpad-category
  (disable_while_typing
   (boolean #t)
   "Disable the touchpad while typing.")
  (natural_scroll
   (boolean #t)
   "Inverts the scrolling direction. When enabled, scrolling moves content directly, rather than manipulating a scrollbar.")
  (scroll_factor
   (real 1.0)
   "Multiplier applied to the amount of scroll movement")
  (middle_button_emulation
   (boolean #f)
   "Sending LMB and RMB simultaneously will be interpreted as a middle click. This disables any touchpad area that would normally send a middle click based on location. https://wayland.freedesktop.org/libinput/doc/latest/middle-button-emulation.html")
  (tap_button_map
   (string "")
   "Sets the tap button mapping for touchpad emulation. Can be one of `lrm` (default) or `lmr` (left, middle, right buttons) [lrm/lmr]")
  (clickfinger_behavior
   (boolean #f)
   "Button presses with 1,2 or 3 fingers will be mapped to LMB, RMB and MMB respectively. This disables interpretation of clicks based on location of the touchpad. https://wayland.freedesktop.org/libinput/doc/latest/clickpad-softbuttons.html#clickfinger-behavior")
  (tap-to-click
   (boolean #t)
   "Tapping on the touchpad with 1,2 or 3 fingers will send LMB, RMB and MMB respectively.")
  (drag_lock
   (boolean #f)
   "When enabled, lifting the finger off for a short time while dragging will not drop the dragged item. https://wayland.freedesktop.org/libinput/doc/latest/tapping.html#tap-and-drag")
  (tap-and-drag
   (boolean #t)
   "Sets the tap and drag mode for the touchpad")
  (flip_x
   (boolean #f)
   "inverts the movement of the touchpad")
  (flip_y
   (boolean #f)
   "inverts the movement of the touchpad"))

(define (serialize-input-touchdevice-category field-name config)
  #~(string-append "\ttouchdevice {\n"
		   #$(serialize-configuration
		      config
		      input-touchdevice-category-fields)
		   "\t}\n"))

(define-configuration input-touchdevice-category
  (transform
   (integer -1)
   "Transform the input from touchdevices. The possible transformations are the same as those of the monitors. -1 means it's unset")
  (output
   (string "Auto")
   "The monitor to bind touch devices. The default is auto-detection. To stop auto-detection, use an empty string or the [[Empty]] value")
  (enabled
   (boolean #t)
   "Whether input is enabled for touch devices."))

(define (serialize-input-tablet-category field-name config)
  #~(string-append "\ttablet {\n"
		   #$(serialize-configuration
		      config
		      input-tablet-category-fields)
		   "\t}\n"))

(define-configuration input-tablet-category
  (transform
   (integer -1)
   "Transform the input from tablets. The possible transformations are the same as those of the monitors. -1 means it's unset")
   (output
   (string "Auto")
   "The monitor to bind tablets. The default is auto-detection. To stop auto-detection, use an empty string or the [[Empty]] value")
   (region_position
    (vec2 '(0 0))
    "Position of the mapped region in monitor layout relative to the top left corner of the bound monitor or all monitors")
   (absolute_region_position
    (boolean #f)
    "whether to treat the `region_position` as an absolute position in monitor layout. Only applies when `output` is empty.")
   (region_size
    (vec2 '(0 0))
    "size of hte mapped region. When this variable is set, tablet input will be mapped to the region. [0, 0] or invalid size means unset.")
   (relative_input
    (boolean #f)
    "Whether the input should be relative")
   (left_handed
    (boolean #f)
    "if enabled, the tablet will be rotated 180 degrees")
   (active_area_size
    (vec2 '(0 0))
    "size of tablet's active area in mm")
   (active_area_position
    (vec2 '(0 0))
    "position of the active area in mm"))

(define (serialize-input-category field-name config)
  #~(string-append "input {\n"
		   #$(serialize-configuration
		      config
		      input-category-fields)
		   "}\n"))

(define-configuration input-category
  (kb_model
   (string "")
   "Appropriate XKB keymap parameter")
  (kb_layout
   (string "us")
   "Appropriate XKB keymap parameter")
  (kb_variant
   (string "")
   "Appropriate XKB keymap parameter")
  (kb_options
   (string "")
   "Appropriate XKB keymap parameter")
  (kb_rules
   (string "")
   "Appropriate XKB keymap parameter")
  (kb_file
   (string "")
   "If you prefer, you can use a path to your custom .xkb file")
  (numlock_by_default
   (boolean #f)
   "Engage numlock by default")
  (resolve_binds_by_sym
   (boolean #f)
   "Determines how keybinds act when multiple layouts are used. If false, keybinds will always act as if the first specified layout is active. If true, keybinds are specified by symbols are activated when you type the respective symbol with the current layout.")
  (repeat_rate
   (integer 25)
   "The repeat rate for held-down keys, in repeats per second.")
  (repeat_delay
   (integer 600)
   "Delay before a held down key is repeated, in milliseconds")
  (sensitivity
   (real 0.0)
   "Sets the mouse input sensitivity. Value is clamped to the range -1.0 to 1.0. https://wayland.freedesktop.org/libinput/doc/latest/pointer-acceleration.html#pointer-acceleration")
  (accel_profile
   (string "")
   "Sets the cursor acceleration profile. Can be one of `adaptive` `flat`. Can also be `custom`, Leave empty to use `libinput`s default mode for your input device.")
  (force_no_accel
   (boolean #f)
   "Force no cursor acceleration. This bypasses most of your pointer settings to get as raw of a signal as possible. Enabling this is not recommended due to potential cursor desynchronisation.")
  (left_handed
   (boolean #f)
   "Switches RMB and LMB")
  (scroll_points
   (string "")
   "Sets the scroll acceleration profile, when `accel_profile` is set to custom. Has to be in the form `<step> <points>`. Leave empty to have a flat sroll curve")
  (scroll_method
   (string "")
   "Sets the scroll method. Can be one of `2fg` (2 fingers), `edge`, `on_button_down`, `no_scroll`. https://wayland.freedesktop.org/libinput/doc/latest/scrolling.html")
  (scroll_button
   (integer 0)
   "Sets the scroll buttom. Has to be an integer, cannot be a string. Check `wev` if you have any doubts regarding the ID. 0 means the default.")
  (scroll_button_lock
   (boolean #f)
   "If the scroll button lock is enabled, the button does not need to be held down. Pressing and releasing the button toggles the button lock, which logically holds the button down or releases it. While the button is logically held down, motion events are converted to scroll events.")
  (scroll_factor
   (real 1.0)
   "Multiplier added to scroll movement for external mice. Note that there is a seperate setting for `touchpad_scroll_factor")
  (natural_scroll
   (boolean #f)
   "Inverts scrolling direction. When enabled, scrolling moves content directly rather than manipulating the scrollbar.")
  (follow_mouse
   (integer 1)
   "Specify if and how cursor movement should affect window focus. [0/1/2/3]")
  (follow_mouse_threshold
   (real 0.0)
   "The smallest distance in logical pixels the mouse needs to travel for the window under it to get focused. Only works with `follow_mouse=1`]")
  (focus_on_close
   (integer 0)
   "Controls the window focus behaviour when a window is closed. When set to 0, focus will shift to the next window candidate. When set to 1, focus will shift to the window under the cursor. [0/1]")
  (mouse_refocus
   (boolean #t)
   "If disabled, mouse focus won't switch to the hovered window unless the mouse crosses a window boundary when `follow_mouse=1`")
  (float_switch_override_focus
   (integer 1)
   "If enabled (1 or 2), focus will change to the window under the cursor when changing from tiled-to-floating and vice versa. If 2, focus will also follow mouse on float-to-float switches.")
  (special_fallthrough
   (boolean #t)
   "if enabled, having only floating windows in the special workspace will not block focusing windows in the regular workspace.")
  (off_window_axis_events
   (integer 1)
   "Handles axis events around (gaps/border for tiled, dragarea/border for floated) a focused window. 0 ignores axis events, 1 sends out-of-bound coordinates, 2 fakes pointer coordinates to the closest point inside the window, 3 warps the cursor to the closest point inside the window.")
  (emulate_discrete_scroll
   (integer 1)
   "Emulates discrete scrolling from high resolution scrolling events. 0 disables it, 1 enables handling of non-standard events only and 2 force enables all scroll wheel events to be handled.")
  (touchpad
   (input-touchpad-category (input-touchpad-category))
   "Touchpad settings")
  (touchdevice
   (input-touchdevice-category (input-touchdevice-category))
   "Touch device settings")
  (tablet
   (input-tablet-category (input-tablet-category))
   "Tablet configurations"))

(define (serialize-gestures-category field-name config)
  #~(string-append "gestures {\n"
		   #$(serialize-configuration
		      config
		      gestures-category-fields)
		   "}\n"))

(define-configuration gestures-category
  (workspace_swipe
   (boolean #f)
   "enable workspace swipe gesture on touchpad")
  (workspace_swipe_fingers
   (integer 3)
   "how many fingers for the touchpad gesture")
  (workspace_swipe_min_fingers
   (boolean #f)
   "if enabled, `workspace_swipe_fingers` is considered the minimum number of fingers to swipe")
  (workspace_swipe_distance
   (integer 300)
   "in px, the distance of the touchpd gesture")
  (workspace_swipe_touch
   (boolean #f)
   "enable workspace swiping from the edge of a touchscreen")
  (workspace_swipe_invert
   (boolean #t)
   "invert the direction (touchpad only)")
  (workspace_swipe_touch_invert
   (boolean #t)
   "invert the direction (touchscreen only)")
  (workspace_swipe_min_speed_to_force
   (integer 30)
   "minimum speed in px per timepoint to force the change ignoring `cancel_ratio`. Setting to 0 will disable this mechanic")
  (workspace_swipe_cancel_ratio
   (real 0.5)
   "how much the swipe has to proceed in order to commence it. (0.7 -> if > 0.7*distance, switch if less, revert) [0/0 - 1.0]")
  (workspace_swipe_create_new
   (boolean #t)
   "whether a swipe right on the last workspace should create a new one.")
  (workspace_swipe_direction_lock
   (boolean #t)
   "if enabled, switching direction will be locked when you swipe past the `direction_lock_threshold` (touchpad only)")
  (workspace_swipe_direction_lock_threshold
   (integer 10)
   "in px, the distance to swipe before direction lock activates (touchpad only)")
  (workspace_swipe_forever
   (boolean #f)
   "if enabled, swiping will not clamp at the neighbouring workspaces but will continue to further ones.")
  (workspace_swipe_use_r
   (boolean #f)
   "if enabled, swiping will use the `r` prefixx instead of the `m` prefix for finding workspaces"))

(define (serialize-group-category field-name config)
  #~(string-append "group {\n"
		   #$(serialize-configuration
		      config
		      group-category-fields)
		   "}\n"))

(define-configuration group-category
  (auto_group
   (boolean #t)
   "whether new windows will be automatically grouped into the focused unlocked group. Note: if you want to disable auto_group only for specific windows, use the `group_barred` window rule instead.")
  (insert_after_current
   (boolean #t)
   "whether new windows in a group spawn after current or at group tail")
  (focus_removed_window
   (boolean #t)
   "whether Hyprland should focus on the window that has just been moved out of the group")
  (drag_into_group
   (integer 1)
   "whether dragging a window into an unlocked group will merge them. Options: 0(disabled), 1(enabled), 2(only when dragging into the groupbar)")
  (merge_groups_on_drag
   (boolean #t)
   "whether window groups can be dragged into other groups")
  (merge_floated_into_tiled_on_groupbar
   (boolean #f)
   "whether dragging a floating window into a tiled window groupbar will merge them")
  (group_on_movetoworkspace
   (boolean #f)
   "whether using movetoworkspace[silent] will merge the window into the workspace's solitary unlocked group")
  (col.border_active
   (gradient "0x66ffff00")
   "active group border")
  (col.border_inactive
   (gradient "0x66777700")
   "inactive (out of focus) group border colour")
  (col.border_locked_active
   (gradient "0x66ff5500")
   "active locked group border colour")
  (col.border_locked_inactive
   (gradient "0x66775500")
   "inactive locked group border colour"))

(define-configuration home-hyprland-variable-configuration
  (general
   (general-category (general-category))
   "General section of hyprland config")
  (decoration
   (decoration-category (decoration-category))
   "Window decoration config")
  (animations
   (animations-category (animations-category))
   "Animations config")
  (input
   (input-category (input-category))
   "Input Settings")
  (gestures
   (gestures-category (gestures-category))
   "Gesture settings")
  (group
   (group-category (group-category))
   "Window grouping config"))

(define (serialize-hyprland-execs cmds)
  #~(string-append "## Exec'ing cmds\n"
		   #$(serialize-configuration
		      cmds
		      hyprland-execs-fields)))

(define (exec-cmds? cmd)
  (every string? cmd))

(define (serialize-exec-cmds field-name cmds)
  #~(string-append #$@(map (lambda (x)
			     (string-append
			      (symbol->string field-name) " = " x "\n"))
			   cmds)))

(define-configuration hyprland-execs
  (exec-once
   (exec-cmds '())
   "list of cmds that only execute on launch (supports rules)")
  (execr-once
   (exec-cmds '())
   "list of cmds that only execute on launch")
  (exec
   (exec-cmds '())
   "list of cmds that execute on each reload (supports rules)")
  (execr
   (exec-cmds '())
   "list of cmds that execute on each reload")
  (exec-shutdown
   (exec-cmds '())
   "list of cmds that execute only on shutdown"))

(define (serialize-home-hyprland-variable-configuration config)
  #~(string-append "# Hyprland variables\n"
		   #$(serialize-configuration
		      config home-hyprland-variable-configuration-fields)))

(define (serialize-hyprland-sources sources)
  #~(string-append "# Additional source files\n"
		   #$@(map (lambda (x)
			     (string-append
			      "source = " x "\n"))
			   sources)))

(define (hyprland-sources? sources)
  (every string? sources))

(define (layer-rules? rules)
  (every string? rules))

(define (serialize-layer-rules rules)
  #~(string-append "# Layer rules\n"
		   #$@(map (lambda (x)
			     (string-append
			      "layerrule = " x "\n"))
			   rules)))

(define (env-vars? vars)
  (every (lambda (x) (and (pair? x) (string? (car x)) (string? (cdr x)))) vars))

(define (serialize-env-vars vars)
  #~(string-append "# Env variables\n"
		   #$@(map (lambda (x)
			     (string-append
			      "env = " (car x) ", " (cdr x) "\n"))
			   vars)))

(define (dbus-env-vars? vars)
  (every (lambda (x) (pair? x) (every string? x)) vars))

(define (serialize-dbus-env-vars vars)
  #~(string-append "# D-Bus Env variables\n"
		   #$@(map (lambda (x)
			     (string-append
			      "envd = " (car x) ", " (cdr x) "\n"))
			   vars)))

(define resolution-regexp "^\\([0-9]+x[0-9]+\\(@[0-9]+|preferred|highres|highrr\\)\\)$")

(define (resolution? res)
  (string-match resolution-regexp res))

(define (serialize-resolution field-name res)    res)

(define (serialize-position field-name pos) pos)

(define position-regexp "(auto((-center)?-(left|right|up|down))?|[0-9]+x[0-9]+)$")

(define (position? pos)
  (string-match position-regexp pos))

(define (serialize-reservation field-name reservation)
  #~(let ((reservation (string-append ", addreserved"
				      #$(serialize-configuration
					 reservation
					 reservation-fields))))
      (if (string= reservation ", addreserved, 0, 0, 0, 0")
	  ""
	  reservation)))

(define (res-serialize-integer field-name val)
  (string-append ", " (number->string val)))

(define-configuration reservation
  (top
   (integer 0)
   "reservation at the top edge of the monitor"
   (serializer res-serialize-integer))
  (bottom
   (integer 0)
   "reservation at the bottom edge of the monitor"
   (serializer res-serialize-integer))
  (left
   (integer 0)
   "reservation at the left edge of the monitor"
   (serializer res-serialize-integer))
  (right
   (integer 0)
   "reservation at the right edge of the monitor"
   (serializer res-serialize-integer)))


(define (stringify thing)
  (cond
   ((string? thing) thing)
   ((number? thing) (number->string thing))
   ((symbol? thing) (symbol->string thing))
   (else thing)))

(define (serialize-value-inline field-name value)
  #~(string-append
     (if (not (equal? #$(stringify field-name) "name")) ", " "")
     #$(stringify value)))

(define (serialize-kv-inline field-name value)
  (let ((k (stringify field-name)) (v (stringify value)))
      #~(string-append ", " #$k ", " #$v)))

(define (serialize-kv-if-not-default field-name value default)
  (if (not (equal? value default))
      (serialize-kv-inline field-name value)
      ""))

(define (serialize-mirror field-name val)
  #~(if (not (string= #$val ""))
	(string-append
	 ", "
	 #$(uglify-field-name field-name)
	 ", "
	 #$val)
	""))

(define (serialize-10-bit field-name val)
  #~(if #$val
	", bitdepth, 10"
	""))

(define (serialize-sdr field-name value)
  (serialize-kv-if-not-default field-name value 1.0))

(define (serialize-colour-management field-name val)
  (serialize-configuration
   val
   colour-management-fields))

(define (serialize-cm-preset field-name value)
  (serialize-kv-if-not-default "cm" value "srgb"))

(define-configuration colour-management
  (preset
   (string "srgb")
   "Colour management preset one of auto, srgb (default), wide, edid, hdr, hdredid"
   (serializer serialize-cm-preset))
  (sdrbrightness
   (real 1.0)
   "If preset is `hdr` you can use this to control SDR brightness"
   (serializer serialize-sdr))
  (sdrsaturation
   (real 1.0)
   "If preset is `hdr you can use this to control SDR saturation"
   (serializer serialize-sdr)))

(define (serialize-modeline field-name value)
  (serialize-kv-if-not-default field-name value ""))

(define (serialize-vrr field-name value)
  (serialize-kv-if-not-default field-name value 0))

(define (serialize-mirror field-name value)
  (serialize-kv-if-not-default field-name value ""))

(define (serialize-bitdepth field-name value)
  (serialize-kv-if-not-default field-name value 0))

(define-configuration monitor
  (name
   (string "")
   "Name of the monitor e.g. DP-1 (run `hyprctl monitors all` for list)"
   (serializer serialize-value-inline))
  (resolution
   (resolution "preferred")
   "Resolution of the monitor, in format 1920x1080@144 refresh rate is optional, alternatively use one of preferred (default), highres highrr for highest resolution or refresh rate supported."
   (serializer serialize-value-inline))
  (position
   (position "auto")
   "Position of the monitor one of auto, auto-right/left/up/down or absolute x,y co-ordinate (negative values allowed"
   (serializer serialize-value-inline))
  (scale
   (real 1)
   "Scale the monitor resolution, useful for high dpi screens."
   (serializer serialize-value-inline))
  (modeline
   (string "")
   "Custom modeline, but honestly your on your own here! replaces resolution, position and scale."
   (serializer serialize-modeline))
  (addreserved
   (reservation (reservation))
   "Reserved area will reserve a number of pixels at the top, bottom, left and right of the monitor."
   (serializer serialize-reservation))
  (enabled
   (boolean #t)
   "if false all other options are ignored and the monitor is marked as disabled"
   (serializer (lambda (x y) "")))
  (bitdepth
   (integer 0)
   "Flag a monitor with 10 bit support"
   (serializer serialize-bitdepth))
  (mirror
   (string "")
   "Name of another monitor to mirror"
   (serializer serialize-mirror))
  (vrr
   (integer 0)
   "Controls the VRR (adaptive sync) of the monitor 0 - off, 1 - on, 2 - fullscreen only"
   (serializer serialize-vrr))
  (cm
   (colour-management (colour-management))
   "Configure colour management"
   (serializer serialize-colour-management))
  (transform
   (integer 0)
   "Monitor rotation configuration: 0 - normal (default), 1-> 90 deg 2-> 180 deg 3-> 270deg 4-> flipped 5-> flipped + 90deg 6-> flipped + 180deg 7-> flipped + 270deg"
   (serializer serialize-kv-inline)))

(define (serialize-monitor monitor)
  (cond
   ((not  (monitor-enabled monitor)) #~(string-append (monitor-name) ", disabled'n"))
   ((not (equal? (monitor-modeline monitor) "")) (serialize-kv-inline "modeline" (monitor-modeline monitor)))
   (else #~(string-append "monitor = "
 			  #$(serialize-configuration
			     monitor
			     monitor-fields)
			  "\n"))))

(define (list-of-monitors? monitors)
  (every monitor? monitors))

(define (serialize-list-of-monitors monitors)
  #~(string-append "# Monitor configurations\n"
		   #$@(map serialize-monitor monitors)))

(define (serialize-bind-value field-name value)
  (if (equal? field-name 'params)
      value
      (string-append value ", ")))

(define (serialize-bind-flag field-name value)
  #~(string-append #$value " = "))

(define (serialize-bind value)
  #~(string-append "bind"
		   #$(serialize-configuration
		      value
		      bind-fields)
		   "\n"))

(define-configuration bind
  (flags
   (string "")
   "Optional flags to apply to the bind."
   (serializer serialize-bind-flag))
  (mods
   string
   "Modkeys associated with the bind"
   (serializer serialize-bind-value))
  (key
   string
   "Key associated with the keybind"
   (serializer serialize-bind-value))
  (dispatcher
   string
   "Which Hyprland dispatcher to use with the bind"
   (serializer serialize-bind-value))
  (params
   (string)
   "Parameters to pass to the dispatcher, written as single string"
   (serializer serialize-bind-value)))

(define (list-of-binds? binds)
  (every bind? binds))

(define (serialize-list-of-binds field-name binds)
  #~(string-append "# Key bind configurations\n"
		   #$@(map serialize-bind binds)))

(define (serialize-submap  value)
  #~(string-append
     #$(serialize-configuration
	value
	submap-fields)
     "submap = reset\n"))

(define (serialize-submap-name field-name value)
  #~(string-append
     "submap = " #$value "\n"))

(define-configuration submap
  (name
   string
   "Name of the submap"
   (serializer serialize-submap-name))
  (binds
   (list-of-binds '())
   "All binds in the submap"))

(define (list-of-submaps? submaps)
  (every submap? submaps))

(define (serialize-list-of-submaps submaps)
  #~(string-append "# Submap configurations\n"
		   #$@(map serialize-submap submaps)))



(define-configuration home-hyprland-configuration
  (variables
   (home-hyprland-variable-configuration (home-hyprland-variable-configuration))
   "All hyprland variables: https://wiki.hyprland.org/Configuring/Variables")
  (command-execution
   (hyprland-execs (hyprland-execs))
   "All exec* commands")
  (layer-rules
   (layer-rules '())
   "List of layer rules")
  (dbus-env-vars
   (dbus-env-vars '())
   "Environment variables to set in Hyprland and exported to D-Bus (systemd only), should be in format \"VARNAME,value\"")
  (env-vars
   (env-vars '())
   "Environment variables to set in Hyprland, should be in format \"VARNAME,value\"")
  (monitors
   (list-of-monitors (list (monitor)))
   "List of monitor configurations") 
  (binds
   (list-of-binds '())
   "All hyprland bindings")
  (submaps
   (list-of-submaps '())
   "All submaps for keybinds, remember to add a binding in `binds` to activate each one, and an exit bind within each submap.")
  (additional_source_files
   (hyprland-sources '())
   "List of additional config files to source"))

(define (home-hyprland-files-service config)
  (list
   `("xdg-desktop-portal/portals.conf"
     ,(plain-file "portals.conf"
		  "[preferred]
# GTK handles most generic portals (FileChooser/OpenURI/Settings).
# Hyprland handles screenshot/screencast.
default=gtk
org.freedesktop.impl.portal.Screenshot=hyprland
org.freedesktop.impl.portal.Screencast=hyprland

[xdg-desktop-portal]
version=1
"))
   `("hypr/hyprland.conf" ,(mixed-text-file "hyprland.conf"
					    (string-append
					     "source = ~/.config/hypr/xdg-desktop-portals.conf\n"
					     "source = ~/.config/hypr/submaps.conf\n"
					     "source = ~/.config/hypr/binds.conf\n"
					     "source = ~/.config/hypr/monitors.conf\n"
					     "source = ~/.config/hypr/environment.conf\n"
					     "source = ~/.config/hypr/dbus-environment.conf\n"
					     "source = ~/.config/hypr/layerrules.conf\n"
					     "source = ~/.config/hypr/sources.conf\n"
					     "source = ~/.config/hypr/exec.conf\n"
					     "source = ~/.config/hypr/variables.conf\n")))
   `("hypr/xdg-desktop-portals.conf"  ,(let ((portal       #~(string-append #$xdg-desktop-portal "/libexec/xdg-desktop-portal"))
					     (gtk-backend  #~(string-append #$xdg-desktop-portal-gtk "/libexec/xdg-desktop-portal-gtk"))
					     (hypr-backend #~(string-append #$xdg-desktop-portal-hyprland "/libexec/xdg-desktop-portal-hyprland"))
					     (dbus-update-bin  #~(string-append #$dbus "/bin/dbus-update-activation-environment"))
					     (sh-bin       #~(string-append #$bash "/bin/sh"))
					     (pkill-bin    #~(string-append #$procps "/bin/pkill"))
					     (sleep-bin    #~(string-append #$coreutils "/bin/sleep")))
					 (mixed-text-file
					  "xdg-desktop-portals.conf"
					  "# --- XDG portal startup (Guix absolute paths) ---\n"
					  "exec-once = " dbus-update-bin " DISPLAY WAYLAND_DISPLAY HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP XDG_SESSION_DESKTOP XDG_SESSION_TYPE\n"
					  "exec-once = " sh-bin " -c '" pkill-bin " -f xdg-desktop-portal-hyprland; " pkill-bin " -f xdg-desktop-portal-gtk; " sleep-bin " 0.5'\n"
					  "exec-once = " hypr-backend " &\n"
					  "exec-once = " gtk-backend " &\n")))
   `("hypr/submaps.conf" ,(mixed-text-file "submaps.conf"
					   (serialize-list-of-submaps
					    (home-hyprland-configuration-submaps config))))
   `("hypr/binds.conf" ,(mixed-text-file "binds.conf"
					 (serialize-list-of-binds "binds"
								  (home-hyprland-configuration-binds config))))
   `("hypr/monitors.conf" ,(mixed-text-file "monitors.conf"
					    (serialize-list-of-monitors
					     (home-hyprland-configuration-monitors config))))
   `("hypr/environment.conf" ,(mixed-text-file "environment.conf"
					       (serialize-env-vars
						(home-hyprland-configuration-env-vars config))))
   `("hypr/dbus-environment.conf" ,(mixed-text-file "dbus-environment.conf"
						    (serialize-dbus-env-vars
						     (home-hyprland-configuration-dbus-env-vars config))))
   `("hypr/layerrules.conf" ,(mixed-text-file "layerrules.conf"
					      (serialize-layer-rules
					       (home-hyprland-configuration-layer-rules config))))
   `("hypr/sources.conf" ,(mixed-text-file "sources.conf"
					   (serialize-hyprland-sources
					    (home-hyprland-configuration-additional_source_files config))))
   `("hypr/exec.conf" ,(mixed-text-file "exec.conf"
					(serialize-hyprland-execs
					 (home-hyprland-configuration-command-execution config))))
   `("hypr/variables.conf" ,(mixed-text-file "variables.conf"
					     (serialize-home-hyprland-variable-configuration
					      (home-hyprland-configuration-variables config))))))

(define (home-hyprland-activation-service-type config)
  #~(system* #$(file-append hyprland "/bin/hyprctl") "reload"))

(define (home-hyprland-environment-variables-service-type config)
  `(("XDG_SESSION_TYPE" . "wayland")
    ("XDG_CURRENT_DESKTOP" . "Hyprland")
    ("XDG_SESSION_DESKTOP" . "Hyprland")
    ("MOZ_ENABLE_WAYLAND" . "1")
    ("QT_QPA_PLATFORM" . "wayland")
    ("GTK_USE_PORTAL" . "1")
    ("NIXOS_OZONE_WL" . "1")))

(define home-hyprland-service-type
  (service-type (name 'home-hyprland-config)
		(extensions
		 (list
		  (service-extension
		   home-activation-service-type
		   home-hyprland-activation-service-type)
		  (service-extension
		   home-environment-variables-service-type
		   home-hyprland-environment-variables-service-type)
		  (service-extension
		   home-profile-service-type
		   home-hyprland-profile-service)
		  (service-extension
		   home-xdg-configuration-files-service-type
		   home-hyprland-files-service)))
		(default-value (home-hyprland-configuration))
		(description "Applies my personal Hyprland base configuration")))
