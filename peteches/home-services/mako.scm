(define-module (peteches home-services mako)
  #:use-module (gnu packages wm)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (gnu services configuration)
  #:export (mako-config))

(define (home-mako-profile-service-type config)
  (list mako))


(define (serialize-integer field-name value)
  #~(string-append #$(symbol->string field-name) "=" #$(number->string value) "\n"))

(define (serialize-string field-name value)
  #~(string-append #$(symbol->string field-name) "=" #$value "\n"))

(define-configuration mako-config
  (max-history
   (integer 5)
   "Set maximum number of expired notifications to keep in the history buffer to n. If the buffer is full, newly expired notifications replace the oldest ones. If 0, history is disabled.")
  (sort
   (string "-time")
   "Sorts incoming notifications by time and/or priority in ascending(+) or descending(-) order.")
  (on-button-left
   (string "invoke-default-action")
   "Performs the action when the left pointer button is pressed.")
  (on-button-middle
   (string "none")
   "Performs the action when the middle pointer button is pressed.") 
  (on-button-right
   (string "dismiss")
   "Performs the action when the right pointer button is pressed.") 
  (on-touch
   (string "dismiss")
   "Performs the action when tapped via a touch device.") 
  (on-notify
   (string "none")
   "Performs the action when the notification is opened.") 
  (font
   (string "font")
   "Set font monospace font, as a Pango font description. For more information on Pango font descriptions, see: https://docs.gtk.org/Pango/type_func.FontDescription.from_string.html#description") 
  (background-color
   (string "#285577FF")
   "285577FF background color to color. See COLORS for more information.") 
  (text-color
   (string "#FFFFFF")
   "FFFFFFFF text color to color. See COLORS for more information.") 
  (width
   (integer 300)
   "Set width of notification popups.") 
  (height
   (integer 100)
   "Set maximum height of notification popups. Notifications whose text takes up less space are shrunk to fit.") 
  (outer-margin
   (integer 0)
   "Set outer-0 of each edge to the size specified by directional. See DIRECTIONAL VALUES for more information. This margin applies to the outside of the whole notification list.") 
  (margin
   (integer 10)
   "Set margin 10 each edge to the size specified by directional. See DIRECTIONAL VALUES for more information. This margin applies to each individual notification. Note that it applies in addition to outer-margin, meaning first and last notifications will use the sum of both margins.") 
  (padding
   (integer 5)
   "Set padding on each side to the size specified by directional. See DIRECTIONAL VALUES for more information.") 
  (border-size
   (integer 2)
   "Set popup border size to px pixels.") 
  (border-color
   (string "#4C7899FF")
   "4C7899FF popup border color to color. See COLORS for more information.") 
  (border-radius
   (integer 0)
   "Set popup corner radius on each side to the size specified by directional. See DIRECTIONAL VALUES for more information.") 
  (progress-color
   (string "#5588AAFF")
   "Set popup progress indicator color to color. See COLOR for more information. To draw the progress indicator on top of the background color, use the over attribute. To replace the background color, use the source attribute (this can be useful when the notification is semi-transparent).

Progress can be indicated in a notification by setting a hint, \"value\"
to an integer between 0 and 100 inclusive.")
  (icons
   (integer 1)
   "Show icons in notifications. Default: 1")
  (max-icon-size
   (integer 64)
   "Set maximum icon size to px pixels. Default: 64")
  (icon-path
   (string "")
   "Paths to search for icons when a notification specifies a name instead of a full path. Colon-delimited. This approximates the search algorithm used by the XDG Icon Theme Specification, but does not support any of the theme metadata. Therefore, if you want to search parent themes, you'll need to add them to the path manually.

The path should be the root of the icon theme, the categories and
resolutions will be searched for the most appropriate match.

/usr/share/icons/hicolor and /usr/share/pixmaps are always searched.")
  (icon-location
   (string "left")
   "Position of the icon relative to the displayed text. Valid options are left, right, top and bottom.")
  (icon-border-radius
   (integer 0)
   "Sets icon corner radius to px pixels.")
  (markup
   (integer 1)
   "If 1, enable Pango markup. If 0, disable Pango markup. If enabled, Pango markup will be interpreted in your format specifier and in the body of notifications.")
  (actions
   (integer 1)
   "Applications may request an action to be associated with activating a notification. Disabling this will cause mako to ignore these requests.")
  (history
   (integer 0)
   "If set, mako will save notifications that have reached their timeout into the history buffer instead of immediately deleting them. max-history determines the size of the history buffer.")
  (format
   (string "<b>%s</b>\\n%b++")
   "Set notification format string to format. See FORMAT SPECIFIERS for more information. To change this for grouped notifications, set it within a grouped criteria.")
  (text-alignment
   (string "left")
   "Set notification text alignment.")
  (default-timeout
    (integer 0)
    "Set the default timeout to timeout in milliseconds. To disable the timeout, set it to zero.")
  (ignore-timeout
   (integer 0)
   "If set, mako will ignore the expire timeout sent by notifications and use the one provided by default-timeout instead.")
  (group-by
   (string "")
   "comma-separated list of criteria fields that will be compared to other visible notifications to determine if this one should form a group with them. All listed criteria must be exactly equal for two notifications to group.")
  (max-visible
   (integer 5)
   "Set maximum number of visible notifications to n. Older notifications will be hidden. If -1, all notifications are visible.")
  (output
   (string "")
   "Show notifications on the specified output. If empty, notifications will appear on the focused output.

Requires the compositor to support the Wayland protocol
xdg-output-unstable-v1 version 2.")
  (layer
   (string "top")
   "Arrange mako at the specified layer, relative to normal windows. Supported values are background, bottom, top, and overlay. Using overlay will cause notifications to be displayed above fullscreen windows, though this may also occur at top depending on your compositor.")
  (anchor
   (string "top-right")
   "Show notifications at the specified position on the output. Supported values are top-right, top-center, top-left, bottom-right, bottom-center, bottom-left, center-right, center-left and center."))

(define (home-mako-xdg-files config)
  (list
   `("mako/config" ,(mixed-text-file "config"
				     (serialize-configuration config mako-config-fields)))))

(define-public home-mako-service-type
  (service-type (name 'mako-config)
		(description "Applies my personal mako config")
		(default-value (mako-config))
		(extensions
		 (list
		  (service-extension
		   home-xdg-configuration-files-service-type
		   home-mako-xdg-files)
		  (service-extension
		   home-profile-service-type
		   home-mako-profile-service-type)))))
