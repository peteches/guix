(define-module (peteches home-configs mako)
  #:use-module (gnu services)
  #:use-module (peteches home services mako)
  #:export (base-mako-config
            base-mako-service))

(define-public base-mako-config
  (mako-config
   (max-history 10)
   (sort "-time")
   (on-button-left "invoke-default-action")
   (on-button-middle "exec makoctl menu -n \"$id\" -- wofi -d -p 'Notification action: '")
   (on-button-right "dismiss")
   (on-touch "dismiss")
   (on-notify "exec canberra-gtk-play -i message-new-instant")
   (font "JetBrains Mono 11")
   ;; Fallback colors. Matugen writes the included color fragment at
   ;; ~/.cache/matugen/mako/colors.conf during activation and wallpaper changes.
   (background-color "#1e1e1eFF")
   (text-color "#FFFFFFFF")
   (border-color "#646464FF")
   (progress-color "over #2FAFFFEE")
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
   ;; Keep the service module external and unchanged.  The current
   ;; home-mako-service-type serializes only known fields, so append the
   ;; Mako-native include and mode criteria through the final string field.
   (anchor (string-append
            "top-right\n"
            "include=~/.cache/matugen/mako/colors.conf\n"
            "\n"
            "[mode=do-not-disturb]\n"
            "invisible=1"))))

(define-public base-mako-service
  (service home-mako-service-type base-mako-config))
