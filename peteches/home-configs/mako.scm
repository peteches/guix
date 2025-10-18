(define-module (peteches home-configs mako)
  #:use-module (peteches home-services mako))

(define-public base-mako-config
  (mako-config
   (max-history 10)
   (sort "-time")
   (on-button-left "invoke-default-action")
   (on-button-middle "none")
   (on-button-right "dismiss")
   (on-touch "dismiss")
   (on-notify "exec canberra-gtk-play -i message-new-instant")
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
