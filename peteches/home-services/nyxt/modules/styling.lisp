;;; Theme colours — defined once here and reused in both blocks below.
;;;
;;;   background      #1c1c1e   on-background  #f5f5f7
;;;   primary         #0a84ff   on-primary     #ffffff
;;;   secondary       #2c2c2e   on-secondary   #f5f5f7
;;;   action          #0a84ff   (= primary, for interactive/focused elements)
;;;   highlight       #ffd60a   (gold accent, for search marks)

;;; Browser-wide theme — all surfaces derived from the palette above.
(define-configuration browser
  ((theme (make-instance 'theme:theme
                         :background-color "#1c1c1e"
                         :on-background-color "#f5f5f7"
                         :primary-color "#0a84ff"
                         :on-primary-color "#ffffff"
                         :secondary-color "#2c2c2e"
                         :on-secondary-color "#f5f5f7"
                         :action-color "#0a84ff"
                         :highlight-color "#ffd60a"))))
