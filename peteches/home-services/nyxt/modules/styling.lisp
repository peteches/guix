;;; Use the built-in dark theme as a base but bump contrast on all surfaces.
(define-configuration browser
  ((theme (make-instance 'theme:theme
                         :dark-p t
                         ;; Page background / default surface
                         :background-color "#1c1c1e"
                         :on-background-color "#f5f5f7"   ; near-white text
                         ;; Primary interactive colour (buttons, active links)
                         :primary-color "#0a84ff"
                         :on-primary-color "#ffffff"
                         ;; Secondary surface — menus, panels
                         :secondary-color "#2c2c2e"
                         :on-secondary-color "#f5f5f7"
                         ;; Accent — highlights, focus rings
                         :accent-color "#ffd60a"
                         :on-accent-color "#1c1c1e"))))

;;; Prompt-buffer (the popup "menu") — override selection highlight and
;;; match highlighting for higher contrast.
(define-configuration nyxt/prompt-buffer:prompt-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '(;; Selected row: vivid blue background, white text
              ("#selection"
               :background-color "#0a50c0"
               :color            "#ffffff")
              ;; Fuzzy-match character highlights inside each row
              (".match"
               :color            "#ffd60a"
               :font-weight      "bold")
              ;; Unselected row text — ensure it reads on the dark panel
              (".result"
               :color            "#e0e0e5")))))))
