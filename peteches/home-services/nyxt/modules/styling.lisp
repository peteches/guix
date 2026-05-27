;;; Theme colours — defined once here and reused in both blocks below.
;;;
;;;   background      #1c1c1e   on-background  #f5f5f7
;;;   primary         #0a84ff   on-primary     #ffffff
;;;   secondary       #2c2c2e   on-secondary   #f5f5f7
;;;   accent          #ffd60a   on-accent      #1c1c1e

;;; Browser-wide theme — all surfaces derived from the palette above.
(define-configuration browser
  ((theme (make-instance 'theme:theme
                         :dark-p t
                         :background-color "#1c1c1e"
                         :on-background-color "#f5f5f7"
                         :primary-color "#0a84ff"
                         :on-primary-color "#ffffff"
                         :secondary-color "#2c2c2e"
                         :on-secondary-color "#f5f5f7"
                         :accent-color "#ffd60a"
                         :on-accent-color "#1c1c1e"))))

;;; Prompt-buffer — all colours taken from the palette above so the
;;; popup feels like part of the same browser, not a foreign widget.
(define-configuration prompt-buffer
  ((style (str:concat
           %slot-default%
           "/* Input / prompt strip */
#prompt-area  { background-color: #2c2c2e; color: #f5f5f7; }
#input        { background-color: #2c2c2e; color: #f5f5f7; }

/* Completions panel background */
#completions  { background-color: #1c1c1e; }

/* Selected row: primary blue + white text */
#selection    { background-color: #0a84ff; color: #ffffff; }

/* Fuzzy-match characters: accent gold */
.match        { color: #ffd60a; font-weight: bold; }

/* Unselected rows: on-background white */
.result       { color: #f5f5f7; }"))))
