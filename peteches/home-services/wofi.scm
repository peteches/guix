(define-module (peteches home-services wofi)
  ;; core Guix/home modules
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix gexp)
  #:use-module (guix records)

  ;; export the record type & service type
  #:export (wofi-config
            wofi-service-type))


;; The record describing user config.
;; NOTE:
;; - fixed duplicate "columns" field / accessor
;; - fixed stray "_search"
;; - renamed content-haligns -> content_halign to match serialize fn
(define-record-type* <wofi-config>
  wofi-config make-wofi-config wofi-config?
  ;; appearance / theme
  (style                wofi-config-style                (default ""))
  (stylesheet           wofi-config-stylesheet           (default ""))
  (color                wofi-config-color                (default ""))
  (colors               wofi-config-colors               (default ""))
  (mode                 wofi-config-mode                 (default ""))
  (width                wofi-config-width                (default ""))
  (height               wofi-config-height               (default ""))
  (prompt               wofi-config-prompt               (default ""))
  (xoffset              wofi-config-xoffset              (default ""))
  (yoffset              wofi-config-yoffset              (default ""))
  (normal_window        wofi-config-normal_window        (default ""))
  (allow_images         wofi-config-allow_images         (default ""))
  (allow_markup         wofi-config-allow_markup         (default ""))
  (cache_file           wofi-config-cache_file           (default ""))
  (term                 wofi-config-term                 (default ""))
  (password             wofi-config-password             (default ""))
  (exec_search          wofi-config-exec_search          (default ""))
  (no_custom_entry      wofi-config-no_custom_entry      (default ""))
  (hide_scroll          wofi-config-hide_scroll          (default ""))
  (matching             wofi-config-matching             (default ""))
  (insensitive          wofi-config-insensitive          (default ""))
  (parse_search         wofi-config-parse_search         (default ""))
  (location             wofi-config-location             (default ""))
  (no_actions           wofi-config-no_actions           (default ""))
  (lines                wofi-config-lines                (default ""))
  (columns              wofi-config-columns              (default ""))
  (sort_order           wofi-config-sort_order           (default ""))
  (gtk_dark             wofi-config-gtk_dark             (default ""))
  (search               wofi-config-search               (default ""))
  (monitor              wofi-config-monitor              (default ""))
  (pre_display_cmd      wofi-config-pre_display_cmd      (default ""))
  (orientation          wofi-config-orientation          (default ""))
  (halign               wofi-config-halign               (default ""))
  (content_halign       wofi-config-content_halign       (default ""))
  (valign               wofi-config-valign               (default ""))
  (filter_rate          wofi-config-filter_rate          (default ""))
  (image_size           wofi-config-image_size           (default ""))
  ;; keybindings
  (key_up               wofi-config-key_up               (default ""))
  (key_down             wofi-config-key_down             (default ""))
  (key_left             wofi-config-key_left             (default ""))
  (key_right            wofi-config-key_right            (default ""))
  (key_forward          wofi-config-key_forward          (default ""))
  (key_backward         wofi-config-key_backward         (default ""))
  (key_submit           wofi-config-key_submit           (default ""))
  (key_exit             wofi-config-key_exit             (default ""))
  (key_pgup             wofi-config-key_pgup             (default ""))
  (key_pgdn             wofi-config-key_pgdn             (default ""))
  (key_expand           wofi-config-key_expand           (default ""))
  (key_hide             wofi-config-key_hide             (default ""))
  (key_copy             wofi-config-key_copy             (default ""))
  (key_custom           wofi-config-key_custom           (default ""))
  ;; misc
  (line_wrap            wofi-config-line_wrap            (default ""))
  (global_coords        wofi-config-global_coords        (default ""))
  (hide_search          wofi-config-hide_search          (default ""))
  (close_on_focus_loss  wofi-config-close_on_focus_loss  (default ""))
  (dynamic_lines        wofi-config-dynamic_lines        (default ""))
  (layer                wofi-config-layer                (default ""))
  (copy_exec            wofi-config-copy_exec            (default ""))
  (single_click         wofi-config-single_click         (default ""))
  (pre_display_exec     wofi-config-pre_display_exec     (default ""))
  (use_search_box       wofi-config-use_search_box       (default "")))

;; Helper: write a key/value pair if the value is non-empty.
;; Wofi treats "false" / "" differently, so:
;; - If the value is "" we skip the line entirely.
;; - Otherwise we emit key=value
(define (maybe-line key value)
  (if (and (string? value)
           (string-null? value))
      ""
      (string-append key "=" value "\n")))

;; Turn a <wofi-config> record into the text of ~/.config/wofi/config
(define (serialize-wofi-config cfg)
  (string-append
   "[wofi]\n"
   (maybe-line "style"                (wofi-config-style cfg))
   (maybe-line "stylesheet"           (wofi-config-stylesheet cfg))
   (maybe-line "color"                (wofi-config-color cfg))
   (maybe-line "colors"               (wofi-config-colors cfg))
   (maybe-line "mode"                 (wofi-config-mode cfg))
   (maybe-line "width"                (wofi-config-width cfg))
   (maybe-line "height"               (wofi-config-height cfg))
   (maybe-line "prompt"               (wofi-config-prompt cfg))
   (maybe-line "xoffset"              (wofi-config-xoffset cfg))
   (maybe-line "yoffset"              (wofi-config-yoffset cfg))
   (maybe-line "normal_window"        (wofi-config-normal_window cfg))
   (maybe-line "allow_images"         (wofi-config-allow_images cfg))
   (maybe-line "allow_markup"         (wofi-config-allow_markup cfg))
   (maybe-line "cache_file"           (wofi-config-cache_file cfg))
   (maybe-line "term"                 (wofi-config-term cfg))
   (maybe-line "password"             (wofi-config-password cfg))
   (maybe-line "exec_search"          (wofi-config-exec_search cfg))
   (maybe-line "no_custom_entry"      (wofi-config-no_custom_entry cfg))
   (maybe-line "hide_scroll"          (wofi-config-hide_scroll cfg))
   (maybe-line "matching"             (wofi-config-matching cfg))
   (maybe-line "insensitive"          (wofi-config-insensitive cfg))
   (maybe-line "parse_search"         (wofi-config-parse_search cfg))
   (maybe-line "location"             (wofi-config-location cfg))
   (maybe-line "no_actions"           (wofi-config-no_actions cfg))
   (maybe-line "lines"                (wofi-config-lines cfg))
   (maybe-line "columns"              (wofi-config-columns cfg))
   (maybe-line "sort_order"           (wofi-config-sort_order cfg))
   (maybe-line "gtk_dark"             (wofi-config-gtk_dark cfg))
   (maybe-line "search"               (wofi-config-search cfg))
   (maybe-line "monitor"              (wofi-config-monitor cfg))
   (maybe-line "pre_display_cmd"      (wofi-config-pre_display_cmd cfg))
   (maybe-line "orientation"          (wofi-config-orientation cfg))
   (maybe-line "halign"               (wofi-config-halign cfg))
   (maybe-line "content_halign"       (wofi-config-content_halign cfg))
   (maybe-line "valign"               (wofi-config-valign cfg))
   (maybe-line "filter_rate"          (wofi-config-filter_rate cfg))
   (maybe-line "image_size"           (wofi-config-image_size cfg))
   (maybe-line "key_up"               (wofi-config-key_up cfg))
   (maybe-line "key_down"             (wofi-config-key_down cfg))
   (maybe-line "key_left"             (wofi-config-key_left cfg))
   (maybe-line "key_right"            (wofi-config-key_right cfg))
   (maybe-line "key_forward"          (wofi-config-key_forward cfg))
   (maybe-line "key_backward"         (wofi-config-key_backward cfg))
   (maybe-line "key_submit"           (wofi-config-key_submit cfg))
   (maybe-line "key_exit"             (wofi-config-key_exit cfg))
   (maybe-line "key_pgup"             (wofi-config-key_pgup cfg))
   (maybe-line "key_pgdn"             (wofi-config-key_pgdn cfg))
   (maybe-line "key_expand"           (wofi-config-key_expand cfg))
   (maybe-line "key_hide"             (wofi-config-key_hide cfg))
   (maybe-line "key_copy"             (wofi-config-key_copy cfg))
   (maybe-line "key_custom"           (wofi-config-key_custom cfg))
   (maybe-line "line_wrap"            (wofi-config-line_wrap cfg))
   (maybe-line "global_coords"        (wofi-config-global_coords cfg))
   (maybe-line "hide_search"          (wofi-config-hide_search cfg))
   (maybe-line "close_on_focus_loss"  (wofi-config-close_on_focus_loss cfg))
   (maybe-line "dynamic_lines"        (wofi-config-dynamic_lines cfg))
   (maybe-line "layer"                (wofi-config-layer cfg))
   (maybe-line "copy_exec"            (wofi-config-copy_exec cfg))
   (maybe-line "single_click"         (wofi-config-single_click cfg))
   (maybe-line "pre_display_exec"     (wofi-config-pre_display_exec cfg))
   (maybe-line "use_search_box"       (wofi-config-use_search_box cfg))))

;; 1. Add wofi itself to the home profile
(define (wofi-profile-service config)
  (list wofi))

;; 2. Install ~/.config/wofi/config with the serialized data
;;
;; We use home-files-service-type to drop files into $HOME.
;; The 'local-file' is a gexp that writes the serialized config
;; at activation time, so it's rebuilt when the record changes.
;;
(define (wofi-files-service cfg)
  `(("wofi/config"
     ,(plain-file "wofi.conf"
                  (serialize-wofi-config cfg)))))

;; Finally, wire both services into a service-type.
;; We extend:
;;   - home-profile-service-type (so wofi is in the user profile)
;;   - home-files-service-type   (so the config file is placed)
(define-public wofi-service-type
  (service-type
   (name 'wofi-config)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      wofi-profile-service)
     (service-extension
      home-xdg-configuration-files-service-type
      wofi-files-service)))
   (default-value
     (wofi-config
      (mode "drun")))   ; pick a sane default
   (description
    "Applies my personal Wofi configuration and scripts.")))
