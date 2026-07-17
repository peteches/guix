;;; peteches/home/modules/theming.scm — matugen wallpaper-derived theming.
;;;
;;; How the pipeline fits together:
;;;
;;;   wallpaper change (dms-random-wallpaper, hourly via mcron below)
;;;     → matugen extracts a Material palette from the image
;;;     → matugen renders each template in ~/.config/matugen/templates/
;;;       to its output path under ~/.cache/matugen/
;;;     → each app reads its generated fragment:
;;;         wofi       ~/.config/wofi/matugen-colors.css
;;;         hyprlock   ~/.cache/matugen/hyprlock-colours.config
;;;         hyprland   ~/.cache/matugen/hypr-colors.lua
;;;         emacs      ~/.cache/matugen/emacs/matugen-theme.el
;;;         alacritty  ~/.cache/matugen/alacritty/colors.toml
;;;         mako       ~/.cache/matugen/mako/colors.conf  (included via
;;;                    the `anchor' hack in (peteches home modules mako))
;;;         nyxt       ~/.cache/matugen/nyxt/theme.lisp   (+ a post_hook that
;;;                    live-reloads it over nyxt --remote)
;;;         shell      ~/.cache/matugen/colours.sh        (MATUGEN_* vars)
;;;
;;; The `matugen-dms-custom-templates' activation service writes
;;; ~/.config/matugen/config.toml itself — it is generated, not a
;;; local-file, because every path must be absolute and $HOME is only known
;;; at activation time.  It then seeds a Catppuccin-ish *fallback* for each
;;; output, guarded by `unless (file-exists? …)', so a fresh machine has
;;; usable colours before the first wallpaper change and existing generated
;;; themes are never overwritten.
;;;
;;; Templates themselves live in configs/matugen/templates/ and are
;;; installed read-only from the store; the generated outputs are writable
;;; files under ~/.cache.  Editing a theme means editing the template.
;;;
;;; KNOWN BUG: `hyprlock-colours' below builds
;;;   (string-append hypr-cache-dir "hyprlock-colours.config")
;;; with no "/" separator, yielding ~/.cache/matugenhyprlock-colours.config.
;;; It is only used for the fallback-seeding existence check, so the effect
;;; is a stray file and a fallback that never seeds — matugen itself writes
;;; the correct path from config.toml.

(define-module (peteches home modules theming)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (peteches packages desktop-scripts)
  #:use-module (peteches repository)
  #:export (base-theming-services))

(define-public base-theming-services
  (list
   (simple-service 'phinger-cursor-theme
		   home-activation-service-type
		   #~(begin
		       (use-modules (guix build utils))
		       (let* ((home (getenv "HOME"))
			      (state-dir (string-append home "/.local/state/peteches"))
			      (colours-file (string-append home "/.cache/matugen/colours.sh"))
			      (setup #$(file-append peteches-desktop-scripts "/bin/setup-phinger-cursors"))
			      (colorize #$(file-append peteches-desktop-scripts "/bin/colorize-phinger-cursors")))
			 (mkdir-p state-dir)
			 (system* setup "--variant" "dark" "--size" "32" "--best-effort")
			 (when (file-exists? colours-file)
			   (system* colorize "--best-effort" "--size" "32")))))

   (simple-service 'random-wallpaper-hourly
		   home-mcron-service-type
		   (list
		    #~(job '(next-hour '(0 1 2 3 4 5 6 7 8 9 10 11
					    12 13 14 15 16 17 18 19 20 21 22 23))
			   "state_dir=\"${XDG_STATE_HOME:-$HOME/.local/state}/peteches\"; mkdir -p \"$state_dir\"; script=\"$HOME/.guix-home/profile/bin/dms-random-wallpaper\"; test -x \"$script\" && \"$script\" >> \"$state_dir/dms-random-wallpaper-hourly.log\" 2>&1 || true")))

   (simple-service 'hyprlock
		   home-xdg-configuration-files-service-type
		   `(("hypr/hyprlock.conf"
		      ,(local-file (source-path "configs/hypr/hyprlock.config")))))

   (simple-service 'matugen-material-style
		   home-xdg-configuration-files-service-type
		   `(("alacritty/alacritty.toml"
		      ,(local-file (source-path "configs/alacritty/alacritty.toml")))
		     ("wofi/style.css"
		      ,(local-file (source-path "configs/wofi/style.css")))
		     ("matugen/templates/hyprlock-colours.config"
		      ,(local-file (source-path "configs/matugen/templates/hyprlock-colours.config")))
		     ("matugen/templates/wofi-colors.css"
		      ,(local-file (source-path "configs/matugen/templates/wofi-colors.css")))
		     ("matugen/templates/hypr-colors.lua"
		      ,(local-file (source-path "configs/matugen/templates/hypr-colors.lua")))
		     ("matugen/templates/emacs-theme.el"
		      ,(local-file (source-path "configs/matugen/templates/emacs-theme.el")))
		     ("matugen/templates/alacritty-colors.toml"
		      ,(local-file (source-path "configs/matugen/templates/alacritty-colors.toml")))
		     ("matugen/templates/mako-colors.conf"
		      ,(local-file (source-path "configs/matugen/templates/mako-colors.conf")))
		     ("matugen/templates/colours.sh"
		      ,(local-file (source-path "configs/matugen/templates/colours.sh")))
		     ("matugen/templates/nyxt-theme.lisp"
		      ,(local-file (source-path "configs/matugen/templates/nyxt-theme.lisp")))))

   (simple-service 'matugen-dms-custom-templates
		   home-activation-service-type
		   #~(begin
		       (use-modules (ice-9 popen) (ice-9 textual-ports))
		       (let* ((home (getenv "HOME"))
			      (matugen-dir (string-append home "/.config/matugen"))
			      (wofi-dir (string-append home "/.config/wofi"))
			      (hypr-cache-dir (string-append home "/.cache/matugen"))
			      (emacs-theme-dir (string-append home "/.cache/matugen/emacs"))
			      (alacritty-theme-dir (string-append home "/.cache/matugen/alacritty"))
			      (mako-theme-dir (string-append home "/.cache/matugen/mako"))
		      (nyxt-theme-dir (string-append home "/.cache/matugen/nyxt"))
			      (config-file (string-append matugen-dir "/config.toml"))
			      (wofi-colors (string-append wofi-dir "/matugen-colors.css"))
			      (hypr-colors (string-append hypr-cache-dir "/hypr-colors.lua"))
			      (hyprlock-colours (string-append hypr-cache-dir "hyprlock-colours.config"))
			      (emacs-theme (string-append emacs-theme-dir "/matugen-theme.el"))
			      (alacritty-colors (string-append alacritty-theme-dir "/colors.toml"))
			      (mako-colors (string-append mako-theme-dir "/colors.conf"))
		      (nyxt-theme (string-append nyxt-theme-dir "/theme.lisp"))
			      (colours-file (string-append hypr-cache-dir "/colours.sh")))
			 (mkdir-p matugen-dir)
			 (mkdir-p wofi-dir)
			 (mkdir-p hypr-cache-dir)
			 (mkdir-p emacs-theme-dir)
			 (mkdir-p alacritty-theme-dir)
			 (mkdir-p mako-theme-dir)
			 (mkdir-p nyxt-theme-dir)
			 (call-with-output-file config-file
			   (lambda (port)
			     (display "[config]\n\n" port)
			     (display "[templates.wofi]\n" port)
			     (display (string-append "input_path = '" home "/.config/matugen/templates/wofi-colors.css'\n") port)
			     (display (string-append "output_path = '" home "/.config/wofi/matugen-colors.css'\n\n") port)
			     (display "[templates.hyprlock-colours]\n" port)
			     (display (string-append "input_path = '" home "/.config/matugen/templates/hyprlock-colours.config'\n") port)
			     (display (string-append "output_path = '" home "/.cache/matugen/hyprlock-colours.config'\n\n") port)
			     (display "[templates.hyprland_lua]\n" port)
			     (display (string-append "input_path = '" home "/.config/matugen/templates/hypr-colors.lua'\n") port)
			     (display (string-append "output_path = '" home "/.cache/matugen/hypr-colors.lua'\n\n") port)
			     (display "[templates.emacs]\n" port)
			     (display (string-append "input_path = '" home "/.config/matugen/templates/emacs-theme.el'\n") port)
			     (display (string-append "output_path = '" home "/.cache/matugen/emacs/matugen-theme.el'\n\n") port)
			     (display "[templates.alacritty]\n" port)
			     (display (string-append "input_path = '" home "/.config/matugen/templates/alacritty-colors.toml'\n") port)
			     (display (string-append "output_path = '" home "/.cache/matugen/alacritty/colors.toml'\n\n") port)
			     (display "[templates.mako]\n" port)
			     (display (string-append "input_path = '" home "/.config/matugen/templates/mako-colors.conf'\n") port)
			     (display (string-append "output_path = '" home "/.cache/matugen/mako/colors.conf'\n") port)
			     (display "\n[templates.colours]\n" port)
			     (display (string-append "input_path = '" home "/.config/matugen/templates/colours.sh'\n") port)
			     (display (string-append "output_path = '" home "/.cache/matugen/colours.sh'\n") port)
			     (display "\n[templates.nyxt]\n" port)
			     (display (string-append "input_path = '" home "/.config/matugen/templates/nyxt-theme.lisp'\n") port)
			     (display (string-append "output_path = '" home "/.cache/matugen/nyxt/theme.lisp'\n") port)
			     (display "post_hook = \"nyxt --remote --quit --eval '(load (uiop:xdg-cache-home \\\"matugen/nyxt/theme.lisp\\\"))' 2>/dev/null || true\"\n\n" port)))
			 (unless (file-exists? wofi-colors)
			   (call-with-output-file wofi-colors
			     (lambda (port)
			       (display "@define-color background #1e1e2e;\n" port)
			       (display "@define-color on_background #cdd6f4;\n" port)
			       (display "@define-color surface #1e1e2e;\n" port)
			       (display "@define-color on_surface #cdd6f4;\n" port)
			       (display "@define-color surface_container #313244;\n" port)
			       (display "@define-color surface_container_high #45475a;\n" port)
			       (display "@define-color surface_container_highest #585b70;\n" port)
			       (display "@define-color primary #89b4fa;\n" port)
			       (display "@define-color on_primary #11111b;\n" port)
			       (display "@define-color secondary #94e2d5;\n" port)
			       (display "@define-color tertiary #cba6f7;\n" port)
			       (display "@define-color outline #6c7086;\n" port))))
			 (unless (file-exists? hypr-colors)
			   (call-with-output-file hypr-colors
			     (lambda (port)
			       (display "return {\n" port)
			       (display "  active_border = \"rgba(89b4faff)\",\n" port)
			       (display "  inactive_border = \"rgba(313244aa)\",\n" port)
			       (display "  group_active = \"rgba(cba6f7ff)\",\n" port)
			       (display "  group_inactive = \"rgba(313244aa)\",\n" port)
			       (display "  locked_active = \"rgba(f38ba8ff)\",\n" port)
			       (display "  locked_inactive = \"rgba(45475aaa)\",\n" port)
			       (display "  background = \"rgba(11111bff)\",\n" port)
			       (display "}\n" port))))
			 (unless (file-exists? emacs-theme)
			   (call-with-output-file emacs-theme
			     (lambda (port)
			       (display ";;; matugen-theme.el --- fallback generated by Guix activation -*- lexical-binding: t; -*-\n" port)
			       (display "(deftheme matugen \"Fallback theme used before matugen generates wallpaper colours.\")\n" port)
			       (display "(let ((class '((class color) (min-colors 89))))\n" port)
			       (display "  (custom-theme-set-faces\n" port)
			       (display "   'matugen\n" port)
			       (display "   `(default ((,class (:background \"#1e1e2e\" :foreground \"#cdd6f4\"))))\n" port)
			       (display "   `(cursor ((,class (:background \"#89b4fa\"))))\n" port)
			       (display "   `(region ((,class (:background \"#313244\" :foreground \"#cdd6f4\"))))\n" port)
			       (display "   `(mode-line ((,class (:background \"#45475a\" :foreground \"#cdd6f4\"))))\n" port)
			       (display "   `(mode-line-inactive ((,class (:background \"#313244\" :foreground \"#9399b2\"))))\n" port)
			       (display "   `(font-lock-keyword-face ((,class (:foreground \"#89b4fa\" :weight bold))))\n" port)
			       (display "   `(font-lock-string-face ((,class (:foreground \"#94e2d5\"))))\n" port)
			       (display "   `(font-lock-comment-face ((,class (:foreground \"#6c7086\" :slant italic))))\n" port)
			       (display "   `(font-lock-function-name-face ((,class (:foreground \"#cba6f7\"))))))\n" port)
			       (display "(provide-theme 'matugen)\n" port))))
			 (unless (file-exists? alacritty-colors)
			   (call-with-output-file alacritty-colors
			     (lambda (port)
			       (display "# Fallback Alacritty colours used before matugen runs.\n" port)
			       (display "[colors]\n" port)
			       (display "draw_bold_text_with_bright_colors = true\n" port)
			       (display "transparent_background_colors = false\n\n" port)
			       (display "[colors.primary]\n" port)
			       (display "background = \"#1e1e2e\"\n" port)
			       (display "foreground = \"#cdd6f4\"\n" port)
			       (display "dim_foreground = \"#6c7086\"\n" port)
			       (display "bright_foreground = \"#ffffff\"\n\n" port)
			       (display "[colors.cursor]\n" port)
			       (display "text = \"#1e1e2e\"\n" port)
			       (display "cursor = \"#89b4fa\"\n\n" port)
			       (display "[colors.vi_mode_cursor]\n" port)
			       (display "text = \"#1e1e2e\"\n" port)
			       (display "cursor = \"#cba6f7\"\n\n" port)
			       (display "[colors.selection]\n" port)
			       (display "text = \"#cdd6f4\"\n" port)
			       (display "background = \"#313244\"\n\n" port)
			       (display "[colors.normal]\n" port)
			       (display "black = \"#45475a\"\nred = \"#f38ba8\"\ngreen = \"#94e2d5\"\nyellow = \"#f9e2af\"\nblue = \"#89b4fa\"\nmagenta = \"#cba6f7\"\ncyan = \"#94e2d5\"\nwhite = \"#cdd6f4\"\n\n" port)
			       (display "[colors.bright]\n" port)
			       (display "black = \"#6c7086\"\nred = \"#f38ba8\"\ngreen = \"#94e2d5\"\nyellow = \"#f9e2af\"\nblue = \"#89b4fa\"\nmagenta = \"#cba6f7\"\ncyan = \"#94e2d5\"\nwhite = \"#ffffff\"\n" port))))
			 (unless (file-exists? mako-colors)
			   (call-with-output-file mako-colors
			     (lambda (port)
			       (display "# Fallback Mako colours used before matugen runs.\n" port)
			       (display "background-color=#1e1e2eF2\n" port)
			       (display "text-color=#cdd6f4FF\n" port)
			       (display "border-color=#89b4faFF\n" port)
			       (display "progress-color=over #89b4faFF\n\n" port)
			       (display "[actionable]\n" port)
			       (display "border-color=#cba6f7FF\n\n" port)
			       (display "[urgency=critical]\n" port)
			       (display "background-color=#f38ba8F2\n" port)
			       (display "text-color=#11111bFF\n" port)
			       (display "border-color=#f38ba8FF\n" port))))
			 (unless (file-exists? colours-file)
			   (call-with-output-file colours-file
			     (lambda (port)
			       (display "# Fallback colours — replaced by matugen on first wallpaper change.\n" port)
			       (display "MATUGEN_PRIMARY=89b4fa\n" port)
			       (display "MATUGEN_ON_PRIMARY=11111b\n" port)
			       (display "MATUGEN_PRIMARY_CONTAINER=002457\n" port)
			       (display "MATUGEN_ON_PRIMARY_CONTAINER=d8e2ff\n" port)
			       (display "MATUGEN_SECONDARY=bac3dc\n" port)
			       (display "MATUGEN_ON_SECONDARY=243048\n" port)
			       (display "MATUGEN_BACKGROUND=11111b\n" port)
			       (display "MATUGEN_ON_BACKGROUND=cdd6f4\n" port)
			       (display "MATUGEN_SURFACE=11111b\n" port)
			       (display "MATUGEN_ON_SURFACE=cdd6f4\n" port)
			       (display "MATUGEN_OUTLINE=6c7086\n" port))))
			 (unless (file-exists? nyxt-theme)
			   (call-with-output-file nyxt-theme
			     (lambda (port)
			       (display ";; Fallback Nyxt theme — replaced by matugen on first wallpaper change.\n" port)
			       (display "(let ((theme (make-instance 'theme:theme\n" port)
			       (display "               :background-color \"#1e1e2e\"\n" port)
			       (display "               :on-background-color \"#cdd6f4\"\n" port)
			       (display "               :primary-color \"#89b4fa\"\n" port)
			       (display "               :on-primary-color \"#11111b\"\n" port)
			       (display "               :secondary-color \"#313244\"\n" port)
			       (display "               :on-secondary-color \"#cdd6f4\"\n" port)
			       (display "               :action-color \"#89b4fa\"\n" port)
			       (display "               :highlight-color \"#cba6f7\")))\n" port)
			       (display "  (define-configuration browser ((theme theme)))\n" port)
			       (display "  (when (and (boundp 'nyxt:*browser*) nyxt:*browser*\n" port)
			       (display "             (slot-value nyxt:*browser* 'nyxt::ready-p))\n" port)
			       (display "    (setf (theme nyxt:*browser*) theme)))\n" port)))))))

   (simple-service 'dms-submap-plugin
		   home-xdg-configuration-files-service-type
		   `(("DankMaterialShell/plugins/hyprSubmapHint"
		      ,(local-file (repo-directory "configs/dms/plugins/hyprSubmapHint")
				   #:recursive? #t))))))
