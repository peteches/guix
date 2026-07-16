;;; Load matugen-generated theme if available; fall back to Catppuccin Mocha.
(let ((theme-file (merge-pathnames "matugen/nyxt/theme.lisp"
                                   (uiop:xdg-cache-home))))
  (if (probe-file theme-file)
      (load theme-file)
      (define-configuration browser
        ((theme (make-instance 'theme:theme
                               :background-color "#1e1e2e"
                               :on-background-color "#cdd6f4"
                               :primary-color "#89b4fa"
                               :on-primary-color "#11111b"
                               :secondary-color "#313244"
                               :on-secondary-color "#cdd6f4"
                               :action-color "#89b4fa"
                               :highlight-color "#cba6f7"))))))
