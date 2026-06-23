;;; peteches-ui-toys.el --- Visual UI polish -*- lexical-binding: t; -*-

;;; Commentary:
;;; Various ui tweaks and enhancements

;;; Assumes straight.el and use-package are already bootstrapped.


;;; Code:
(require 'use-package)


;;;; Cursor / point effects

(use-package beacon
  :straight t
  :demand t
  :custom
  ;; Push mark before point moves more than this many lines; nil disables it.
  (beacon-push-mark 35)
  ;; Blink after large vertical point movement; nil disables this trigger.
  (beacon-blink-when-point-moves-vertically nil)
  ;; Blink after large horizontal point movement; nil disables this trigger.
  (beacon-blink-when-point-moves-horizontally nil)
  ;; Blink when changing buffers.
  (beacon-blink-when-buffer-changes t)
  ;; Blink when the window scrolls.
  (beacon-blink-when-window-scrolls t)
  ;; Blink when switching windows.
  (beacon-blink-when-window-changes t)
  ;; Blink when Emacs gains focus.
  (beacon-blink-when-focused t)
  ;; Duration of the fade-out animation.
  (beacon-blink-duration 0.3)
  ;; Delay before the beacon starts fading.
  (beacon-blink-delay 0.3)
  ;; Beacon length in characters.
  (beacon-size 40)
  ;; Beacon color; number means brightness derived from theme background.
  (beacon-color 0.5)
  ;; Major modes where beacon should not blink.
  (beacon-dont-blink-major-modes
   '(t magit-status-mode magit-popup-mode inf-ruby-mode
       mu4e-headers-mode gnus-summary-mode gnus-group-mode))
  ;; Commands that should not trigger beacon.
  (beacon-dont-blink-commands
   '(next-line previous-line forward-line))
  ;; Hook run immediately before blinking.
  (beacon-before-blink-hook nil)
  ;; Mode-line lighter.
  (beacon-lighter " (*)")
  :config
  (beacon-mode 1))


(use-package comet-trail
  :straight (:host github :repo "emacsmirror/comet-trail")
  :if (version<= "29.1" emacs-version)
  :hook
  ((prog-mode text-mode conf-mode) . comet-trail-mode)
  :custom
  ;; Face used for the trail; nil uses cursor color.
  (comet-trail-face 'comet-trail-highlight)
  ;; Maximum visible trail length.
  (comet-trail-length 10)
  ;; Duration of the trail animation in seconds.
  (comet-trail-speed 0.2)
  ;; Timer interval between frames; roughly 60 FPS.
  (comet-trail-tick-interval 0.016)
  ;; Brightness falloff curve for the trail.
  (comet-trail-fade-exponent 2.0)
  ;; Minimum movement distance before animating.
  (comet-trail-minimum-distance 2))


;;;; Pulse / edit feedback

(use-package pulsar
  :straight t
  :demand t
  :custom
  ;; Delay between pulse increments.
  (pulsar-delay 0.05)
  ;; Number of pulse increments.
  (pulsar-iterations pulse-iterations)
  ;; Commands that pulse the current line after running.
  (pulsar-pulse-functions
   '(ace-window
     backward-page
     bookmark-jump
     delete-other-windows
     delete-window
     dired-maybe-insert-subdir
     dired-up-directory
     dired-goto-file
     dired-next-dirline
     dired-prev-dirline
     evil-goto-first-line
     evil-goto-line
     evil-scroll-down
     evil-scroll-line-to-bottom
     evil-scroll-line-to-center
     evil-scroll-line-to-top
     evil-scroll-page-down
     evil-scroll-page-up
     evil-scroll-up
     evil-window-bottom
     evil-window-delete
     evil-window-down
     evil-window-left
     evil-window-new
     evil-window-next
     evil-window-right
     evil-window-split
     evil-window-top
     evil-window-up
     evil-window-vsplit
     forward-page
     goto-line
     handle-switch-frame
     imenu
     logos-backward-page-dwim
     logos-forward-page-dwim
     handle-select-window
     move-to-window-line-top-bottom
     narrow-to-defun
     narrow-to-page
     narrow-to-region
     next-buffer
     next-error
     next-error-recenter
     next-multiframe-window
     occur-mode-goto-occurrence
     org-backward-heading-same-level
     org-forward-heading-same-level
     org-next-visible-heading
     org-previous-visible-heading
     other-window
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     outline-up-heading
     previous-buffer
     previous-error
     quit-window
     recenter-top-bottom
     reposition-window
     scroll-down-command
     scroll-up-command
     tab-close
     tab-new
     tab-next
     tab-previous
     widen
     windmove-down
     windmove-left
     windmove-right
     windmove-swap-states-down
     windmove-swap-states-left
     windmove-swap-states-right
     windmove-swap-states-up
     windmove-up))
  ;; Commands that pulse affected regions.
  (pulsar-pulse-region-functions
   '(append-next-kill
     delete-region
     evil-delete
     evil-delete-line
     evil-paste-after
     evil-paste-before
     evil-undo
     evil-yank
     evil-yank-line
     kill-line
     kill-paragraph
     backward-kill-paragraph
     kill-rectangle
     kill-region
     kill-ring-save
     kill-sentence
     backward-kill-sentence
     kill-sexp
     backward-kill-sexp
     kill-visual-line
     kill-whole-line
     kill-word
     backward-kill-word
     open-rectangle
     undo
     yank
     yank-rectangle))
  ;; Resolve command aliases in the pulse command lists.
  (pulsar-resolve-pulse-function-aliases t)
  ;; Do not enable Pulsar in hidden buffers.
  (pulsar-inhibit-hidden-buffers t)
  ;; Pulse on every window change.
  (pulsar-pulse-on-window-change t)
  ;; Face used for regular line pulses.
  (pulsar-face 'pulsar-generic)
  ;; Face used for static temporary/permanent highlights.
  (pulsar-highlight-face pulsar-face)
  ;; Face used for pulsing unchanged regions.
  (pulsar-region-face pulsar-face)
  ;; Face used for pulsing changed regions.
  (pulsar-region-change-face pulsar-face)
  ;; Face used for window-change pulses.
  (pulsar-window-change-face pulsar-face)
  :config
  (pulsar-global-mode 1))


;;;; Syntax / decorative text

(use-package rainbow-delimiters
  :straight t
  :hook
  ((prog-mode . rainbow-delimiters-mode))
  :custom
  ;; Function used to choose the face for a delimiter.
  (rainbow-delimiters-pick-face-function
   #'rainbow-delimiters-default-pick-face)
  ;; Number of depth faces before cycling.
  (rainbow-delimiters-max-face-count 9)
  ;; Number of outermost faces reserved before cycling inner faces.
  (rainbow-delimiters-outermost-only-face-count 0))

(use-package svg-lib
  :straight t
  :defer t
  :custom
  ;; Icon collection URL templates.
  (svg-lib-icon-collections
   '(("bootstrap" . "https://icons.getbootstrap.com/assets/icons/%s.svg")
     ("simple" . "https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
     ("material" . "https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
     ("octicons" . "https://raw.githubusercontent.com/primer/octicons/main/icons/%s-24.svg")
     ("boxicons" . "https://boxicons.com/static/img/svg/regular/bx-%s.svg")
     ("vscode" . "https://raw.githubusercontent.com/microsoft/vscode-icons/main/icons/light/%s.svg")))
  ;; Local SVG icon cache directory.
  (svg-lib-icons-dir
   (expand-file-name (concat user-emacs-directory ".cache/svg-lib/")))
  ;; Default style plist; nil means compute it from the current frame/font.
  (svg-lib-style-default nil))


(defun peteches/svg-tag-mode-maybe ()
  "Enable `svg-tag-mode' when SVG display is available."
  (when (and (display-graphic-p)
             (image-type-available-p 'svg))
    (svg-tag-mode 1)))

(use-package svg-tag-mode
  :straight t
  :after svg-lib
  :hook
  ((org-mode text-mode) . peteches/svg-tag-mode-maybe)
  :custom
  (svg-tag-action-at-point 'echo)
  (svg-tag-tags
   '(("\\(:TODO:\\)" .
      ((lambda (tag)
         (svg-tag-make tag :beg 1 :end -1)))))))

;; :TODO:
;;;; Icons and modeline

;; The core `nerd-icons' library exposes no defcustom user options.
;; Manage the actual Nerd Font through Guix.
(use-package nerd-icons
  :straight t
  :defer t)

(use-package doom-modeline
  :straight t
  :init
  ;; Add imenu support for doom-modeline declarations; must be set before load.
  (setq doom-modeline-support-imenu nil)
  :hook
  (after-init . doom-modeline-mode)
  :custom
  ;; Mode-line height in GUI frames.
  (doom-modeline-height (+ (window-font-height nil 'mode-line) 4))
  ;; Width of the left mode-line bar.
  (doom-modeline-bar-width 4)
  ;; Use HUD bar instead of the default bar.
  (doom-modeline-hud nil)
  ;; Minimum pixel height of the HUD thumb.
  (doom-modeline-hud-min-height 2)
  ;; Hide some details below this window width.
  (doom-modeline-window-width-limit 85)
  ;; Face overrides for spacing segments.
  (doom-modeline-spc-face-overrides nil)
  ;; Project root detection backend.
  (doom-modeline-project-detection 'auto)
  ;; How buffer file names are shortened.
  (doom-modeline-buffer-file-name-style 'auto)
  ;; Use `file-truename' for buffer file names.
  (doom-modeline-buffer-file-true-name nil)
  ;; Show icons.
  (doom-modeline-icon t)
  ;; Show major-mode icon.
  (doom-modeline-major-mode-icon t)
  ;; Use colorful major-mode icon.
  (doom-modeline-major-mode-color-icon t)
  ;; Show buffer state icon.
  (doom-modeline-buffer-state-icon t)
  ;; Show modified-buffer icon.
  (doom-modeline-buffer-modification-icon t)
  ;; Show LSP client icon.
  (doom-modeline-lsp-icon t)
  ;; Show time icon.
  (doom-modeline-time-icon t)
  ;; Use live time icons.
  (doom-modeline-time-live-icon t)
  ;; Draw analogue clock SVG for live time.
  (doom-modeline-time-analogue-clock t)
  ;; Clock update resolution in minutes.
  (doom-modeline-time-clock-minute-resolution 1)
  ;; Analogue clock icon size.
  (doom-modeline-time-clock-size 0.7)
  ;; Use Unicode numbers.
  (doom-modeline-unicode-number t)
  ;; Use Unicode fallback when icons are disabled.
  (doom-modeline-unicode-fallback nil)
  ;; Show buffer name.
  (doom-modeline-buffer-name t)
  ;; Highlight modified buffer name.
  (doom-modeline-highlight-modified-buffer-name t)
  ;; Display zero-based columns.
  (doom-modeline-column-zero-based t)
  ;; Buffer percentage display.
  (doom-modeline-percent-position '(-3 "%p"))
  ;; Line-number display format.
  (doom-modeline-position-line-format '("L%l"))
  ;; Column-number display format.
  (doom-modeline-position-column-format '("C%c"))
  ;; Combined line/column display format.
  (doom-modeline-position-column-line-format '("%l:%c"))
  ;; Show minor modes.
  (doom-modeline-minor-modes nil)
  ;; Show selection information.
  (doom-modeline-selection-info t)
  ;; Show word count in selection info.
  (doom-modeline-enable-word-count nil)
  ;; Modes where word count can be continuous.
  (doom-modeline-continuous-word-count-modes
   '(markdown-mode gfm-mode org-mode))
  ;; Show buffer position.
  (doom-modeline-enable-buffer-position t)
  ;; Show buffer encoding.
  (doom-modeline-buffer-encoding t)
  ;; Baseline coding system for nondefault encoding display.
  (doom-modeline-default-coding-system 'utf-8)
  ;; Baseline EOL type for nondefault encoding display.
  (doom-modeline-default-eol-type 0)
  ;; Show indentation info.
  (doom-modeline-indent-info nil)
  ;; Show total line count.
  (doom-modeline-total-line-number nil)
  ;; Show remote host.
  (doom-modeline-remote-host t)
  ;; Major-mode to indentation-variable lookup.
  (doom-modeline-indent-alist
   '((apache-mode apache-indent-level)
     (awk-mode c-basic-offset)
     (awk-ts-mode awk-ts-mode-indent-level)
     (bpftrace-mode c-basic-offset)
     (c-mode c-basic-offset)
     (c++-mode c-basic-offset)
     (c-ts-mode c-ts-mode-indent-offset)
     (c++-ts-mode c-ts-mode-indent-offset)
     (cmake-mode cmake-tab-width)
     (cmake-ts-mode cmake-ts-mode-indent-offset)
     (coffee-mode coffee-tab-width)
     (cperl-mode cperl-indent-level)
     (crystal-mode crystal-indent-level)
     (csharp-mode c-basic-offset)
     (csharp-ts-mode csharp-ts-mode-indent-offset)
     (css-mode css-indent-offset)
     (css-ts-mode css-indent-offset)
     (d-mode c-basic-offset)
     (emacs-lisp-mode lisp-indent-offset)
     (enh-ruby-mode enh-ruby-indent-level)
     (erlang-mode erlang-indent-level)
     (ess-mode ess-indent-offset)
     (f90-mode f90-associate-indent f90-continuation-indent
               f90-critical-indent f90-do-indent f90-if-indent
               f90-program-indent f90-type-indent)
     (feature-mode feature-indent-offset feature-indent-level)
     (fsharp-mode fsharp-continuation-offset fsharp-indent-level
                  fsharp-indent-offset)
     (groovy-mode groovy-indent-offset)
     (haskell-mode haskell-indent-spaces haskell-indent-offset
                   haskell-indentation-layout-offset
                   haskell-indentation-left-offset
                   haskell-indentation-starter-offset
                   haskell-indentation-where-post-offset
                   haskell-indentation-where-pre-offset
                   shm-indent-spaces)
     (haskell-ts-mode)
     (haxor-mode haxor-tab-width)
     (idl-mode c-basic-offset)
     (jade-mode jade-tab-width)
     (java-mode c-basic-offset)
     (java-ts-mode java-ts-mode-indent-offset c-ts-common-statement-offset)
     (js-mode js-indent-level)
     (js-ts-mode js-indent-level)
     (js-jsx-mode js-indent-level sgml-basic-offset)
     (js2-mode js2-basic-offset)
     (js2-jsx-mode js2-basic-offset sgml-basic-offset)
     (js3-mode js3-indent-level)
     (json-mode js-indent-level)
     (json-ts-mode json-ts-mode-indent-offset)
     (julia-mode julia-indent-offset)
     (julia-ts-mode julia-ts-indent-offset)
     (kotlin-mode kotlin-tab-width)
     (kotlin-mode kotlin-ts-mode-indent-offset)
     (latex-mode tex-indent-basic)
     (lisp-mode lisp-indent-offset)
     (livescript-mode livescript-tab-width)
     (lua-mode lua-indent-level)
     (lua-ts-mode lua-ts-indent-offset)
     (matlab-mode matlab-indent-level)
     (mips-mode mips-tab-width)
     (mustache-mode mustache-basic-offset)
     (nasm-mode nasm-basic-offset)
     (nginx-mode nginx-indent-level)
     (nxml-mode nxml-child-indent)
     (objc-mode c-basic-offset)
     (octave-mode octave-block-offset)
     (perl-mode perl-indent-level)
     (php-mode c-basic-offset)
     (php-ts-mode php-ts-mode-indent-offset)
     (pike-mode c-basic-offset)
     (ps-mode ps-mode-tab)
     (pug-mode pug-tab-width)
     (puppet-mode puppet-indent-level)
     (python-mode python-indent-offset)
     (python-ts-mode python-indent-offset)
     (ruby-mode ruby-indent-level)
     (ruby-ts-mode ruby-indent-level)
     (rust-mode rust-indent-offset)
     (rust-ts-mode rust-ts-mode-indent-offset)
     (rustic-mode rustic-indent-offset)
     (scala-mode scala-indent:step)
     (scala-ts-mode scala-ts-indent-offset)
     (scss-mode css-indent-offset)
     (sgml-mode sgml-basic-offset)
     (sh-mode sh-basic-offset sh-indentation)
     (bash-ts-mode sh-basic-offset sh-indentation)
     (slim-mode slim-indent-offset)
     (sml-mode sml-indent-level)
     (tcl-mode tcl-indent-level tcl-continued-indent-level)
     (terra-mode terra-indent-level)
     (typescript-mode typescript-indent-level)
     (typescript-ts-mode typescript-ts-mode-indent-offset)
     (verilog-mode verilog-indent-level verilog-indent-level-behavioral
                   verilog-indent-level-declaration verilog-indent-level-module
                   verilog-cexp-indent verilog-case-indent)
     (web-mode web-mode-attr-indent-offset web-mode-attr-value-indent-offset
               web-mode-code-indent-offset web-mode-css-indent-offset
               web-mode-markup-indent-offset web-mode-sql-indent-offset
               web-mode-block-padding web-mode-script-padding
               web-mode-style-padding)
     (yaml-mode yaml-indent-offset)
     (yaml-ts-mode yaml-indent-offset)))
  ;; Show VCS icon.
  (doom-modeline-vcs-icon t)
  ;; Maximum displayed VCS branch length.
  (doom-modeline-vcs-max-length 15)
  ;; Function used to display VCS branch names.
  (doom-modeline-vcs-display-function #'doom-modeline-vcs-name)
  ;; Faces used for selected VCS states.
  (doom-modeline-vcs-state-faces-alist
   '((needs-update . (doom-modeline-warning bold))
     (removed . (doom-modeline-urgent bold))
     (conflict . (doom-modeline-urgent bold))
     (unregistered . (doom-modeline-urgent bold))))
  ;; Show diagnostics/check icon.
  (doom-modeline-check-icon t)
  ;; Diagnostics display style.
  (doom-modeline-check 'auto)
  ;; Maximum displayed notification count.
  (doom-modeline-number-limit 99)
  ;; Show project name.
  (doom-modeline-project-name (bound-and-true-p project-mode-line))
  ;; Show workspace name.
  (doom-modeline-workspace-name t)
  ;; Show perspective name.
  (doom-modeline-persp-name t)
  ;; Show default perspective name.
  (doom-modeline-display-default-persp-name nil)
  ;; Show perspective icon.
  (doom-modeline-persp-icon t)
  ;; Show REPL state.
  (doom-modeline-repl t)
  ;; Show LSP state.
  (doom-modeline-lsp t)
  ;; Show GitHub notifications.
  (doom-modeline-github nil)
  ;; GitHub notification polling interval.
  (doom-modeline-github-interval 1800)
  ;; Show programming environment version.
  (doom-modeline-env-version t)
  ;; Show modal editing state.
  (doom-modeline-modal t)
  ;; Show modal editing icon.
  (doom-modeline-modal-icon t)
  ;; Use modern modal icons.
  (doom-modeline-modal-modern-icon t)
  ;; Always show Evil macro register.
  (doom-modeline-always-show-macro-register nil)
  ;; Show mu4e notifications.
  (doom-modeline-mu4e nil)
  ;; Show Gnus notifications.
  (doom-modeline-gnus nil)
  ;; Gnus polling interval in minutes.
  (doom-modeline-gnus-timer 2)
  ;; Use idle time for Gnus polling.
  (doom-modeline-gnus-idle nil)
  ;; Gnus groups excluded from unread count.
  (doom-modeline-gnus-excluded-groups nil)
  ;; Show IRC notifications.
  (doom-modeline-irc t)
  ;; Show unread IRC buffers.
  (doom-modeline-irc-buffers nil)
  ;; Show only priority IRC notifications.
  (doom-modeline-irc-priority-only nil)
  ;; Function used to style IRC buffer names.
  (doom-modeline-irc-stylize #'doom-modeline-shorten-irc)
  ;; Show battery status when `display-battery-mode' is active.
  (doom-modeline-battery t)
  ;; Show time when `display-time-mode' is active.
  (doom-modeline-time t)
  ;; Show misc segment in all mode lines.
  (doom-modeline-display-misc-in-all-mode-lines t)
  ;; Segments visible even in inactive windows.
  (doom-modeline-always-visible-segments nil)
  ;; Function applied to `buffer-file-name'.
  (doom-modeline-buffer-file-name-function #'identity)
  ;; Function applied to `buffer-file-truename'.
  (doom-modeline-buffer-file-truename-function #'identity)
  ;; Show Kubernetes namespace.
  (doom-modeline-k8s-show-namespace t)
  ;; Modelines excluded from programmatic segment changes.
  (doom-modeline-excluded-modelines nil))


(use-package doom-modeline-env
  :straight nil
  :after doom-modeline
  :custom
  ;; Placeholder displayed while an environment version is loading.
  (doom-modeline-env-load-string doom-modeline-ellipsis)
  ;; Hook run before environment version string updates.
  (doom-modeline-before-update-env-hook nil)
  ;; Hook run after environment version string updates.
  (doom-modeline-after-update-env-hook nil)
  ;; Show Python environment version.
  (doom-modeline-env-enable-python t)
  ;; Python executable override.
  (doom-modeline-env-python-executable nil)
  ;; Show Ruby environment version.
  (doom-modeline-env-enable-ruby t)
  ;; Ruby executable override.
  (doom-modeline-env-ruby-executable nil)
  ;; Show Perl environment version.
  (doom-modeline-env-enable-perl t)
  ;; Perl executable override.
  (doom-modeline-env-perl-executable nil)
  ;; Show Go environment version.
  (doom-modeline-env-enable-go t)
  ;; Go executable override.
  (doom-modeline-env-go-executable nil)
  ;; Show Elixir environment version.
  (doom-modeline-env-enable-elixir t)
  ;; Elixir executable override.
  (doom-modeline-env-elixir-executable nil)
  ;; Show Rust environment version.
  (doom-modeline-env-enable-rust t)
  ;; Rust executable override.
  (doom-modeline-env-rust-executable nil))


(use-package spacious-padding
  :straight t
  :hook
  (after-init . spacious-padding-mode)
  :custom
  ;; Pixel widths for frame/window padding.
  (spacious-padding-widths
   '(:internal-border-width 15
     :header-line-width 4
     :mode-line-width 6
     :custom-button-width 3
     :tab-width 4
     :right-divider-width 30
     :scroll-bar-width 8
     :fringe-width 8))
  ;; Use subtle overlines/underlines for mode/header lines.
  (spacious-padding-subtle-frame-lines nil))


(provide 'peteches-ui-toys)

;;; peteches-ui-toys.el ends here
