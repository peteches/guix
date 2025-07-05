(provide 'peteches-which-key)
(require 'which-key)

(message "Loading which key configs")

(setq which-key-idle-delay 0.3)
(which-key-mode)

;; By default, Which-key doesn't give much help for prefix-keys.  It
;; either shows the generic description, "+prefix", or the name of a
;; prefix-command, which usually isn't as descriptive as we'd like.
;;
;; Here are some descriptions for the default bindings in `global-map'
;; and `org-mode-map'.

(which-key-add-key-based-replacements
  "<f1> 4"        "help-other-win"
  "<f1>"          "help"
  "<f2>"          "2-column"
  "C-c"           "mode-and-user"
  "C-h 4"         "help-other-win"
  "C-h"           "help"
  "C-x 4"         "other-window"
  "C-x 5"         "other-frame"
  "C-x 6"         "2-column"
  "C-x 8"         "insert-special"
  "C-x C-k C-q"   "kmacro-counters"
  "C-x C-k C-r a" "kmacro-add"
  "C-x C-k C-r"   "kmacro-register"
  "C-x C-k"       "keyboard-macros"
  "C-x RET"       "encoding/input"
  "C-x a i"       "abbrevs-inverse-add"
  "C-x a"         "abbrevs"
  "C-x n"         "narrowing"
  "C-x p"         "projects"
  "C-x r"         "reg/rect/bkmks"
  "C-x t ^"       "tab-bar-detach"
  "C-x t"         "tab-bar"
  "C-x v M"       "vc-mergebase"
  "C-x v b"       "vc-branch"
  "C-x v"         "version-control"
  "C-x w ^"       "window-detach"
  "C-x w"         "window-extras"
  "C-x x"         "buffer-extras"
  "C-x"           "extra-commands"
  "M-g"           "goto-map"
  "M-s h"         "search-highlight"
  "M-s"           "search-map")

;; Upon loading, the built-in `page-ext' package turns "C-x C-p" into
;; a prefix-key.  If you know of other built-in packages that have
;; this behavior, please let me know, so I can add them.
(with-eval-after-load 'page-ext
  (which-key-add-key-based-replacements
    "C-x C-p" "page-extras"))

;; Org-mode provides some additional prefix-keys in `org-mode-map'.
(with-eval-after-load 'org
  (which-key-add-keymap-based-replacements org-mode-map
    "C-c \""      "org-plot"
    "C-c C-v"     "org-babel"
    "C-c C-x"     "org-extra-commands"))
