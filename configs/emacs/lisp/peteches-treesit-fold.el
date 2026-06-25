;;; peteches-treesit-fold --- my folding settings using treesitter -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Author: Pete "Peteches" McCabe
;;; Maintainer: Pete "Peteches" McCabe
;;; Version: 0.1.0
;;; Package-Requires: ((Emacs "30.2"))
;;; Keywords: convenience
;;; URL: https://github.com/peteches/guix
;; Description: Description.

;;; Code:

(straight-use-package '(treesit-fold
			:host github
			:repo "emacs-tree-sitter/treesit-fold"))

(require 'treesit-fold-indicators)

;; Pull in the optional indicator module so its configuration variables are
;; defined before the `setq' below.
(require 'treesit-fold-indicators)

(define-prefix-command 'treesit-fold-map)
(define-key treesit-fold-map (kbd "t") 'treesit-fold-toggle)
(define-key treesit-fold-map (kbd "c") 'treesit-fold-close)
(define-key treesit-fold-map (kbd "o") 'treesit-fold-open)
(define-key treesit-fold-map (kbd "O") 'treesit-fold-open-all)
(define-key treesit-fold-map (kbd "C") 'treesit-fold-close-all)
(define-key treesit-fold-map (kbd "r") 'treesit-fold-open-recursively)
(global-set-key (kbd "C-c f") 'treesit-fold-map)

(setq
 ;; Alist mapping major modes to foldable tree-sitter node definitions.
 ;; Default: package-provided mode/node parser table.
 treesit-fold-range-alist
 (default-value 'treesit-fold-range-alist)

 ;; Hook run when `treesit-fold-mode' is enabled.
 ;; Default: nil.
 treesit-fold-mode-hook
 nil

 ;; Hook run after folding/unfolding actions.
 ;; Default: nil.
 treesit-fold-on-fold-hook
 nil

 ;; For languages with explicit block-closing keywords, leave closing
 ;; keywords on the next visible line.
 ;; Default: t.
 treesit-fold-on-next-line
 t

 ;; Overlay priority for hidden fold regions.
 ;; Default: 30.
 treesit-fold-priority
 30

 ;; Show the number of folded lines in the replacement text.
 ;; Default: nil.
 treesit-fold-line-count-show
 t

 ;; Format used when `treesit-fold-line-count-show' is non-nil.
 ;; `%d' is replaced with the folded line count.
 ;; Default: (concat (truncate-string-ellipsis) " %d " (truncate-string-ellipsis)).
 treesit-fold-line-count-format
 (concat (truncate-string-ellipsis) " %d folded lines " (truncate-string-ellipsis))

 ;; Show extracted summaries for foldable comments/docstrings when available.
 ;; Default: t.
 treesit-fold-summary-show
 t

 ;; Maximum length of extracted summaries. Set to nil for no truncation.
 ;; Default: 60.
 treesit-fold-summary-max-length
 60

 ;; Format used to display an extracted summary.
 ;; `%s' is replaced with the extracted summary text.
 ;; Default: " <S> %s ".
 treesit-fold-summary-format
 " <S> %s "

 ;; Alist mapping major modes to summary parser functions.
 ;; Default: package-provided mode/parser table.
 treesit-fold-summary-parsers-alist
 (default-value 'treesit-fold-summary-parsers-alist)

 ;; Fringe used by `treesit-fold-indicators-mode'.
 ;; Valid values: `left-fringe' or `right-fringe'.
 ;; Default: left-fringe.
 treesit-fold-indicators-fringe
 'left-fringe

 ;; Overlay priority for fold indicators.
 ;; Default: 30.
 treesit-fold-indicators-priority
 30

 ;; Optional function used to choose the indicator face at a given position.
 ;; Default: nil.
 treesit-fold-indicators-face-function
 nil

 ;; Rendering strategy for indicators.
 ;; Valid values: `partial' or `full'.
 ;; Default: partial.
 treesit-fold-indicators-render-method
 'partial

 ;; Hook run after fold indicators are refreshed.
 ;; Default: nil.
 treesit-fold-indicators-refresh-hook
 nil)

;; Enable folding automatically in buffers that already have a tree-sitter
;; parser and are supported by `treesit-fold-range-alist'.
(global-treesit-fold-mode 1)

(provide 'peteches-treesit-fold)
;;; peteches-treesit-fold.el ends here
