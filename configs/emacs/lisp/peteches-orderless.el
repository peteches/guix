(provide 'peteches-orderless)
(require 'orderless)

(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))
