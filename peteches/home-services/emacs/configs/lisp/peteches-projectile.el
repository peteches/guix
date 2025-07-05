(provide 'peteches-projectile)
(require 'projectile)

(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
(global-set-key (kbd "C-c p") 'projectile-command-map)

(setq projectile-per-project-compilation-buffer t)


