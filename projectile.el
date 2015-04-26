(require 'projectile)

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-require-project-root nil)
(setq projectile-switch-project-action 'projectile-dired)
