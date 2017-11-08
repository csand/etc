;;; init-projectile.el --- Project-wide navigation and functions

(use-package projectile
  :ensure t
  :commands (projectile-find-file projectile-switch-project)
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-enable-caching t
          projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "bower_components")
    (projectile-global-mode)))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config
  (counsel-projectile-on))

(provide 'init-projectile)
