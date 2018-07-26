;;; init-projectile.el --- Project-wide navigation and functions

(use-package projectile
  :ensure t
  :pin melpa
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-enable-caching is-windows
          projectile-indexing-method 'alien
          projectile-completion-system 'ivy)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    (add-to-list 'projectile-globally-ignored-directories "bower_components")
    (projectile-register-project-type 'npm '("package.json")
                                      :test "npm --no-color run test"
                                      :test-suffix ".spec")
    (add-to-list 'projectile-other-file-alist
                 '("js" "spec.js" "scss" "html"))
    (add-to-list 'projectile-other-file-alist
                 '("spec.js" "js"))
    (projectile-global-mode)))

(use-package counsel-projectile
  :ensure t
  :pin melpa
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

(provide 'init-projectile)
