;;; init-projectile.el --- Project-wide navigation and functions

(use-package projectile
  :pin melpa
  :diminish projectile-mode
  :custom
  (projectile-indexing-method 'alien)
  (projectile-completion-system 'ivy)
  (projectile-globally-ignored-files '(".DS_Store"))
  (projectile-globally-ignored-directories
   '("bower_components"
     "node_modules"
     ".idea"
     ".ensime_cache"
     ".eunit"
     ".git"
     ".hg"
     ".fslckout"
     "_FOSSIL_"
     ".bzr"
     "_darcs"
     ".tox"
     ".svn"
     ".stack-work"))
  :config
  (setq projectile-enable-caching is-windows)

  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm run build"
                                    :test "npm test"
                                    :test-suffix ".spec")

  (add-to-list 'projectile-other-file-alist
               '("js" "spec.js" "scss" "html"))
  (add-to-list 'projectile-other-file-alist
               '("spec.js" "js"))
  (projectile-global-mode 1))

(use-package counsel-projectile
  :pin melpa
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

(provide 'init-projectile)
