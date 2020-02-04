;;; init-projectile.el --- Project-wide navigation and functions

(use-package projectile
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
  (projectile-project-root-files
   '(".projectile"
     "package.json"
     "rebar.config"
     "project.clj"
     "build.boot"
     "deps.edn"
     "SConstruct"
     "pom.xml"
     "build.sbt"
     "gradlew"
     "build.gradle"
     ".ensime"
     "Gemfile"
     "requirements.txt"
     "setup.py"
     "tox.ini"
     "composer.json"
     "Cargo.toml"
     "mix.exs"
     "stack.yaml"
     "info.rkt"
     "DESCRIPTION"
     "TAGS"
     "GTAGS"
     "configure.in"
     "configure.ac"
     "cscope.out"))
  (projectile-project-root-files-functions
   '(projectile-root-local
     projectile-root-bottom-up
     projectile-root-top-down
     projectile-root-top-down-recurring))
  :config
  (setq projectile-enable-caching nil)
  (when (executable-find "fd")
    (let ((fd-command "fd . -0 -t file -t symlink --hidden --color never"))
      (setq projectile-generic-command fd-command
            projectile-git-command fd-command)))
  (projectile-register-project-type 'npm '("package.json")
                                    :compile "npm run build"
                                    :test "npm test"
                                    :test-suffix ".spec")
  (projectile-register-project-type 'webpack '("webpack.config.js package.json")
                                    :compile "webpack --env development"
                                    :test "jest"
                                    :test-suffix ".spec")
  (add-to-list 'projectile-other-file-alist
               '("js" "spec.js" "scss" "html"))
  (add-to-list 'projectile-other-file-alist
               '("spec.js" "js"))
  (add-to-list 'projectile-other-file-alist
               '("spec.ts" "ts"))
  (define-key projectile-mode-map
    (kbd "C-c p") 'projectile-command-map)
  (projectile-global-mode 1))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

(provide 'init-projectile)
