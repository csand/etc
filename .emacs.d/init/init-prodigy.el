;;; init-prodigy.el --- Manage services with prodigy

(use-package prodigy
  :defer t)

(prodigy-define-tag
  :name 'webpack-watch
  :path '("node_modules/.bin")
  :env '(("BUILD_PATH" "/Users/samuelan/code/msx-ui/client/build/services"))
  :ready-message "Built at:"
  :truncate-output t
  :stop-signal 'sigkill)

(prodigy-define-service
  :name "msx-ui Webpack"
  :tags '(webpack-watch)
  :cwd (concat (getenv "HOME") "/code/msx-ui")
  :command "webpack"
  :args '("--watch" "--env" "development"))

(provide 'init-prodigy)
