;;; init-magit.el --- Don't get off the pot

(require 'init-packages)
(require 'init-evil)
(require 'init-general)

(use-package magit
  :pin melpa
  :diminish auto-revert-mode
  :config
  (require 'magit-blame)
  (setq magit-completing-read-function 'ivy-completing-read
        magit-repository-directories
        `((,(if is-windows "~/Code" "~/code") . 1)
          ("~/.emacs.d" . 0)
          ("~/etc" . 0))))

(use-package with-editor
  :general
  ('normal with-editor-mode-map
   ", a" 'with-editor-cancel
   ", c" 'with-editor-finish
   ", k" 'with-editor-cancel
   ", ," 'with-editor-finish))

(provide 'init-magit)
