;;; init-modeline.el --- Personal modeline setup

(require 'init-extras)

(setq ns-use-srgb-colorspace nil)

(use-package minions
  :config (minions-mode 1))

(provide 'init-modeline)
