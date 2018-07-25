;;; init-themes.el --- Whatever themes I feel like installing

(use-package darktooth-theme
  :ensure t
  :defer t)

(use-package dracula-theme
  :ensure t
  :defer t)

(use-package gruvbox-theme
  :ensure t
  :defer t)

(use-package leuven-theme
  :ensure t
  :defer t)

(use-package moe-theme
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :defer t
  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-high-contrast-mode-line nil)
  (setq solarized-use-less-bold t)
  (setq solarized-use-more-italic nil))

(use-package flatui-theme
  :ensure t
  :defer t)

(use-package tao-theme
  :ensure t
  :defer t)

(use-package material-theme
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package afternoon-theme
  :ensure t
  :defer t)

(use-package atom-one-dark-theme
  :ensure t
  :defer t)

(use-package nord-theme
  :ensure t
  :defer t
  :init
  (setq nord-comment-brightness 20))

(use-package sublime-themes
  :ensure t
  :defer t)

(use-package zerodark-theme
  :ensure t
  :defer t)

(use-package challenger-deep-theme
  :ensure t
  :defer t)

(use-package majapahit-theme
  :ensure t
  :defer t)

(use-package arjen-grey-theme
  :ensure t
  :defer t)

(use-package goose-theme
  :ensure t
  :defer t)

(use-package oldlace-theme
  :ensure t
  :defer t)

(use-package greymatters-theme
  :ensure t
  :defer t)

(use-package poet-theme
  :ensure t
  :defer t)

(provide 'init-themes)
