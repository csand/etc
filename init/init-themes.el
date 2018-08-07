;;; init-themes.el --- Whatever themes I feel like installing

(use-package darktooth-theme :defer t)

(use-package dracula-theme :defer t)

(use-package gruvbox-theme :defer t)

(use-package leuven-theme :defer t)

(use-package moe-theme :defer t)

(use-package solarized-theme
  :defer t
  :custom
  (solarized-use-variable-pitch nil)
  (solarized-high-contrast-mode-line nil)
  (solarized-use-less-bold t)
  (solarized-use-more-italic nil))

(use-package flatui-theme :defer t)

(use-package tao-theme :defer t)

(use-package material-theme :defer t)

(use-package color-theme-sanityinc-tomorrow :defer t)

(use-package afternoon-theme :defer t)

(use-package atom-one-dark-theme :defer t)

(use-package nord-theme
  :defer t
  :init
  (setq nord-comment-brightness 20))

(use-package sublime-themes :defer t)

(use-package zerodark-theme :defer t)

(use-package challenger-deep-theme :defer t)

(use-package majapahit-theme :defer t)

(use-package arjen-grey-theme :defer t)

(use-package goose-theme :defer t)

(use-package oldlace-theme :defer t)

(use-package greymatters-theme :defer t)

(use-package poet-theme :defer t)

(provide 'init-themes)
