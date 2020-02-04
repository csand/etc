;;; init-themes.el --- Whatever themes I feel like installing

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(use-package solarized-theme
  :defer t
  :custom
  (solarized-use-variable-pitch nil)
  (solarized-high-contrast-mode-line nil)
  (solarized-use-less-bold t)
  (solarized-use-more-italic t))

(use-package nord-theme
  :defer t
  :init
  (setq nord-comment-brightness 20)
  (setq nord-uniform-mode-lines t))

;; Bundles
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package doom-themes
  :defer t
  :custom
  (doom-challenger-deep-brighter-comments t))
(use-package gruvbox-theme :defer t) ;; grubox-light-medium +1
(use-package kaolin-themes :defer t) ;; kaolin-galaxy +1
(use-package leuven-theme :defer t)
(use-package majapahit-theme :defer t)
(use-package material-theme :defer t) ;; material-light +1
(use-package poet-theme :defer t)

;; Light
(use-package cloud-theme :defer t) ;; +1
(use-package flatui-theme :defer t)
(use-package greymatters-theme :defer t)
(use-package oldlace-theme :defer t)
(use-package parchment-theme :pin melpa :defer t)
(use-package silkworm-theme :defer t)
(use-package twilight-bright-theme :defer t) ;; Interesting highlighting
(use-package white-sand-theme :defer t)

;; Dark
(use-package arjen-grey-theme :defer t)
(use-package atom-one-dark-theme :defer t)
(use-package badwolf-theme :defer t)
(use-package dakrone-theme :defer t) ;; Good, but I hate the default gutter
(use-package dracula-theme :defer t)
(use-package naysayer-theme :defer t)
(use-package oceanic-theme :defer t)
(use-package omtose-phellack-theme :defer t)
(use-package planet-theme :defer t)
(use-package railscasts-reloaded-theme :defer t)
(use-package subatomic-theme :defer t)
(use-package zerodark-theme :defer t)

;; Monochromatic
(use-package eink-theme :defer t)
(use-package goose-theme :defer t) ;; Super grey
(use-package monotropic-theme :defer t)
(use-package plain-theme :defer t)
(use-package punpun-theme :defer t)
(use-package tao-theme :defer t)

;; Crazy
(use-package cyberpunk-2019-theme :defer t) ;; Modelines are unreadable

(provide 'init-themes)
