;;; init-themes.el --- Whatever themes I feel like installing

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(use-package poet-theme
  :defer t
  :config
  (defun customize-poet-theme ()
    (if (member 'poet custom-enabled-themes)
        (let ((line-color (face-attribute 'mode-line :overline)))
          (set-face-underline 'mode-line line-color)
          ;; (set-face-attribute 'mode-line nil :box nil)
          ;; (set-face-attribute 'mode-line-inactive nil :box nil)
          ;; (set-face-background 'mode-line-inactive "#bdb7a8")
          (set-face-underline 'mode-line-buffer-id nil))))
  (add-hook 'after-load-theme-hook 'customize-poet-theme)
  ;; (defun customize-poet-dark-monochrome-theme ()
  ;;   (if (member 'poet-dark-monochrome custom-enabled-themes)
  ;;       (let ((line-color (face-attribute 'mode-line :overline)))
  ;;         (set-face-underline 'mode-line line-color)
  ;;         (set-face-underline 'mode-line-buffer-id nil))))
  ;; (add-hook 'after-load-theme-hook 'customize-poet-dark-monochrome-theme)
  (defun customize-poet-monochrome-theme ()
    (if (member 'poet-monochrome custom-enabled-themes)
        (let ((line-color (face-attribute 'mode-line :overline)))
          (set-face-underline 'mode-line line-color)
          (set-face-underline 'mode-line-buffer-id nil))))
  (add-hook 'after-load-theme-hook 'customize-poet-monochrome-theme))

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

(use-package afternoon-theme :defer t)
(use-package ample-theme :defer t)
(use-package arjen-grey-theme :defer t)
(use-package atom-one-dark-theme :defer t)
(use-package autumn-light-theme :defer t)
(use-package badwolf-theme :defer t)
(use-package birds-of-paradise-plus-theme :defer t)
(use-package challenger-deep-theme :defer t)
(use-package cloud-theme :defer t) ;; Pretty good
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package cyberpunk-2019-theme :defer t)
(use-package dakrone-theme :defer t)
(use-package darktooth-theme :defer t)
(use-package doom-themes :defer t)
(use-package dracula-theme :defer t)
(use-package eink-theme :defer t)
(use-package flatui-theme :defer t)
(use-package goose-theme :defer t) ;; Super grey
(use-package grayscale-theme :defer t)
(use-package greymatters-theme :defer t)
(use-package gruvbox-theme :defer t) ;; grubox-light-medium +1
(use-package kaolin-themes :defer t)
(use-package leuven-theme :defer t)
(use-package majapahit-theme :defer t)
(use-package material-theme :defer t) ;; material-light +1
(use-package moe-theme :defer t)
(use-package monotropic-theme :defer t)
(use-package naysayer-theme :defer t)
(use-package nordless-theme :defer t)
(use-package oceanic-theme :defer t)
(use-package oldlace-theme :defer t)
(use-package omtose-phellack-theme :defer t)
(use-package parchment-theme :defer t)
(use-package plain-theme :defer t)
(use-package planet-theme :defer t)
(use-package punpun-theme :defer t)
(use-package railscasts-reloaded-theme :defer t)
(use-package silkworm-theme :defer t)
(use-package soft-morning-theme :defer t)
(use-package srcery-theme :defer t :pin melpa)
(use-package subatomic-theme :defer t)
(use-package sublime-themes :defer t)
(use-package tao-theme :defer t)
(use-package twilight-bright-theme :defer t) ;; Interesting highlighting
(use-package warm-night-theme :defer t)
(use-package white-sand-theme :defer t)
(use-package zerodark-theme :defer t)

(provide 'init-themes)
