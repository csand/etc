;;; languages.el --- Packages and config for language major modes

(use-package css-mode)

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-mode-show-strict-warnings nil))

(use-package json-mode
  :mode "\\.json\\'")

(use-package markdown-mode
  :mode "\\.md\\'")

(when (eq system-type 'windows-nt)
  (use-package powershell
    :mode ("\\.ps[dm]?1\\'" . powershell-mode)))

(use-package python-mode
  :mode "\\.py\\'")

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (use-package racer
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (setq company-tooltip-align-annotations t)
    :bind
    (:map rust-mode-map
          ("TAB" . company-indent-or-complete-common))))

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

(use-package web-mode
  :mode "\\.html\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(provide 'init-langs)
