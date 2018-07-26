;;; init-langs.el --- Language major modes

(use-package css-mode
  :ensure t)

(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'")

(use-package jinja2-mode
  :ensure t
  :mode "\\.j2\\'")

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-mode-show-strict-warnings nil)
  (defun set-js2-mode-company-backends ()
    (set (make-local-variable 'company-backends)
         '(company-dabbrev-code company-yasnippet)))
  (add-hook 'js2-mode-hook #'set-js2-mode-company-backends))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "pandoc -f markdown"
        markdown-asymmetric-header t
        markdown-italic-underscore t
        markdown-coding-system "utf-8"))

(use-package powershell
  :if is-windows
  :ensure t
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))

(use-package python-mode
  :ensure t
  :mode "\\.py\\'")

(use-package rust-mode
  :ensure t
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
  :ensure t
  :mode "\\.sass\\'")

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

(use-package taskpaper-mode
  :ensure t
  :mode "\\.taskpaper\\'")

(use-package web-mode
  :ensure t
  :mode "\\.html\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(provide 'init-langs)
