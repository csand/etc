;;; init-langs.el -

(require 'init-packages)

(use-package css-mode
  :mode "\\.css\\'")

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package js2-mode
  :mode "\\.js\\'"
  :custom
  (js2-mode-show-strict-warnings nil)
  (js2-basic-offset 2)
  :custom-face
  ;; Unset default jsdoc colours since a lot of themes miss these
  (js2-jsdoc-tag ((t (:foreground nil))))
  (js2-jsdoc-type ((t (:foreground nil))))
  (js2-jsdoc-value ((t (:foreground nil))))
  (js2-function-param ((t (:foreground nil))))
  :config
  (defun set-js2-mode-company-backends ()
    (set (make-local-variable 'company-backends)
         '(company-dabbrev-code company-yasnippet)))
  (add-hook 'js2-mode-hook #'set-js2-mode-company-backends))

(use-package json-mode
  :mode "\\.json\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-command "pandoc -f markdown")
  (markdown-asymmetric-header t)
  (markdown-italic-underscore t)
  (markdown-coding-system "utf-8")
  (markdown-header-scaling t))

(use-package ng2-mode
  :mode (("\\.ngml" . ng2-html-mode))
  :config
  ;; Kindly piss off ng2-mode
  (setq auto-mode-alist
        (assq-delete-all-equal "\\.component.ts\\'" auto-mode-alist)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package powershell
  :if is-windows
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))

(use-package python-mode
  :mode "\\.py\\'")

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (use-package racer
    :hook
    ((rust-mode . racer-mode)
     (racer-mode . eldoc-mode)
     (racer-mode . company-mode))
    :general
    ('insert rust-mode-map
     "TAB" 'company-indent-or-complete-common)))

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

(use-package taskpaper-mode
  :mode "\\.taskpaper\\'")

(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level 2))

(use-package web-mode
  :mode "\\.html\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(provide 'init-langs)
