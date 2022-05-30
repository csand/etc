;;; init-langs.el -

(require 'init-packages)

(use-package css-mode
  :mode "\\.css\\'")

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package flow-js2-mode
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (when (flow-minor-tag-present-p)
                (flow-js2-mode +1)))))

(use-package flow-minor-mode
  :config
  (add-hook 'js2-mode-hook 'flow-minor-enable-automatically))

(use-package groovy-mode
  :mode "\\.groovy\\'")

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
  (add-hook 'js2-mode-hook #'set-js2-mode-company-backends)
  (defun sand/js-copyright-comment-present-p ()
    (save-excursion
      (beginning-of-buffer)
      (when (re-search-forward " *\\*+ *Copyright" nil t)
        t)))
  (defun sand/hide-garbage-if-copyright-present ()
    (when (sand/js-copyright-comment-present-p)
      (sand/hide-garbage)))
  (add-hook 'js2-mode-hook #'sand/hide-garbage-if-copyright-present))

(use-package json-mode
  :mode "\\.json\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-command "pandoc -f markdown")
  (markdown-asymmetric-header t)
  (markdown-italic-underscore t)
  (markdown-coding-system "utf-8")
  (markdown-header-scaling t)
  (markdown-code-lang-modes
   '(("elisp" . emacs-lisp-mode)
     ("html" . web-mode)
     ("shell" . sh-mode))))

(use-package ng2-mode
  :mode (("\\.ngml" . ng2-html-mode))
  :config
  ;; Kindly piss off ng2-mode
  (setq auto-mode-alist
        (assq-delete-all-equal "\\.component.ts\\'" auto-mode-alist))
  (setq auto-mode-alist
        (assq-delete-all-equal "\\.module.ts\\'" auto-mode-alist)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package powershell
  :if is-windows
  :mode ("\\.ps[dm]?1\\'" . powershell-mode))

(use-package python-mode
  :mode "\\.py\\'")

(use-package rjsx-mode)

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

(use-package taskpaper-mode
  :mode "\\.taskpaper\\'")

(use-package typescript-mode
  :pin melpa
  :mode "\\.ts\\'"
  :hook (typescript-mode . flycheck-mode)
  :custom
  (typescript-indent-level 2))

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(provide 'init-langs)
