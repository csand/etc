;;; init-extras.el --- Additional "fun"-ctionality

(require 'init-packages)
(require 'init-general)

(use-package ag :defer t)

(use-package avy
  :after ivy
  :general
  (ivy-mode-map
   "C-'" 'ivy-avy))

(use-package browse-kill-ring :defer t)

(use-package color-identifiers-mode :defer t)

(use-package company
  :defer t
  :pin melpa
  :custom
  (company-tooltip-align-annotations t)
  :init
  (global-company-mode 1)
  :config
  (add-to-list 'company-dabbrev-code-modes 'js2-mode)
  (add-to-list 'company-dabbrev-code-modes 'web-mode))

(use-package deadgrep :defer t)

(use-package direnv
  :defer t
  :config
  (direnv-mode 1))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (add-hook 'editorconfig-custom-hooks
            (lambda (props)
              (setq web-mode-attr-indent-offset nil)))
  (editorconfig-mode 1))

(use-package emmet-mode
  :hook
  (css-mode
   html-mode
   sass-mode
   scss-mode
   web-mode)
  :init
  (defun emmet-expand ()
    (interactive)
    (if (bound-and-true-p yas-minor-mode)
        (call-interactively 'emmet-expand-yas)
      (call-interactively 'emmet-expand-line)))
  :general
  ('insert emmet-mode-keymap
   "TAB" 'emmet-expand))

(use-package exec-path-from-shell
  :if is-mac
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :hook
  ((js-mode . flycheck-mode)
   (python-mode . flycheck-mode)
   (rst-mode . flycheck-mode)
   (sh-mode . flycheck-mode))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package flyspell
  :hook
  (text-mode . flyspell-mode))

(use-package git-timemachine
  :after evil
  :hook
  (git-timemachine-mode . evil-normalize-keymaps)
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal))

(use-package hydra)

(use-package iedit :defer t)

(use-package olivetti :defer t)

(use-package paradox
  :config
  (paradox-enable))

(use-package pdf-tools
  :defer t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook
  (css-mode . (lambda () (rainbow-mode 1))))

(use-package restart-emacs :pin melpa)

(use-package smartparens
  :diminish smartparens-mode
  :hook
  ((prog-mode . smartparens-mode)
   (smartparens-mode . show-smartparens-mode))
  :init
  (require 'smartparens-config))

(use-package tramp)

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

(use-package wgrep
  :commands (ivy-wgrep-change-to-wgrep-mode
             wgrep-change-to-wgrep-mode))

(use-package wgrep-ag
  :after ag)

(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 0.4)
  (which-key-popup-type 'minibuffer)
  :config
  (which-key-mode 1))

(use-package winum
  :config
  (winum-mode 1))

(use-package yasnippet
  :defer t
  :config
  (diminish 'yas-global-mode)
  (diminish 'yas-minor-mode)
  (yas-global-mode 1))

(provide 'init-extras)
