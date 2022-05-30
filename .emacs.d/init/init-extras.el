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
  (direnv-mode 1)
  (defvar after-direnv-update-hook nil
    "Hook run after a `direnv-update-directory-environment' has finished.")
  (defadvice direnv-update-directory-environment (after run-after-direnv-update-hook activate)
    "Run `after-direnv-update-hook'."
    (run-hooks 'after-direnv-update-hook))
  (defun update-eshell-path-from-exec-path ()
    (interactive)
    (setq eshell-path-env (s-join ":" exec-path)))
  (add-hook 'after-direnv-update-hook #'update-eshell-path-from-exec-path))

(use-package dtrt-indent
  :defer t
  :diminish dtrt-indent-mode
  :config
  (dtrt-indent-global-mode 1))

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
  :if (mac?)
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :pin melpa
  :commands (flycheck-mode)
  :hook
  ((js-mode . flycheck-mode)
   (python-mode . flycheck-mode)
   (rst-mode . flycheck-mode)
   (sh-mode . flycheck-mode))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package flyspell
  :hook (text-mode . flyspell-mode)
  :config
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--run-together")))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"))))

(use-package git-timemachine :defer t)

(use-package htmlize :defer t)

(use-package hydra)

(use-package iedit :defer t)

(use-package lorem-ipsum :defer t)

(use-package md4rd
  :defer t
  :pin melpa
  :config
  (setq md4rd-subs-active '(emacs elderscrollsonline))
  (add-to-list 'evil-emacs-state-modes 'md4rd-mode))

(use-package nov
  :defer t
  :pin melpa
  :mode ("\\.epub\\'" . nov-mode)
  :config (setq nov-text-width 80))

(use-package olivetti :defer t)

(use-package paradox
  :config (paradox-enable))

(use-package pdf-tools
  :defer t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook (css-mode . (lambda () (rainbow-mode 1))))

(use-package restart-emacs :pin melpa)

(use-package restclient :pin melpa)

(use-package shackle
  :defer t
  :custom
  (shackle-rules
   '((compilation-mode
      :select nil
      :popup t
      :align below
      :size 15)
     ))
  :config
  (shackle-mode 1))

(use-package slime
  :pin melpa-stable
  :config
  (setq slime-default-lisp 'clisp
        slime-lisp-implementations '((clisp ("clisp"))
                                     )
        slime-contribs '(slime-fancy)))

(use-package smartparens
  :diminish smartparens-mode
  :hook
  ((prog-mode . smartparens-mode)
   (conf-mode . smartparens-mode)
   (smartparens-mode . show-smartparens-mode))
  :init
  (require 'smartparens-config))

(use-package super-save
  :diminish super-save-mode
  :config
  (super-save-mode 1))

(use-package tide
  :pin melpa
  :commands (tide-setup)
  :hook (typescript-mode . (lambda () (tide-setup)))
  :config
  ;; (setq tide-tsserver-process-environment
  ;;       '("TSS_LOG=-level verbose -file /tmp/tss.log -traceToConsole true"))
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (tide-setup)))))

(use-package tramp)

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

(use-package web-beautify
  :pin melpa
  :defer t)

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
