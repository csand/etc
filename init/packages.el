;;; packages.el --- Install packages with use-package.el

(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 2)
                                   ("melpa" . 1)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (message "Refreshing packages")
  (package-refresh-contents)
  (message "Installing use-package")
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)

(use-package evil
  :diminish undo-tree-mode
  :init
  (setq evil-search-module 'evil-search
        evil-want-C-w-in-emacs-state t)
  (evil-mode 1)
  )

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

(use-package evil-smartparens
  :after evil
  :diminish evil-smartparens-mode
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-height 15
        ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :config
  (when (eq system-type 'windows-nt)
    (setq counsel-ag-base-command "pt /e /nocolor /nogroup"))
  :bind
  ("M-x" . counsel-M-x)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable))

(use-package swiper
  :after ivy)

(use-package projectile
  :commands (projectile-find-file projectile-switch-project)
  :diminish projectile-mode
  :config
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien)
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "bower_components")
  (projectile-global-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-on))

(use-package magit
  :diminish auto-revert-mode
  :config
  (require 'magit-blame)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :after (evil magit))

(use-package company
  :diminish company-mode
  :defer t
  :init
  (global-company-mode)
  :config
  (add-to-list 'company-dabbrev-code-modes 'js2-mode)
  (add-to-list 'company-dabbrev-code-modes 'web-mode))

(use-package smartparens
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (add-hook 'smartparens-mode-hook #'show-smartparens-mode))

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package general)

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-popup-type 'minibuffer
        which-key-idle-delay 0.4)
  (which-key-mode 1))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package flycheck
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (add-hook 'js-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'rst-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode))

(use-package restart-emacs
  :pin melpa)

(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook (lambda () (rainbow-mode 1))))

(use-package ssh-agency)

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

(use-package telephone-line
  :pin melpa
  :config
  (setq telephone-line-height (let* ((dpi-multiplier (if (eq system-type 'windows-nt) 2 1))
                                    (whitespace-multiplier 1.4))
                                (floor (* csand-font-height whitespace-multiplier dpi-multiplier) 10))
        telephone-line-primary-left-separator 'telephone-line-cubed-right
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-left
        telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-buffer-segment)))
        telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment
                     telephone-line-airline-position-segment
                     telephone-line-simple-minor-mode-segment))
          (accent . (telephone-line-major-mode-segment))))
  (telephone-line-mode 1))

(use-package pdf-tools)

(use-package git-timemachine
  :after evil
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(use-package winum
  :init
  (winum-mode))

(use-package emmet-mode
  :commands emmet-mode
  :init
  (csand/add-to-hooks 'emmet-mode '(css-mode-hook
                                    sass-mode-hook
                                    scss-mode-hook
                                    web-mode-hook)))
