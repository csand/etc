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
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

(use-package evil-smartparens
  :diminish evil-smartparens-mode
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package counsel
  :config
  (when (eq system-type 'windows-nt)
    (setq counsel-ag-base-command "pt /e /nocolor /nogroup"))
  :bind
  ("M-x" . counsel-M-x)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable))

(use-package swiper)

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
  :config
  (counsel-projectile-on))

(use-package magit
  :diminish auto-revert-mode
  :config
  (require 'magit-blame)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit)

(use-package company
  :diminish company-mode
  :defer t
  :init
  (global-company-mode))

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

(use-package flycheck)

(use-package restart-emacs)

(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook (lambda () (rainbow-mode 1))))

(use-package ssh-agency)
