;;; init-packages.el --- Configure package installation

(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '(("melpa-stable" . 2)
                                   ("melpa" . 1)))

(package-initialize)

;; Install use-package if it isn't installed
(unless (package-installed-p 'use-package)
  (message "Refreshing packages")
  (package-refresh-contents)
  (message "Installing use-package")
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Load util libraries
(use-package s :ensure t)
(use-package dash :ensure t)

;; Add site-lisp packages to load-path
(mapc (lambda (site)
        (add-to-list 'load-path (expand-file-name site)))
      (-filter 'file-directory-p (csand/ls (emacs-d "site-lisp") t)))

(use-package magit
  :ensure t
  :diminish auto-revert-mode
  :config
  (require 'magit-blame)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :ensure t
  :after (evil magit))

(use-package company
  :ensure t
  ;; :diminish company-mode
  :defer t
  :init
  (global-company-mode)
  :config
  (add-to-list 'company-dabbrev-code-modes 'js2-mode)
  (add-to-list 'company-dabbrev-code-modes 'web-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (add-hook 'smartparens-mode-hook #'show-smartparens-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package general
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-popup-type 'minibuffer
        which-key-idle-delay 0.4)
  (which-key-mode 1))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (add-hook 'js-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'rst-mode-hook 'flycheck-mode)
  (add-hook 'sh-mode-hook 'flycheck-mode))

(use-package restart-emacs
  :pin melpa
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (add-hook 'css-mode-hook (lambda () (rainbow-mode 1))))

(use-package ssh-agency
  :ensure t)

(use-package exec-path-from-shell
  :if is-mac
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package telephone-line
  :pin melpa
  :ensure t
  :config
  (progn
    (setq
     telephone-line-height
     (let* ((dpi-multiplier (if (eq system-type 'windows-nt) 2 1))
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
    ;; (telephone-line-mode 1)
    ))

(use-package pdf-tools
  :ensure t)

(use-package git-timemachine
  :ensure t
  :after evil
  :config
  (progn
    (evil-make-overriding-map git-timemachine-mode-map 'normal)
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)
    ))

(use-package winum
  :ensure t
  :config
  (winum-mode 1))

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  (progn
    (setq emmet-enabled-modes '(css-mode-hook
                                html-mode-hook
                                sass-mode-hook
                                scss-mode-hook
                                web-mode-hook))
    (csand/add-to-hooks 'emmet-mode emmet-enabled-modes))
  :config
  (progn
    (defun emmet-expand ()
      (interactive)
      (if (bound-and-true-p yas-minor-mode)
          (call-interactively 'emmet-expand-yas)
        (call-interactively 'emmet-expand-line)))
    (general-define-key
     :keymaps emmet-mode-keymap
     :states 'insert
     "TAB" 'emmet-expand)
    ))

(use-package yasnippet
  :ensure t
  :config
  (diminish 'yas-global-mode)
  (diminish 'yas-minor-mode)
  (yas-global-mode 1))

(use-package visual-fill-column
  :ensure t
  :commands visual-fill-column-mode
  :init
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(use-package hydra
  :ensure t)

(provide 'init-packages)
