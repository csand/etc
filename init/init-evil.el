;;; init-evil.el --- vi emulation mode

(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq evil-search-module 'evil-search
        evil-want-C-w-in-emacs-state t
        evil-want-integration nil)
  :config
  (evil-mode 1))

(use-package evil-collection
 :ensure t
 :pin melpa
 :after evil
 :config
 (evil-collection-init))

(use-package evil-ediff
  :ensure t
  :after evil)

(use-package evil-commentary
  :ensure t
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-smartparens
  :ensure t
  :after (evil smartparens)
  :diminish evil-smartparens-mode
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package key-chord
  :ensure t
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(provide 'init-evil)
