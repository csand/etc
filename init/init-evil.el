;;; init-evil.el --- vi emulation mode

(use-package evil
  :ensure t
  :diminish undo-tree-mode
  :init
  (setq evil-search-module 'evil-search
        evil-want-C-w-in-emacs-state t)
  (evil-mode 1))

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

(provide 'init-evil)
