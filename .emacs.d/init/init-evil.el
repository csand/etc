;;; init-evil.el --- vi emulation mode

(require 'init-packages)

(use-package evil
  :diminish undo-tree-mode
  :custom
  (evil-ex-substitute-global t)
  (evil-search-mode 'evil-search)
  (evil-want-C-w-in-emacs-state t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (undo-tree-auto-save-history nil)
  :config
  (evil-mode 1))

(use-package evil-collection
 :pin melpa
 :after evil
 :custom
 (evil-collection-magit-want-horizontal-movement t)
 :config
 (evil-collection-init))

(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

(use-package evil-ediff
  :after evil)

(use-package evil-org
  :pin melpa
  :after (evil org)
  :diminish evil-org-mode
  :hook
  ((org-mode . evil-org-mode)
   (evil-org-mode . evil-org-set-key-theme)))

(use-package evil-smartparens
  :after (evil smartparens)
  :diminish evil-smartparens-mode
  :hook
  (smartparens-enabled-hook . evil-smartparens-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map
                    "jk" 'evil-normal-state))

(provide 'init-evil)
