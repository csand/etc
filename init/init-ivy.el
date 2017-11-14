;;; init-ivy --- Completion system and applications

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :config
  (progn
    (setq ivy-count-format "(%d/%d) "
          ivy-height 18
          ivy-initial-inputs-alist nil
          ivy-use-virtual-buffers t)))

(use-package counsel
  :ensure t
  :after ivy
  :bind*
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable))
  :config
  (when is-windows
    (setq counsel-ag-base-command "pt /e /nocolor /nogroup")))

(use-package swiper
  :ensure t
  :after ivy
  :bind*
  (("C-s" . swiper)))

(use-package ivy-hydra
  :ensure t
  :after ivy)

(provide 'init-ivy)
