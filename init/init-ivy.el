;;; init-ivy --- Completion system and applications

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (progn
    (setq ivy-height 15
          ivy-initial-inputs-alist nil)
    (ivy-mode 1)))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (when (eq system-type 'windows-nt)
    (setq counsel-ag-base-command "pt /e /nocolor /nogroup"))
  :bind
  ("M-x" . counsel-M-x)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable))

(use-package swiper
  :ensure t
  :after ivy)

(provide 'init-ivy)
