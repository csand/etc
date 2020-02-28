;;; init-ivy --- Completion system and applications

(require 'init-packages)
(require 'init-general)

(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-height 18)
  (ivy-use-virtual-buffers t)
  :config
  ;; Doesn't stick if set in customizations
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :after ivy
  :general
  ("M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file
   "C-x C-r" 'counsel-recentf
   "C-h f" 'counsel-describe-function
   "C-h v" 'counsel-describe-variable)
  :config
  (when is-windows
    (setq counsel-ag-base-command "pt /e /nocolor /nogroup")))

(use-package swiper
  :after ivy
  :general
  ("C-s" 'swiper))

(use-package ivy-hydra
  :after ivy)

(provide 'init-ivy)
