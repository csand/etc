;;; org.el --- Org-mode config and enhancements

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (defun setup-org-fill ()
    (set-fill-column 80)
    (auto-fill-mode 1))
  :config
  (setq
   org-directory "~/Dropbox/Org"
   org-agenda-files (concat org-directory "/agenda.org")
   org-default-notes-file (concat org-directory "/inbox.org"))
  (setq
   org-export-coding-system 'utf-8
   org-src-tab-acts-natively t
   org-tags-column 80
   org-startup-folded nil)
  (add-to-list 'org-src-lang-modes '("javascript" . js2))
  (add-hook 'org-mode-hook #'setup-org-fill))

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package evil-org
  :after org
  :pin melpa
  :diminish evil-org-mode
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme))))
