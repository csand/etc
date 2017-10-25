;;; org.el --- Org-mode config and enhancements

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-startup-folded nil
        org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . auto))))

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

(setq org-directory "~/Dropbox/Org"
      org-default-notes-file (concat org-directory "/inbox.org")
      org-agenda-files (concat org-directory "/agenda.org")
      org-export-coding-system 'utf-8
      org-src-tab-acts-natively t
      org-tags-column 80)
