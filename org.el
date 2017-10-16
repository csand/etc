;;; org.el --- Org-mode config and enhancements

(use-package org
  :mode "\\.org\\'")

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
