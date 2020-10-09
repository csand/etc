;;; init-lsp.el --- Configure language server protocol packages

(use-package lsp-mode
  :pin melpa
  :commands (lsp lsp-deferred))

;; (use-package lsp-ui
;;   :commands (lsp-ui-mode))

(use-package company-lsp
  :after (company lsp)
  :commands (company-lsp)
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ivy
  :after lsp-mode
  :commands (lsp-ivy-workspace-symbol))

(provide 'init-lsp)
