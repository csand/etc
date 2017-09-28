;; Use pt for helm-ag
(setq-default helm-ag-base-command "pt -e --nocolor --nogroup")

;; JS and JSON indent levels
(setq-default js-indent-level 2
              js2-basic-offset 2)

;; Disable email address recognition in javascript and JSON
(add-hook 'js2-mode-hook  (lambda () (goto-address-mode -1)))
(add-hook 'json-mode-hook (lambda () (goto-address-mode -1)))
