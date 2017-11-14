;;; init-eshell.el --- The best shell is lisp

(use-package eshell
  :init
  (progn
    (setq
     eshell-hist-ignoredups t
     eshell-save-history-on-exit t
     eshell-prefer-lisp-functions t
     eshell-destroy-buffer-when-process-dies t)))

;; Visual Commands
(add-hook 'eshell-mode-hook
              (lambda ()
                (add-to-list 'eshell-visual-commands "ssh")
                (add-to-list 'eshell-visual-commands "tail")
                (add-to-list 'eshell-visual-commands "top")))

;; Aliases
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "ff" "find-file $1")
            (eshell/alias "emacs" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")))

(provide 'init-eshell)
