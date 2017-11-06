;;; defaults.el --- Better Emacs defaults

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1))

(column-number-mode 1)
(show-paren-mode 1)

(setq my-coding-system (if (eq system-type 'windows-nt) 'utf-8-dos 'utf-8))
(prefer-coding-system my-coding-system)
(set-default-coding-systems my-coding-system)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq
 auto-save-default nil
 compilation-scroll-output t
 create-lockfiles nil
 backup-inhibited t
 inhibit-startup-screen t
 initial-scratch-message nil)

;; Basic edit settings
(setq-default
 fill-column 80
 indent-tabs-mode nil
 tab-width 2
 truncate-lines t
 require-final-newline t)

;; Enable recent files list
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Always select the help window
(setq help-window-select t)

;; Swap out yes/no prompts for y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show buffer file name in title bar
(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

;; When saving a file with the magic byte (#!), make it executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Causes recursion error somehow
;; (add-hook 'window-configuration-change-hook #'balance-windows)

(provide 'init-defaults)
