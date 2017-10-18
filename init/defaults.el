;;; defaults.el --- Better Emacs defaults

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1))

(auto-save-mode -1)

(column-number-mode 1)
(show-paren-mode 1)

(prefer-coding-system 'utf-8)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq
 compilation-scroll-output t
 create-lockfiles nil
 backup-inhibited t
 inhibit-startup-screen t
 initial-scratch-message nil)

(setq-default
 fill-column 80
 indent-tabs-mode nil
 tab-width 2
 truncate-lines t
 require-final-newline t)

(recentf-mode 1)
(setq recentf-max-menu-items 100)

;; Swap out yes/no prompts for y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; When saving a file with the magic byte (#!), make it executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Causes recursion error somehow
;; (add-hook 'window-configuration-change-hook #'balance-windows)
