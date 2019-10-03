;;; init-hub.el --- From the keyboard, EMACS spoke

(define-hub-key
  "!" 'shell-command
  "&" 'async-shell-command
  "/" 'swiper
  "SPC" 'counsel-M-x
  "TAB" 'switch-to-previous-buffer)

;; Winum window selection
(define-hub-key
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9
  "0" 'winum-select-window-0-or-10)

(define-hub-key :infix "a"
  "" '(nil :which-key "applications")
  "s" 'prodigy)

(define-hub-key :infix "b"
  "" '(nil :which-key "buffers")
  "b" 'switch-to-buffer
  "d" 'evil-delete-buffer
  "s" 'csand/itch)

(define-hub-key :infix "e"
  "" '(nil :which-key "errors")
  "n" 'flycheck-next-error
  "l" 'flycheck-list-errors)

(define-hub-key :infix "f"
  "" '(nil :which-key "files")
  "D" 'delete-file-and-buffer
  "f" 'counsel-find-file
  "r" 'counsel-recentf
  "R" 'rename-file-and-buffer
  "e" '(nil :which-key ".emacs.d")
  "ed" 'edit-user-init)

(define-hub-key :infix "F"
  "" '(nil :which-key "frames")
  "c" 'delete-frame
  "F" 'toggle-frame-fullscreen
  "m" 'toggle-frame-maximized
  "n" 'new-frame
  "s" 'select-frame-by-name)

(define-hub-key :infix "g"
  "" '(nil :which-key "git")
  "b" 'magit-blame
  "c" 'magit-commit
  "r" 'magit-list-repositories
  "s" 'magit-status)

(define-hub-key :infix "h"
  "" '(nil :which-key "help")
  "a" 'apropos-command)

(define-hub-key :infix "hd"
  "" '(nil :which-key "describe")
  "C" 'describe-coding-system
  "b" 'counsel-descbinds
  "f" 'counsel-describe-function
  "F" 'counsel-describe-face
  "k" 'describe-key
  "m" 'describe-mode
  "p" 'describe-package
  "s" 'counsel-info-lookup-symbol
  "v" 'counsel-describe-variable)

(define-hub-key :infix "i"
  "" '(nil :which-key "input")
  "u" 'counsel-unicode-char)

(define-hub-key :infix "n"
  "" '(nil :which-key "narrow/widen")
  "f" 'narrow-to-defun
  "r" 'narrow-to-region
  "s" 'org-narrow-to-subtree
  "w" 'widen)

(define-hub-key :infix "o"
  :global-prefix "C-c"
  "" '(nil :which-key "org")
  "a" 'org-agenda
  "c" 'org-capture
  "l" 'org-store-link)

(define-hub-key :infix "p"
  "" '(nil :which-key "projectile")
  "SPC" 'counsel-projectile
  "!" 'projectile-run-shell-command-in-root
  "&" 'projectile-run-async-shell-command-in-root
  "$" 'projectile-run-eshell
  "a" 'projectile-find-other-file
  "b" 'projectile-switch-to-buffer
  "c" 'projectile-compile-project
  "f" 'counsel-projectile-find-file
  "I" 'projectile-invalidate-cache
  "k" 'projectile-kill-buffers
  "p" 'projectile-switch-project
  "P" 'projectile-test-project
  "s" 'counsel-projectile-rg
  "t" 'projectile-toggle-between-implementation-and-test)

(define-hub-key :infix "q"
  "" '(nil :which-key "quit/restart")
  "q" 'save-buffers-kill-emacs
  "r" 'restart-emacs-and-resume
  "R" 'restart-emacs)

(define-hub-key :infix "s"
  "" '(nil :which-key "search")
  "p" 'counsel-projectile-rg
  "s" 'swiper)

(define-hub-key :infix "T"
  "" '(nil :which-key "toggles")
  "w" 'whitespace-mode)

(define-hub-key :infix "t"
  "" '(nil :which-key "themes")
  "t" 'counsel-load-theme)

(define-hub-key :infix "w"
  "" '(nil :which-key "windows")
  "1" 'delete-other-windows
  "c" 'evil-window-delete
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "s" 'evil-window-split
  "v" 'evil-window-vsplit)

(provide 'init-hub)
