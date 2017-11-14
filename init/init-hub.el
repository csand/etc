;;; init-hub.el ---

;; Tell which-key about the hub
(setq hub-prefixes
      '(
        ("b"   . "buffers")
        ("F"   . "frames")
        ("f"   . "files")
        ("f e" . ".emacs.d")
        ("g"   . "git")
        ("h"   . "help")
        ("h d" . "describe")
        ("n"   . "narrow/widen")
        ("o"   . "org")
        ("p"   . "projectile")
        ("q"   . "quit")
        ("s"   . "search")
        ("T"   . "toggles")
        ("t"   . "themes")
        ("w"   . "windows")
        ))

(dolist (pf hub-prefixes)
  (let ((prefix (concat "SPC " (car pf)))
        (description (cdr pf)))
    (which-key-add-key-based-replacements prefix description)))

(general-create-definer define-hub-key
                        :states '(normal motion)
                        :prefix "SPC")

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

;; Buffers
(define-hub-key :infix "b"
  "b" 'switch-to-buffer
  "d" 'evil-delete-buffer)

;; Errors
(define-hub-key :infix "e"
  "n" 'flycheck-next-error
  "l" 'flycheck-list-errors)

;; Files
(define-hub-key :infix "f"
  "D" 'delete-file-and-buffer
  "f" 'counsel-find-file
  "r" 'counsel-recentf
  "R" 'rename-file-and-buffer
  "ed" 'edit-init-el)

;; Frames
(define-hub-key :infix "F"
  "c" 'delete-frame
  "F" 'toggle-frame-fullscreen
  "m" 'toggle-frame-maximized
  "n" 'new-frame
  "s" 'select-frame-by-name)

;; Git
(define-hub-key :infix "g"
  "b" 'magit-blame
  "c" 'magit-commit
  "r" 'magit-list-repositories
  "s" 'magit-status)

;; Help
(define-hub-key :infix "h"
  "a" 'apropos-command)

;; Describe
(define-hub-key :infix "hd"
  "C" 'describe-coding-system
  "f" 'counsel-describe-function
  "F" 'counsel-describe-face
  "k" 'describe-key
  "m" 'describe-mode
  "p" 'describe-package
  "s" 'counsel-info-lookup-symbol
  "v" 'counsel-describe-variable)

;; Input
(define-hub-key :infix "i"
  "u" 'counsel-unicode-char)

;; Narrow/Widen
(define-hub-key :infix "n"
  "s" 'org-narrow-to-subtree)

;; Org
(define-hub-key :infix "o"
  :global-prefix "C-c"
  "a" 'org-agenda
  "c" 'org-capture
  "l" 'org-store-link)

;; Projectile
(define-hub-key :infix "p"
  "!" 'projectile-run-shell-command-in-root
  "&" 'projectile-run-async-shell-command-in-root
  "b" 'projectile-switch-to-buffer
  "f" 'projectile-find-file
  "I" 'projectile-invalidate-cache
  "k" 'projectile-kill-buffers
  "p" 'projectile-switch-project
  "s" 'counsel-projectile-rg)

;; Quit/Restart
(define-hub-key :infix "q"
  "q" 'save-buffers-kill-emacs
  "r" 'restart-emacs-and-resume
  "R" 'restart-emacs)

;; Search
(define-hub-key :infix "s"
  "p" 'counsel-projectile-rg
  "s" 'swiper)

;; Toggles
(define-hub-key :infix "T"
  "w" 'whitespace-mode)

;; Themes
(define-hub-key :infix "t"
  "t" 'counsel-load-theme)

;; Windows
(define-hub-key :infix "w"
  "c" 'evil-window-delete
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "s" 'evil-window-split
  "v" 'evil-window-vsplit)

(provide 'init-hub)
