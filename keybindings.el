;;; keybindings.el --- Efficient keybindings with general.el

(general-evil-setup)

;; Hub

(general-mmap
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line
 "gj" 'evil-next-line
 "gk" 'evil-previous-line)

(general-nmap
 :prefix "SPC"
 "SPC" 'counsel-M-x
 "TAB" 'switch-to-previous-buffer
 "/" 'swiper)

;; Buffers
(general-nmap
 :prefix "SPC b"
 "b" 'switch-to-buffer
 "d" 'evil-delete-buffer)

;; Files
(general-nmap
 :prefix "SPC f"
 "D" 'delete-file-and-buffer
 "f" 'counsel-find-file
 "r" 'counsel-recentf
 "R" 'rename-file-and-buffer
 "ed" 'edit-init-el)

;; Git
(general-nmap
 :prefix "SPC g"
 "b" 'magit-blame
 "c" 'magit-commit
 "s" 'magit-status)

;; Help

;; Describe
(general-nmap
 :prefix "SPC h d"
 "f" 'counsel-describe-function
 "k" 'describe-key
 "m" 'describe-mode
 "p" 'describe-package
 "v" 'counsel-describe-variable)

;; Org
(general-nmap
 :prefix "SPC o"
 :global-prefix "C-c"
 "a" 'org-agenda
 "c" 'org-capture
 "l" 'org-store-link)

;; Projectile
(general-nmap
 :prefix "SPC p"
 "!" 'projectile-run-shell-command-in-root
 "&" 'projectile-run-async-shell-command-in-root
 "b" 'projectile-switch-to-buffer
 "f" 'projectile-find-file
 "I" 'projectile-invalidate-cache
 "p" 'projectile-switch-project
 "s" 'projectile-rg)

;; Quit/Restart
(general-nmap
 :prefix "SPC q"
 "q" 'save-buffers-kill-emacs
 "r" 'restart-emacs-and-resume
 "R" 'restart-emacs)

;; Quit/Restart
(general-nmap
 :prefix "SPC w"
 "c" 'evil-window-delete
 "h" 'evil-window-left
 "j" 'evil-window-down
 "k" 'evil-window-up
 "l" 'evil-window-right
 "s" 'evil-window-split
 "v" 'evil-window-vsplit
 )

;; Tell which-key about all this
(setq hub-prefixes
      '(
        ("b"   . "buffers")
        ("f"   . "files")
        ("f e" . ".emacs.d")
        ("g"   . "git")
        ("h"   . "help")
        ("h d" . "describe")
        ("o"   . "org")
        ("p"   . "projectile")
        ("q"   . "quit")
        ("w"   . "windows")
        ))

(dolist (pf hub-prefixes)
  (let ((prefix (concat "SPC " (car pf)))
        (description (cdr pf)))
    (which-key-add-key-based-replacements prefix description)))

;; Leader

(general-nmap
 :prefix ","
 "SPC" 'evil-ex-nohighlight)
