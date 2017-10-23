;;; keybindings.el --- Efficient keybindings with general.el

(general-define-key :states '(motion)
                    "j" 'evil-next-visual-line
                    "k" 'evil-previous-visual-line
                    "gj" 'evil-next-line
                    "gk" 'evil-previous-line)

;; Hub
(general-create-definer define-hub-key
                        :states '(normal motion)
                        :prefix "SPC")

(define-hub-key
  "SPC" 'counsel-M-x
  "TAB" 'switch-to-previous-buffer
  "/" 'swiper)

;; Buffers
(define-hub-key :infix "b"
  "b" 'switch-to-buffer
  "d" 'evil-delete-buffer)

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
  "m" 'toggle-frame-maximized
  "n" 'new-frame
  "f" 'toggle-frame-fullscreen
  "F" 'select-frame-by-name)

;; Git
(define-hub-key :infix "g"
  "b" 'magit-blame
  "c" 'magit-commit
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

;; Quit/Restart
(define-hub-key :infix "w"
  "c" 'evil-window-delete
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "s" 'evil-window-split
  "v" 'evil-window-vsplit)

;; Add hub prefixes to which-key
(setq hub-prefixes
      '(
        ("b"   . "buffers")
        ("f"   . "files")
        ("f e" . ".emacs.d")
        ("F"   . "frames")
        ("g"   . "git")
        ("h"   . "help")
        ("h d" . "describe")
        ("o"   . "org")
        ("p"   . "projectile")
        ("q"   . "quit")
        ("s"   . "search")
        ("w"   . "windows")
        ))

(dolist (pf hub-prefixes)
  (let ((prefix (concat "SPC " (car pf)))
        (description (cdr pf)))
    (which-key-add-key-based-replacements prefix description)))

;; Leader
(general-create-definer define-follower-key
                        :keymaps 'evil-normal-state-map
                        :prefix ",")

;; Would be nice to use `:major-mode' instead of `:keymaps'
(general-create-definer define-major-mode-follower-key
                        :states '(normal)
                        :prefix ",")

(define-follower-key
  "SPC" 'evil-ex-nohighlight)

;; Lisp
(define-major-mode-follower-key
  :keymaps 'lisp-mode-map
  "," 'eval-last-sexp)

;; PDF Tools
(general-define-key :keymaps 'pdf-view-mode-map
                    "j" 'pdf-view-next-line-or-next-page
                    "k" 'pdf-view-previous-line-or-previous-page)

(general-define-key :keymaps 'pdf-outline-buffer-mode-map
                    "j" 'next-line
                    "k" 'previous-line)
