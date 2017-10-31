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
  "!" 'shell-command
  "&" 'async-shell-command
  "/" 'swiper
  "SPC" 'counsel-M-x
  "TAB" 'switch-to-previous-buffer)

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

;; Windows
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
  :keymaps '(emacs-lisp-mode-map
             lisp-interaction-mode-map)
  "," 'eval-last-sexp)

;; PDF Tools
(general-define-key :keymaps 'pdf-view-mode-map
                    "j" 'pdf-view-next-line-or-next-page
                    "k" 'pdf-view-previous-line-or-previous-page)

(general-define-key :keymaps 'pdf-outline-buffer-mode-map
                    "j" 'next-line
                    "k" 'previous-line)

;; Org-mode
(general-define-key
 :keymaps 'org-mode-map
 :states 'normal
 "t" 'org-todo)

(define-major-mode-follower-key
  :keymaps 'org-mode-map
  "," 'org-ctrl-c-ctrl-c)

;; Emmet
(general-define-key
 :keymaps '(css-mode-map web-mode-map)
 :states 'insert
 "TAB" 'emmet-expand-line
 )
