;;; init-keybindings.el --- Efficient keybindings with general.el

(general-define-key :states '(motion)
                    "j" 'evil-next-visual-line
                    "k" 'evil-previous-visual-line
                    "gj" 'evil-next-line
                    "gk" 'evil-previous-line)

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

(provide 'init-keybindings)
