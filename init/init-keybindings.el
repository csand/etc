;;; init-keybindings.el --- Efficient keybindings with general.el

(require 'init-general)

;; Configure macOS modifiers
(when (mac?)
  (setq ns-command-modifier 'meta
        ns-option-modifier 'alt))

(general-define-key :keymaps 'motion
                    "j" 'evil-next-visual-line
                    "k" 'evil-previous-visual-line
                    "gj" 'evil-next-line
                    "gk" 'evil-previous-line)

(define-follower-key
  "SPC" 'evil-ex-nohighlight)

;; Lisp
(define-major-mode-follower-key
  :keymaps '(emacs-lisp-mode-map
             lisp-interaction-mode-map)
  "," 'eval-last-sexp)

;; Emmet
(general-define-key
 :states 'insert
 :keymaps '(css-mode-map web-mode-map)
 "TAB" 'emmet-expand-line)

;; Magit Repolist
(general-define-key
 :state 'normal
 :keymaps 'magit-repolist-mode-map
 "RET" 'magit-repolist-status)

(provide 'init-keybindings)
