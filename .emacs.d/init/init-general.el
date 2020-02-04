;;; init-general.el ---

(require 'init-packages)
(require 'init-evil)

(use-package general
  :after evil
  :config
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))

  (general-override-mode 1)

  ;; Add definers for global and major mode leaders
  (general-create-definer define-follower-key
    :keymaps 'evil-normal-state-map
    :prefix ",")

  ;; Would be nice to use `:major-mode' instead of `:keymaps'
  (general-create-definer define-major-mode-follower-key
    :states '(normal)
    :prefix ",")

  ;; Used later for creating hub bindings
  (general-create-definer define-hub-key
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC"))

(provide 'init-general)
