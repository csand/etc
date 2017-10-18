;;; macos.el --- Special settings for macOS

(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize))
  (setq dired-use-ls-dired nil))

;; Enable the ligature support in railwaycat's Emacs port
;;(mac-auto-operator-composition-mode)
