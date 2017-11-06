;; Don't garbage collect during initialization
(setq default-gc-cons-threshold gc-cons-threshold
      gc-cons-threshold most-positive-fixnum)

(setq csand-base-font-height 110
      csand-font-height
      (cond ((eq system-type 'darwin) (* csand-base-font-height-base-font-height 1.4))
            ((eq system-type 'windows-nt) csand-base-font-height)
            ((eq system-type 'gnu/linux) csand-base-font-height)))

(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height csand-font-height
                    :width 'normal
                    :weight 'normal)

(set-face-attribute 'fixed-pitch nil :family "PragmataPro Mono")

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(add-to-list 'load-path (emacs-d "init"))

(require 'init-defaults)
(require 'init-funcs)
(require 'init-packages)
(require 'init-langs)
(require 'init-org)
(require 'init-themes)
(require 'init-keybindings)

(load-theme 'gruvbox t)

;; Start maximized
(when (display-graphic-p)
  (toggle-frame-maximized))

;; Enable the ligature support in railwaycat's Emacs port
;; (when (eq window-system 'mac)
;;   (mac-auto-operator-composition-mode))

;; Stop modifying init.el on me, Emacs.
;; package.el adds this line to init.el if it is not present.
;; (package-initialize)
;; Move custom variables to their own file.
(setq custom-file (emacs-d "custom.el"))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)

;; Reset to default gc threshold
(setq gc-cons-threshold default-gc-cons-threshold)
