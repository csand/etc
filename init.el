;; Don't garbage collect during initialization
(setq default-gc-cons-threshold gc-cons-threshold
      gc-cons-threshold most-positive-fixnum)

(defconst is-linux (eq system-type 'gnu/linux))
(defconst is-mac (eq system-type 'darwin))
(defconst is-windows (eq system-type 'windows-nt))

(defconst csand-base-font-height 120)
(defconst csand-font-height
  (if is-mac (truncate (* csand-base-font-height 1.3))
    csand-base-font-height))

(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height csand-font-height
                    :width 'normal
                    :weight 'normal)

;; Set the fixed-pitch font. Mostly affects markdown mode source blocks
(set-face-attribute 'fixed-pitch nil :family "PragmataPro Mono")

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(add-to-list 'load-path (emacs-d "init"))

(require 'init-defaults)
(require 'init-funcs)
(require 'init-packages)
(require 'init-evil)
(require 'init-ivy)
(require 'init-projectile)
(require 'init-langs)
(require 'init-org)
(require 'init-themes)
(require 'init-keybindings)
(require 'init-hub)
(require 'init-modeline)
(require 'init-eshell)

(load-theme 'zerodark t)

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
