;;; init.el --- Boot away

;; Don't garbage collect during initialization
(setq default-gc-cons-threshold gc-cons-threshold
      gc-cons-threshold most-positive-fixnum)

(defconst is-linux (eq system-type 'gnu/linux))
(defconst is-mac (eq system-type 'darwin))
(defconst is-windows (eq system-type 'windows-nt))

(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height 140
                    :width 'normal
                    :weight 'normal)

;; Set the fixed-pitch font. Mostly affects markdown mode source blocks
(set-face-attribute 'fixed-pitch nil :family "PragmataPro Mono")

;; Weirdly this variable was void at some point
(setq user-emacs-directory
      (or user-emacs-directory
          (expand-file-name ".emacs.d" (getenv "HOME"))))

;; Definitely don't want auth info in plain text
(setq-default auth-sources '("~/.authinfo.gpg"
                             "~/.authinfo"
                             "~/.netrc"))

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(add-to-list 'load-path (emacs-d "init"))

(require 'init-defaults)
(require 'init-funcs)
(require 'init-packages)
(require 'init-evil)
(require 'init-general)
(require 'init-ivy)
(require 'init-magit)
(require 'init-projectile)
(require 'init-langs)
(require 'init-org)
(require 'init-themes)
(require 'init-keybindings)
(require 'init-hub)
(require 'init-eshell)
(require 'init-prodigy)
(require 'init-extras)
(require 'init-modeline)

(load-theme 'poet t)

;; Start maximized
(when (display-graphic-p)
  (toggle-frame-maximized))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

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
