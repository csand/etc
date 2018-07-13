;; Don't garbage collect during initialization
(setq default-gc-cons-threshold gc-cons-threshold
      gc-cons-threshold most-positive-fixnum)

(defconst is-linux (eq system-type 'gnu/linux))
(defconst is-mac (eq system-type 'darwin))
(defconst is-windows (eq system-type 'windows-nt))

(defconst csand-base-font-height 130)
(defconst csand-font-height
  (if is-mac (truncate (* csand-base-font-height 1.1))
    csand-base-font-height))

(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height csand-font-height
                    :width 'normal
                    :weight 'normal)

;; Set the fixed-pitch font. Mostly affects markdown mode source blocks
(set-face-attribute 'fixed-pitch nil :family "PragmataPro Mono")

(defun split-window-sensibly-prefer-horizontal (&optional window)
  "Similar to `split-window-sensibly' except it tries to split horizontally
before trying vertically. See `split-window-sensibly' for more details."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it vertically disregarding
             ;; the value of `split-height-threshold'.
             (let ((split-height-threshold 0))
               (when (window-splittable-p window)
                 (with-selected-window window
                   (split-window-below))))))))

(setq split-window-preferred-function #'split-window-sensibly-prefer-horizontal
      split-width-threshold 150)

;; Disable alarm bell
(setq ring-bell-function 'ignore)

;; Weirdly this variable was void at some point
(setq user-emacs-directory
      (or user-emacs-directory (expand-file-name ".emacs.d" (getenv "HOME"))))

;; Definitely don't want auth info in plain text
(setq-default auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

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

(load-theme 'poet t)

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
