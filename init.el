(defvar default-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000)

(setq my-font-height (cond ((eq system-type 'darwin) 140)
                           ((eq system-type 'windows-nt) 110)
                           ((eq system-type 'gnu/linux) 110)))

(setq-default line-spacing 2)

(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height my-font-height
                    :width 'normal
                    :weight 'normal)

(defun emacs-d (filename)
  "Expand FILENAME relative to `user-emacs-directory'."
  (expand-file-name filename user-emacs-directory))

(defvar my-config '(
                    defaults
                    funcs
                    packages
                    macos
                    langs
                    org
                    themes
                    keybindings
                    ))

(dolist (file my-config)
  (load (emacs-d (symbol-name file))))

(load-theme 'gruvbox t)

(when (display-graphic-p)
  (toggle-frame-maximized))

;; Stop modifying init.el on me, Emacs.
;; package.el adds this line to init.el if it is not present.
;; (package-initialize)
;; Move custom variables to their own file.
(setq custom-file (emacs-d "custom.el"))
(unless (file-exists-p custom-file)
  (with-temp-buffer (write-file custom-file)))
(load custom-file)
