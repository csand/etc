;;; init-packages.el --- Configure package installation

(require 'package)

(setq package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/"))
      package-archive-priorities '(("org" . 10)
                                   ("melpa-stable" . 2)
                                   ("melpa" . 1))
      package-pinned-packages '((use-package . "melpa")))

(package-initialize)

;; Install use-package if it isn't installed
(unless (package-installed-p 'use-package)
  (message "Refreshing packages")
  (package-refresh-contents)
  (message "Installing use-package")
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Always install packages
(setq use-package-always-ensure t)

;; Load util libraries
(use-package s)
(use-package dash)

;; Add site-lisp packages to load-path
(mapc (lambda (site)
        (add-to-list 'load-path (expand-file-name site)))
      (-filter 'file-directory-p (csand/ls (emacs-d "site-lisp") t)))

(provide 'init-packages)
