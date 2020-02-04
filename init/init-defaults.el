;;; init-defaults.el --- Better Emacs defaults

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(fringe-mode nil)

(column-number-mode 1)
(show-paren-mode 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq
 confirm-kill-emacs 'yes-or-no-p
 auto-save-default nil
 backup-inhibited t
 make-backup-files nil
 compilation-scroll-output t
 create-lockfiles nil
 inhibit-startup-screen t
 initial-scratch-message nil
 ring-bell-function 'ignore)

;; Basic edit settings
(setq-default
 case-fold-search nil
 fill-column 80
 indent-tabs-mode nil
 tab-width 2
 truncate-lines t
 require-final-newline t)

;; Move to trash on delete
(setq delete-by-moving-to-trash t)

;; Enable recent files list
(recentf-mode 1)
(setq recentf-max-menu-items 25)

;; Always select the help window
(setq help-window-select t)

;; Swap out yes/no prompts for y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show buffer file name in title bar
(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

(setq frame-resize-pixelwise t)

;; When saving a file with the magic byte (#!), make it executable
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Stop "Reverting buffer..." message spam
(setq auto-revert-verbose nil)

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

(provide 'init-defaults)
