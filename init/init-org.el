;;; init-org.el --- Org-mode config and enhancements

(defconst org-directory (expand-file-name "Dropbox/Org" (getenv "HOME")))

(defun org-file (filename)
  (expand-file-name filename org-directory))

(defconst org-archive-file (org-file "archive.org"))
(defconst org-inbox-file (org-file "inbox.org"))
(defconst org-projects-file (org-file "projects.org"))
(defconst org-work-file
  (org-file (concat "work-journal-" (format-time-string "%Y") ".org")))

(defconst org-todo-template
  '("t" "TODO" entry (file org-inbox-file) "* TODO %i%? %^G"))

(defconst org-work-item-template
  '("w" "Work Item" item (file+olp+datetree org-work-file)
    "- %i%?"
    :tree-type week))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :mode ("\\.org\\'" . org-mode)
  :init
  (progn
    (setq org-agenda-files `(,org-inbox-file
                             ,org-projects-file
                             ,org-work-file)
          org-export-coding-system 'utf-8
          org-refile-targets '((nil :maxlevel . 9)
                               (org-inbox-file :level . 1)
                               (org-projects-file :level . 1))
          org-src-tab-acts-natively t
          org-startup-folded nil
          org-tags-column -80)
    (setq org-capture-templates `(,org-todo-template
                                  ,org-work-item-template))
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w!)" "|" "DONE(d)" "SOMEDAY(s)" "WONTDO(n!)")))
    (add-hook 'org-mode-hook #'visual-line-mode))
  :config
  (progn
    (define-follower-key "c" 'org-capture)
    (define-major-mode-follower-key
      :keymaps 'org-mode-map
      "," 'org-ctrl-c-ctrl-c
      ":" 'org-set-tags
      "'" 'org-edit-special
      "r" 'org-refile
      "t" 'org-todo)
    (general-define-key
     :keymaps 'org-mode-map
     :states 'normal
     "t" 'org-todo)
    (define-major-mode-follower-key
      :keymaps 'org-capture-mode-map
      "," 'org-capture-finalize
      "a" 'org-capture-kill
      "c" 'org-capture-finalize
      "k" 'org-capture-kill
      "r" 'org-capture-refile
      "w" 'org-capture-refile)
    (add-to-list 'org-src-lang-modes '("javascript" . js2))))

(use-package evil-org
  :ensure t
  :after org
  :diminish evil-org-mode
  :pin melpa
  :init
  (progn
    (add-hook 'org-mode-hook #'evil-org-mode)
    (add-hook 'evil-org-mode-hook #'evil-org-set-key-theme)))

(provide 'init-org)
