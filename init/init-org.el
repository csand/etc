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
  :custom-face
  (org-document-title ((t (:family "Triplicate C4"))))
  (org-level-1 ((t (:family "Triplicate C4"))))
  (org-level-2 ((t (:family "Triplicate C4"))))
  (org-level-3 ((t (:family "Triplicate C4"))))
  (org-level-4 ((t (:family "Triplicate C4"))))
  (org-level-5 ((t (:family "Triplicate C4"))))
  (org-level-6 ((t (:family "Triplicate C4"))))
  (org-level-7 ((t (:family "Triplicate C4"))))
  (org-level-8 ((t (:family "Triplicate C4"))))
  :init
  (setq org-agenda-files `(,org-inbox-file
                           ,org-projects-file
                           ,org-work-file)
        org-export-coding-system 'utf-8
        org-refile-targets '((nil :maxlevel . 9)
                             (org-inbox-file :level . 1)
                             (org-projects-file :level . 1))
        org-src-tab-acts-natively t
        org-startup-folded nil
        org-tags-column -80
        org-hide-emphasis-markers t)
  (setq org-capture-templates `(,org-todo-template
                                ,org-work-item-template))
  (setq org-todo-keywords '((sequence
                             "TODO(t)"
                             "WAITING(w!)"
                             "|"
                             "DONE(d)"
                             "SOMEDAY(s)"
                             "WONTDO(n!)")))
  :config
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
  (add-to-list 'org-src-lang-modes '("javascript" . js2))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((awk . t)))
  (set-face-attribute 'org-code nil
                      :family "Triplicate T4")
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist
               '("js" . "src javascript")))

(use-package ob-http
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((http .t))))

(use-package org-mime
  :pin melpa
  :after org)

(use-package ox-pandoc
  :pin melpa
  :after org)

(provide 'init-org)
