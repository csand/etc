;;; init-org.el --- Org-mode config and enhancements

(require 'init-fonts)

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
  :custom
  (org-hide-emphasis-markers t)
  (org-export-coding-system 'utf-8)
  (org-export-backends '(ascii
                         html
                         icalendar
                         latex
                         md
                         odt))
  (org-modules '(org-bbdb
                 org-bibtex
                 org-docview
                 org-eww
                 org-gnus
                 org-info
                 org-irc
                 org-mhe
                 org-rmail
                 org-tempo
                 org-w3m))
  (org-src-lang-modes '(("bash" . sh)
                        ("html" . web)
                        ("http" . ob-http)
                        ("elisp" . emacs-lisp)
                        ("javascript" . js)
                        ("sass" . sass-mode)
                        ("scss" . scss-mode)
                        ("sh" . sh)
                        ("typescript" . typescript)))
  (org-src-tab-acts-natively t)
  (org-startup-folded nil)
  (org-structure-template-alist '(("C" . "comment")
                                  ("E" . "export")
                                  ("a" . "export ascii")
                                  ("c" . "center")
                                  ("e" . "example")
                                  ("h" . "export html")
                                  ("js" . "src javascript")
                                  ("l" . "export latex")
                                  ("q" . "quote")
                                  ("s" . "src")
                                  ("ts" . "src typescript")
                                  ("v" . "verse")))
  (org-tags-column -80)
  (org-todo-keywords '((sequence "TODO(t)"
                                 "WAITING(w!)"
                                 "|"
                                 "DONE(d)"
                                 "SOMEDAY(s)"
                                 "WONTDO(n!)")))
  :custom-face
  (org-ode ((t (:family "Triplicate T4"))))
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
        org-refile-targets '((nil :maxlevel . 9)
                             (org-inbox-file :level . 1)
                             (org-projects-file :level . 1))
        org-capture-templates `(,org-todo-template
                                ,org-work-item-template))
  :config
  (define-follower-key "c" 'org-capture)
  (define-major-mode-follower-key
    :keymaps 'org-mode-map
    "," 'org-ctrl-c-ctrl-c
    ":" 'org-set-tags
    "'" 'org-edit-special
    "r" 'org-refile
    "t" 'org-todo)
  (general-define-key :keymaps 'org-mode-map
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
  (org-babel-do-load-languages 'org-babel-load-languages '((awk . t)))
  ;; NO spell check for embedded snippets
  (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
    (let* ((rlt ad-return-value)
           (begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
           (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
           (case-fold-search t)
           b e)
      (when ad-return-value
        (save-excursion
          (setq b (re-search-backward begin-regexp nil t))
          (if b (setq e (re-search-forward end-regexp nil t))))
        (if (and b e (< (point) e)) (setq rlt nil)))
      (setq ad-return-value rlt))))

(use-package ob-http
  :after org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((http . t))))

(use-package org-mime
  :pin melpa
  :after org)

(use-package ox-pandoc
  :pin melpa
  :after org)

(use-package ox-reveal :after org)

(provide 'init-org)
