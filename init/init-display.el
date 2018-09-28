;;; init-display.el ---

(setq display-buffer-alist nil)

(defmacro sidebar-buffers (for mode on side default-size size)
  `(add-to-list 'display-buffer-alist
                '((lambda (buffer &optional action)
                    (eq (quote ,mode) (buffer-local-value 'major-mode (get-buffer buffer))))
                  (display-buffer-reuse-window
                   display-buffer-in-side-window)
                  (side . ,side)
                  (slot . 0)
                  (window-width . ,size)
                  (window-height . ,size)
                  )))

;; (defmacro display-rule (regexp-or-mode &optional args)
;;   `(let* ((args* '(,args))
;;           (condition
;;            (if (stringp regexp-or-mode)
;;                regexp-or-mode
;;              (lambda (buffer &optional action)
;;                (eq ,regexp-or-mode (buffer-local-value 'major-mode (get-buffer buffer))))
;;              )))
;;      (add-to-list 'display-buffer-alist
;;                   '(condition
;;                     (display-buffer-reuse-window
;;                      display-buffer-in-side-window)
;;                     (side . (plist-get args* :side))))
;;      )
;;   )

;; (display-rule 'help-mode
;;               :side right
;;               :size 78)

(sidebar-buffers for help-mode on right size 78)
;; (sidebar-buffers for magit-status-mode on top size 20)
(sidebar-buffers for compilation-mode on bottom size 15)

(provide 'init-display)
