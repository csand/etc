;;; funcs.el --- Useful functions that should probably be in Emacs by default

(defun edit-init-el ()
  "Edit user init file."
  (interactive)
  (find-file user-init-file))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))

(defun delete-file-and-buffer ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when (and filename (y-or-n-p "Are you sure you want to delete this file?"))
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun restart-emacs-and-resume ()
  "Restart emacs with the experimental restore frame option."
  (interactive)
  (setq restart-emacs-restore-frames t)
  (restart-emacs))
