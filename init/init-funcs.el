;;; init-funcs.el --- Useful functions that should probably be in Emacs by default

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

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun csand/add-to-hooks (hook pirates)
  (dolist (pirate pirates)
    (add-hook hook pirate)))

(defun csand/ls (path &optional abs match nosort)
  "Like directory-files, but omits . and .."
  (let* ((fails (directory-files path nil match nosort))
         (files (remove "." (remove ".." fails))))
    (if abs (mapcar (lambda (file)
                      (expand-file-name file path)) files)
      files)))

(provide 'init-funcs)
