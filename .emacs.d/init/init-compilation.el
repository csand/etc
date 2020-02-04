;;; init-compilation.el -- Customized compilation mode

(require 'ansi-color)

;; Stolen from http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
(defun colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'colorize-compilation)

(provide 'init-compilation)
