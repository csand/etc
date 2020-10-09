;;; init-fonts.el --- Font face configuration

;; Pragmata Pro has a very high baseline, so place
;; the underline at the descent line instead.
(setq x-underline-at-descent-line t)

(setq-default line-spacing nil)

(defvar sand-font-height 140)

(set-face-attribute 'default nil
                    :height sand-font-height
                    :width 'condensed
                    :weight 'medium)

(defun sand/set-font-family (family &optional face)
  (interactive "sFont family: ")
  (set-face-attribute (or face 'default) nil :family family))

(defun sand/set-font-height (&optional height face)
  (interactive (list (read-number (format "Font height: ") sand-font-height)))
  (set-face-attribute (or face 'default) nil
                      :height (or height sand-font-height)))

(sand/set-font-family "PragmataPro")

;; Mostly affects markdown mode source blocks
(sand/set-font-family "PragmataPro Mono" 'fixed-pitch)

(sand/set-font-family "Triplicate T4c" 'fixed-pitch-serif)
(sand/set-font-family "Triplicate T4" 'variable-pitch)

(sand/set-font-family "Gintronic")
(sand/set-font-family "Gintronic" 'fixed-pitch)
(sand/set-font-height 120)

;; (set-face-attribute 'default nil :weight 'bold)
(set-face-attribute 'default nil :weight 'light)
;; (set-face-attribute 'default nil :weight 'normal)
;; (setq-default line-spacing 1)

(provide 'init-fonts)
