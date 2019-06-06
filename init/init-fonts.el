;;; init-fonts.el --- Font face configuration

;; Pragmata has a very high baseline, so place the
;; underline at the descent line instead
(setq x-underline-at-descent-line t)

(setq sand-font-height 130)
(set-face-attribute 'default nil
                    :family "PragmataPro"
                    :height sand-font-height
                    :width 'normal
                    :weight 'normal)

;; Set the fixed-pitch font. Mostly affects markdown mode source blocks
(set-face-attribute 'fixed-pitch nil
                    :family "PragmataPro Mono")

(set-face-attribute 'fixed-pitch-serif nil
                    :family "Triplicate T4c")

(set-face-attribute 'variable-pitch nil
                    :family "Triplicate T4"
                    :height sand-font-height)

(provide 'init-fonts)
