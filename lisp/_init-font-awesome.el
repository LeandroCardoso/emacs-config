;; Set FontAwesome when available
(when (member "FontAwesome" (font-family-list))
  (set-fontset-font "fontset-default" '(#xf000 . #xf453) (font-spec :size 15 :name "FontAwesome")))

(defun fontawesome-p ()
  "Return non nil if FontAwesome is available."
  (member "FontAwesome" (font-family-list)))

;; TODO ibuffer
;; TODO dired
;; TODO flycheck
;; TODO clock
;; TODO smex
;; TODO org
