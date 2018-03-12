(defun fontawesome ()
  "Returns the FontAwesome font name if available, otherwise returns nil."
  (seq-find (lambda (font-name) (string-match "font.?awesome" font-name)) (font-family-list)))

;; Set FontAwesome when available
(let ((fontawesome (fontawesome)))
  (when fontawesome
    (set-fontset-font "fontset-default"
                      '(#xf000 . #xf453)
                      (font-spec :size (if (eq system-type 'windows-nt) 12 15) :name fontawesome))))

;; TODO ibuffer
;; TODO dired
;; TODO flycheck
;; TODO clock
;; TODO smex
;; TODO org
