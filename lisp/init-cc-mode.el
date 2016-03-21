(defun my-cc-init-hook ()
  (setq-default c-basic-offset 4)
  ;; find-file
  (setq cc-search-directories '("." "include" "*" "../*" "/usr/include" "/usr/local/include/*")))

(add-hook 'c-initialization-hook 'my-cc-init-hook)

;; TODO proper style customization
(defun my-c-mode-common-hook ()
  ;;(set c-offsets-alist '((substatement-open . 0) (case-label . +))))
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-c-mode-hook ())
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ())
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
