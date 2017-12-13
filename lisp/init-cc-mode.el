(setq-default c-basic-offset 4)

;; find-file
(setq cc-search-directories '("." "*" "../*" "/usr/include" "/usr/local/include/*"))

;; Unfortunately it is really sad that people keep using .h for c++ header files instead of anything
;; more appropriate and I have to keep the following setting.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; hooks

(defun my-cc-init-hook ())
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
