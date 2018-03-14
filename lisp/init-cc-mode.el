(setq-default c-basic-offset 4)

;; find-file
(setq cc-search-directories '("." "./*" "../*" "/usr/include" "/usr/local/include/*"))

;; Unfortunately it is really sad that people keep using .h for c++ header files instead of anything
;; more appropriate and I have to keep the following setting.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; hooks

(defun c-init-setup-hook ())
(add-hook 'c-initialization-hook #'c-init-setup-hook)


(defun c-common-setup-hook ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label '+))
(add-hook 'c-mode-common-hook #'c-common-setup-hook)


(defun c-setup-hook ())
(add-hook 'c-mode-hook #'c-setup-hook)


(defun c++-setup-hook ())
(add-hook 'c++-mode-hook #'c++-setup-hook)
