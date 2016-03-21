(defun my-cc-init-hook ()
  (setq c-basic-offset 4)
  (setq c-offsets-alist '((substatement-open . 0) (case-label . +)))
  ;;(setq cc-search-directories '("." "include" "*" "../*" "/usr/include" "/usr/local/include/*"))
  )

(add-hook 'c-initialization-hook 'my-cc-init-hook)

(defun my-c-mode-common-hook ())
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-c-mode-hook ())
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ())
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
