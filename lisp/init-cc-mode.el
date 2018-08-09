(with-eval-after-load "cc-mode"
  (add-to-list 'c-default-style '(c++-mode . "stroustrup"))
  (add-to-list 'c-default-style '(c-mode . "stroustrup"))

  ;; find-file
  (setq cc-search-directories '("." "./*" "../*" "/usr/include" "/usr/local/include/*"))

  (add-to-list 'c-font-lock-extra-types "BOOL")
  (add-to-list 'c++-font-lock-extra-types "BOOL")

  ;; hooks
  (defun c-common-setup-hook ())

  (defun c-setup-hook ()
    (font-lock-add-keywords nil '(("\\<\\(TRUE\\|FALSE\\)\\>" . 'font-lock-constant-face))))

  (defun c++-setup-hook ())

  (add-hook 'c-mode-common-hook #'c-common-setup-hook)
  (add-hook 'c-mode-hook #'c-setup-hook)
  (add-hook 'c++-mode-hook #'c-setup-hook)
  (add-hook 'c++-mode-hook #'c++-setup-hook))
