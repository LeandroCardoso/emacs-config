(with-eval-after-load "cc-mode"
  (setq c-guess-region-max 100000)

  (defun c-or-c++-mode-i ()
    "Analyze buffer and enable either C or C++ mode.
This is the interactive version of `c-or-c++-mode'."
    (interactive)
    (c-or-c++-mode))

  (c-add-style "c++-custom" '("stroustrup" (c-offsets-alist (inlambda . 0)
                                                          (inline-open . 0))))

  (add-to-list 'c-default-style '(c++-mode . "c++-custom"))
  (add-to-list 'c-default-style '(c-mode . "c++-custom"))

  ;; find-file
  (setq cc-search-directories '("." "./*" "../*" "/usr/include" "/usr/local/include/*"))

  (add-to-list 'c-font-lock-extra-types "BOOL")
  (add-to-list 'c++-font-lock-extra-types "BOOL")

  ;; hooks
  (defun c-c++-setup-hook ()
    (font-lock-add-keywords nil '(("\\<\\(TRUE\\|FALSE\\)\\>" . 'font-lock-constant-face))))

  (defun c-setup-hook ()
    (c-toggle-comment-style -1))

  (defun c++-setup-hook ())

  (add-hook 'c-mode-hook #'c-setup-hook)
  (add-hook 'c-mode-hook #'c-c++-setup-hook)
  (add-hook 'c++-mode-hook #'c++-setup-hook)
  (add-hook 'c++-mode-hook #'c-c++-setup-hook))
