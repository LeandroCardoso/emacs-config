(when (require 'which-key nil t)
  (setq which-key-max-description-length 50)
  (global-set-key (kbd "<f1> x") 'which-key-show-top-level)
  (which-key-mode))
