(when (require 'woman nil t)
  (setq woman-fill-frame t)
  (setq woman-use-symbol-font t)
  (set-face-attribute 'woman-bold nil :inherit '(Man-overstrike))
  (set-face-attribute 'woman-italic nil :inherit '(Man-underline))
  (global-set-key (kbd "C-c m") 'woman))