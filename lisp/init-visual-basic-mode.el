(when (require 'visual-basic-mode nil t)
  (setq visual-basic-mode-indent 4)
  (add-to-list 'auto-mode-alist '("\\.vbs?\\'" . visual-basic-mode)))
