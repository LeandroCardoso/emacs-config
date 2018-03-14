(when (require 'symbol-overlay nil t)
  (add-hook 'prog-mode-hook #'symbol-overlay-mode))
