(when (require 'yasnippet nil t)
  (setq yas-wrap-around-region t)
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))
