(with-eval-after-load "desktop"
  (add-hook 'desktop-save-hook 'clean-buffer-list)
  (setq desktop-save 'ask-if-exists)
  (desktop-save-mode))
