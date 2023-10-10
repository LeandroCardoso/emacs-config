(with-eval-after-load "desktop"
  (setq desktop-save 'ask-if-exists)
  (desktop-save-mode)
  (add-hook 'desktop-after-read-hook 'clean-buffer-list))
