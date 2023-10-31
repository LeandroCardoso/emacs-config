(with-eval-after-load "desktop"
  (setq desktop-save 'ask-if-exists)
  (desktop-save-mode)
  (add-to-list 'desktop-locals-to-save 'buffer-display-time)
  (add-hook 'desktop-after-read-hook 'clean-buffer-list))
