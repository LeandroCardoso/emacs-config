(when (require 'yasnippet nil t)
  (setq yas-wrap-around-region t)
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)

  (require 'hippie-exp)
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)

  ;; redefine keymap to use <C-tab>, instead of <tab> to avoid conflicting with company
  (define-key yas-keymap (kbd "<tab>") nil)
  (define-key yas-keymap (kbd "<S-tab>") nil)
  (define-key yas-keymap (kbd "<backtab>") nil)
  (define-key yas-keymap (kbd "<C-tab>") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "<C-S-tab>") 'yas-prev-field))
