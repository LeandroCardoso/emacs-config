(when (require 'yasnippet nil t)
  (setq yas-wrap-around-region t)
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode)


  ;; redefine keymap to use <C-tab>, instead of <tab> to avoid conflicting with company
  (define-key yas-keymap (kbd "<tab>") nil)
  (define-key yas-keymap (kbd "<S-tab>") nil)
  (define-key yas-keymap (kbd "<backtab>") nil)
  (define-key yas-keymap (kbd "<C-tab>") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "<C-S-tab>") 'yas-prev-field)
  (define-key yas-keymap (kbd "<C-S-iso-lefttab>") 'yas-prev-field))
