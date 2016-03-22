(when (require 'ag nil t)
  (setq ag-highlight-search t)
  (global-set-key (kbd "C-c g") 'ag))

  ;; wgrep-ag
(when (require 'wgrep-ag nil t)
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))
