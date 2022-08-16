(when (require 'highlight-parentheses nil t)
  (setq hl-paren-delay 0.25)
  (global-highlight-parentheses-mode)
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup))
