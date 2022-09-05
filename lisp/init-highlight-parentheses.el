(when (require 'highlight-parentheses nil t)
  (global-highlight-parentheses-mode)
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup))
