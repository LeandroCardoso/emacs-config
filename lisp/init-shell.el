(global-set-key (kbd "C-x $") 'shell) ; original is set-selective-display

(when (require 'bash-completion nil t)
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete))
