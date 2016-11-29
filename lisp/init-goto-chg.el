(when (require 'goto-chg nil t)
  (global-set-key (kbd "C-.") 'goto-last-change)
  (global-set-key (kbd "C-,") 'goto-last-change-reverse))
