(defun shell-other-window (&optional buffer)
  "Like `shell', but creates a new window or reuses an existing
one."
  (interactive "P")
  (if (one-window-p)
      (split-window)
    (other-window 1))
  (shell buffer))

(global-set-key (kbd "C-x $") 'shell) ; original is set-selective-display
(global-set-key (kbd "C-x 4 $") 'shell-other-window)

(when (require 'bash-completion nil t)
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete))
