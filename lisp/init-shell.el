(defun shell-other-window (&optional buffer)
  "Like `shell', but creates a new window or reuses an existing
one."
  (interactive "P")
  (if (one-window-p)
      (split-window)
    (other-window 1))
  (shell buffer))

(define-key ctl-x-map (kbd "$") 'shell) ; original is set-selective-display
(define-key ctl-x-4-map (kbd "$") 'shell-other-window)

(when (require 'bash-completion nil t)
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete))
