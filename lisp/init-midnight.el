(require 'midnight)

(setq clean-buffer-list-delay-general 2)

(defun clean-buffer-list-check-idle-time ()
  "Return t if emacs idle time is less than the
`clean-buffer-list-delay-general'."
  (< (round (float-time (or (current-idle-time) '(0 0 0))))
     (* clean-buffer-list-delay-general 24 60 60)))

;; Don't cleanup the buffer list when I am idle for more than 1 day, like weekends and holidays.
(advice-add 'clean-buffer-list :before-while #'clean-buffer-list-check-idle-time)

;; Add with DEPTH=-10 because `clean-buffer-list' should run before the `desktop-kill'
(add-hook 'kill-emacs-hook 'clean-buffer-list -10)

(midnight-mode t)
