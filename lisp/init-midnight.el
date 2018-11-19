(require 'midnight)

(setq clean-buffer-list-delay-general 1)

(defun clean-buffer-list-check-idle-time ()
  "Return t if emacs idle time is less than the
`clean-buffer-list-delay-general'."
  (let (idle (current-idle-time))
    (or (not idle)
        (< (round (float-time idle)) (* clean-buffer-list-delay-general 24 60 60)))))

;; Don't cleanup the buffer list when I am idle for more than 1 day, like weekends and holidays.
(advice-add 'clean-buffer-list :before-while #'clean-buffer-list-check-idle-time)

(midnight-mode t)
