(require 'midnight)

(setq clean-buffer-list-delay-general 1)
(setq clean-buffer-list-last-execution (current-time))

;; Don't cleanup the buffer list when I am idle for more than 1 day, like weekends and holidays.
(defun clean-buffer-list-check-idle-time ()
  "Like `clean-buffer-list', but it only excecutes when computer
is not idle."
  (when (and (< (round (float-time (or (current-idle-time) '(0 0 0))))
                (* clean-buffer-list-delay-general 24 60 60))
             ;; Avoid execution when computer was resumed from suspension, which usually leads to
             ;; all buffers to be killed.
             (< (- (round (float-time (current-time)))
                   (round (float-time clean-buffer-list-last-execution)))
                (+ (* clean-buffer-list-delay-general 24 60 60)
                   (* 2 60 60))))
    (clean-buffer-list))
  (setq clean-buffer-list-last-execution (current-time)))

(remove-hook 'midnight-hook 'clean-buffer-list)
(add-hook 'midnight-hook 'clean-buffer-list-check-idle-time)

(midnight-mode t)
