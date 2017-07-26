(defun display-time-mode-dwim ()
  "Enable `display-time-mode' in text teminals or when any frame
is in fullscreen in a graphical display."
  (display-time-mode
   (if (or (seq-some #'(lambda (frame)
                         (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)))
                     (frame-list))
           (not (display-graphic-p)))
       1
     0)))

(setq display-time-24hr-format t)
(setq display-time-load-average-threshold 0.5)

(advice-add 'toggle-frame-fullscreen :after #'display-time-mode-dwim)
