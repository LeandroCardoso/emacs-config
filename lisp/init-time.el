(defun smart-display-time-mode ()
  "Enable `display-time-mode' if emacs is running in a text
teminal or if it is running in a graphical display and any frame
is in fullscreen. Disable it otherwise."
  (display-time-mode
   (if (or (not (display-graphic-p))
           (seq-some #'(lambda (frame)
                         (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)))
                     (frame-list)))
       1 ; enable
     0)))

(setq display-time-24hr-format t)
(setq display-time-interval 15)
(setq display-time-load-average-threshold 0.33)

(advice-add 'toggle-frame-fullscreen :after #'smart-display-time-mode)

;; call smart-display-time-mode on startup
(smart-display-time-mode)
