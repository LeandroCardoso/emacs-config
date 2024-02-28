(when (require 'transpose-frame nil t)
  (define-key ctl-x-map (kbd "|") 'rotate-frame-clockwise)
  (define-key ctl-x-map (kbd "\\") 'rotate-frame))
