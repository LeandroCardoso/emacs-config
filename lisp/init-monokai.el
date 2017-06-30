(when (require 'monokai-theme nil t)
  (load-theme 'monokai t)
  (set-face-background 'cursor monokai-red)
  (set-face-foreground 'fringe monokai-gray)
  ;; monokai has some really weird mode-line faces
  (set-face-attribute 'mode-line-inactive nil
                      :background (face-background 'mode-line)
                      :box `(:line-width 2
                                         :color ,(face-background 'mode-line)
                                         :style unspecified))
  (set-face-attribute 'mode-line nil
                      :box `(:line-width 2
                                         :color ,monokai-gray
                                         :style unspecified)))
