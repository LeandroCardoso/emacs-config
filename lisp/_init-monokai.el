(when (require 'monokai-theme nil t)
  (load-theme 'monokai t)
  (set-face-background 'cursor monokai-red)
  (set-face-foreground 'fringe monokai-gray)
  ;; monokai has some really weird mode-line faces
  (set-face-attribute 'mode-line nil
                      :overline monokai-gray
                      :box `(:line-width 2
                                         :color ,(face-background 'mode-line)
                                         :style unspecified))
  (set-face-attribute 'mode-line-inactive nil
                      :background (face-background 'mode-line)
                      :overline nil
                      :box (face-attribute 'mode-line :box))

  (with-eval-after-load "org"
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5
                    org-level-6
                    org-level-7
                    org-level-8))
      (set-face-attribute face nil :height 'unspecified)))
)
