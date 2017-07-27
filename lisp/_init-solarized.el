(when (require 'solarized-theme nil t)
  (setq solarized-distinct-doc-face t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (load-theme 'solarized-dark t)
  (set-face-background 'cursor "#b58900")              ;yellow
  (set-face-foreground 'mode-line-buffer-id "#b58900") ;yellow
  (set-face-attribute 'mode-line nil
                      :background "#073642" ;s-base03
                      :overline "#586e75"   ;s-base02
                      :underline nil
                      :box `(:line-width 2
                                         :color ,(face-background 'mode-line)
                                         :style unspecified))
  (set-face-attribute 'mode-line-inactive nil
                      :background (face-background 'mode-line)
                      :overline nil
                      :underline nil
                      :box (face-attribute 'mode-line :box))
  ;; TODO packages
  ;; TODO some grep improvements
  )
