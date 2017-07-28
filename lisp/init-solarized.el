(when (require 'solarized-theme nil t)
  (setq solarized-distinct-doc-face t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (load-theme 'solarized-dark t)
  (set-face-background 'cursor "#268bd2")              ;blue
  (set-face-foreground 'mode-line-buffer-id "#b58900") ;yellow
  (set-face-foreground 'minibuffer-prompt (face-foreground 'mode-line-buffer-id))
  (set-face-attribute 'mode-line nil
                      :background "#073642"            ;s-base02
                      :overline (face-foreground 'mode-line-buffer-id)
                      :underline nil
                      :box `(:line-width 2
                                         :color ,(face-background 'mode-line)
                                         :style unspecified))
  (set-face-attribute 'mode-line-inactive nil
                      :background (face-background 'mode-line)
                      :overline nil
                      :underline nil
                      :box (face-attribute 'mode-line :box))

  ;; These should be fixed in upstream
  (set-face-foreground 'magit-blame-name "#8B2C02") ;orange-d
  (set-face-foreground 'magit-blame-date "#8B2C02") ;orange-d
  
  ;; TODO packages
  ;; TODO some grep improvements
  )
