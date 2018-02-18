(when (require 'powerline nil t)
  (setq powerline-default-separator 'slant)
  (setq powerline-display-hud nil)
  (setq powerline-gui-use-vcs-glyph t)
  (setq powerline-height 28)
  ;; TODO customize mode-line-format instead of use powerline-default-theme
  (powerline-default-theme)
  )
