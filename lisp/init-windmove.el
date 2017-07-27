;; enable windmove - CTRL is only modifier not used by org-mode
(windmove-default-keybindings 'ctrl)

;; framemove is a windmove extension for frames.
(when (require 'framemove nil t)
  (setq framemove-hook-into-windmove t))
