;; enable windmove - CTRL is only modifier not used by org-mode
(windmove-default-keybindings 'ctrl)
(when (>= emacs-major-version 27)
  (windmove-delete-default-keybindings 'C-x 'ctrl)
  (windmove-display-default-keybindings '(ctrl meta))
  (windmove-swap-states-default-keybindings '(ctrl shift)))

(setq windmove-create-window t)

;; framemove is a windmove extension for frames.
(when (require 'framemove nil t)
  (setq framemove-hook-into-windmove t))
