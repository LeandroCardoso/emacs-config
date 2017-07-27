(when (require 'solarized-theme nil t)
  (setq solarized-distinct-doc-face t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (load-theme 'solarized-dark t)
  (set-face-background 'cursor "#b58900") ;yellow
  (set-face-foreground 'mode-line-buffer-id "#b58900") ;yellow
  ;; TODO packages
  ;; TODO some grep improvements
  )
