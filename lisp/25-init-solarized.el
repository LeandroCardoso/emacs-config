(when (and (require 'solarized-theme nil t) (display-graphic-p))
  (setq solarized-distinct-doc-face t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (setq solarized-use-variable-pitch nil)

  (load-theme 'solarized-dark t)

  (solarized-with-color-variables 'dark
    (set-face-background 'cursor blue)
    (set-face-attribute 'header-line nil
                        :foreground yellow
                        :weight 'bold)
    (set-face-foreground 'fringe (face-foreground 'vertical-border))
    (set-face-foreground 'mode-line-buffer-id yellow)
    (set-face-foreground 'minibuffer-prompt yellow)
    (set-face-background 'mode-line (solarized-color-blend base02 blue 0.85)))

  (with-eval-after-load "dired"
    (set-face-attribute 'dired-header nil
                        :foreground (face-foreground 'header-line)
                        :background 'unspecified
                        :weight 'bold))

  (with-eval-after-load "symbol-overlay"
    (set-face-attribute 'symbol-overlay-default-face nil
                        :inherit 'unspecified
                        :foreground (face-background 'isearch))) ; magenta

  (with-eval-after-load "woman"
    (set-face-attribute 'woman-bold nil :inherit '(Man-overstrike))
    (set-face-attribute 'woman-italic nil :inherit '(Man-underline)))
  )
