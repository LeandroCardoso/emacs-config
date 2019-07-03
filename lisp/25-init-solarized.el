(when (and (require 'solarized-theme nil t) (display-graphic-p))
  (setq solarized-distinct-doc-face t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (setq solarized-use-variable-pitch nil)

  (load-theme 'solarized-dark t)

  (set-face-background 'cursor (face-foreground 'font-lock-constant-face)) ; blue
  (set-face-attribute 'header-line nil
                      :foreground (face-foreground 'warning)
                      :weight 'bold) ; yellow
  (set-face-foreground 'fringe (face-foreground 'vertical-border))
  (set-face-foreground 'mode-line-buffer-id (face-foreground 'warning))    ; yellow
  (set-face-foreground 'minibuffer-prompt (face-foreground 'mode-line-buffer-id))
  (set-face-attribute 'mode-line nil
                      :overline (face-foreground 'mode-line-buffer-id)
                      :underline 'unspecified
                      :box `(:line-width 4
                             :color ,(face-background 'mode-line)
                             :style 'unspecified))

  (set-face-attribute 'mode-line-inactive nil
                      :background (face-background 'mode-line)
                      :overline 'unspecified
                      :underline 'unspecified
                      :box (face-attribute 'mode-line :box))

  (with-eval-after-load "dired"
    (set-face-attribute 'dired-header nil
                        :foreground (face-foreground 'header-line)
                        :background 'unspecified
                        :weight 'bold))

  (with-eval-after-load "moody"
    (set-face-background 'mode-line-inactive (solarized-color-blend (face-background 'mode-line)
                                                                    (face-background 'default)
                                                                    0.5)))

  (with-eval-after-load "symbol-overlay"
    (set-face-attribute 'symbol-overlay-default-face nil
                        :inherit 'unspecified
                        :foreground (face-background 'isearch))) ; magenta

  (with-eval-after-load "woman"
    (set-face-attribute 'woman-bold nil :inherit '(Man-overstrike))
    (set-face-attribute 'woman-italic nil :inherit '(Man-underline)))
  )
