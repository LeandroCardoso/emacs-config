(when (and (require 'solarized-theme nil t) (display-graphic-p))
  (setq solarized-distinct-doc-face t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (setq solarized-use-variable-pitch nil)

  (load-theme 'solarized-dark t)

  (set-face-background 'cursor (face-foreground 'font-lock-constant-face)) ; blue
  (set-face-foreground 'mode-line-buffer-id (face-foreground 'warning))    ; yellow
  (set-face-foreground 'minibuffer-prompt (face-foreground 'mode-line-buffer-id))
  (set-face-attribute 'mode-line nil
                      :overline (face-foreground 'mode-line-buffer-id)
                      :underline 'unspecified
                      :box `(:line-width 2
                             :color ,(face-background 'mode-line)
                             :style 'unspecified))

  (set-face-attribute 'mode-line-inactive nil
                      :background (face-background 'mode-line)
                      :overline 'unspecified
                      :underline 'unspecified
                      :box (face-attribute 'mode-line :box))

  (with-eval-after-load "dired"
    (set-face-attribute 'dired-header nil
                        :foreground (face-foreground 'dired-directory) ; blue
                        :background 'unspecified
                        :weight 'bold))

  (with-eval-after-load "powerline"
    (set-face-foreground 'mode-line-buffer-id-inactive "#7B6000") ; yellow-d
    ;; overline does not mix well with powerline
    (set-face-attribute 'mode-line nil :overline 'unspecified)
    ;; use a box color instead
    (set-face-attribute 'mode-line nil :box (face-foreground 'mode-line-buffer-id-inactive))
    (set-face-attribute 'mode-line-inactive nil :box (face-background 'mode-line-inactive)))

  (with-eval-after-load "symbol-overlay"
    (set-face-attribute 'symbol-overlay-default-face nil
                        :inherit 'unspecified
                        :foreground "#d33682")) ; magenta

  ;; TODO packages
  )
