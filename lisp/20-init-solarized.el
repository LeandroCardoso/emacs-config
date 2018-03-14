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

  (with-eval-after-load "semantic-idle"
    ;; original semantic-idle-symbol-highlight does not work play well with
    ;; semantic-highlight-func-current-tag-face
    (set-face-attribute 'semantic-idle-symbol-highlight nil
                        :background (face-background 'highlight) ; s-base02
                        :inherit 'unspecified))

  (with-eval-after-load "powerline"
    (set-face-foreground 'mode-line-buffer-id-inactive "#7B6000") ; yellow-d
    ;; overline does not mix well with powerline
    (set-face-attribute 'mode-line nil :overline 'unspecified)
    ;; use a box color instead
    (set-face-attribute 'mode-line nil :box (face-foreground 'mode-line-buffer-id-inactive))
    (set-face-attribute 'mode-line-inactive nil :box (face-background 'mode-line-inactive)))

  (with-eval-after-load "highlight-parentheses"
    (setq hl-paren-colors `("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))

  ;; TODO packages
  ;; TODO some grep improvements
  )
