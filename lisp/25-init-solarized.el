(when (and (require 'solarized-theme nil t) (display-graphic-p))
  (require 'solarized-palettes)
  (setq solarized-distinct-doc-face t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (setq solarized-use-variable-pitch nil)

  (setq solarized-custom-faces
        '("My personal solarized theme customization."
          (custom-theme-set-faces
           theme-name
           `(cursor ((,class (:background ,blue))))
           `(fringe ((,class (:foreground ,s-line))))
           `(header-line ((,class (:foreground ,yellow :underline ,base02 :weight bold))))
           `(minibuffer-prompt ((,class (:foreground ,yellow))))
           `(mode-line ((,class (:background ,(solarized-color-blend base02 blue 0.85)))))
           `(mode-line-buffer-id ((,class (:foreground ,yellow :weight bold))))
           `(mode-line-inactive ((,class (:background ,base02))))
           ;; dired
           `(dired-header ((,class (:foreground ,yellow :underline t :weight bold))))
           ;; transient
           `(transient-separator ((,class (:foreground ,s-line))))
           ;; symbol-overlay
           `(symbol-overlay-default-face ((,class :inherit unspecified :foreground ,magenta)))
           ;; woman
           `(woman-bold ((,class (:inherit Man-overstrike))))
           `(woman-italic ((,class (:inherit Man-underline)))))
          ;; (custom-theme-set-variables theme-name)
          ))

  (load-theme 'solarized-dark t)

  (solarized-with-color-variables
    'dark 'solarized-dark solarized-dark-color-palette-alist solarized-custom-faces))
