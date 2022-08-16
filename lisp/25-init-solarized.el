(when (and (require 'solarized nil t) (display-graphic-p))
  (require 'solarized-palettes)
  (setq solarized-distinct-doc-face t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (setq solarized-use-variable-pitch nil)

  (load-theme 'solarized-dark t)

  (setq solarized-custom-faces
        '("My personal solarized theme customizations"
          (custom-theme-set-faces
           theme-name
           `(button ((t (:inherit link))))
           `(cursor ((t (:background ,yellow))))
           `(dired-header ((t (:foreground ,blue :underline ,blue :weight bold))))
           `(fringe ((t (:foreground ,s-line))))
           `(header-line ((t (:foreground ,yellow :underline ,yellow :weight bold))))
           `(minibuffer-prompt ((t (:foreground ,yellow))))
           `(mode-line ((t (:background ,blue-2bg))))
           `(mode-line-buffer-id ((t (:weight bold))))
           `(mode-line-inactive ((t (:background ,base02))))
           ;; symbol-overlay - TODO add other faces and send upstream
           `(symbol-overlay-default-face ((t :inherit unspecified :foreground ,magenta))))))

  (solarized-with-color-variables
    'dark 'solarized-dark solarized-dark-color-palette-alist solarized-custom-faces))
