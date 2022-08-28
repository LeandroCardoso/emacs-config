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
           `(button ((t :inherit link)))
           `(cursor ((t :background ,yellow)))
           `(custom-button ((t :inherit button)))
           `(dired-header ((t :inherit dired-directory :weight bold)))
           `(fringe ((t :foreground ,s-line)))
           `(header-line ((t :foreground ,yellow :underline ,yellow :weight bold :extend t)))
           `(help-key-binding ((t :box (:line-width -1 :color ,s-line) :weight bold)))
           `(minibuffer-prompt ((t :foreground ,yellow)))
           `(mode-line ((t :background ,blue-2bg)))
           `(mode-line-buffer-id ((t :foreground ,yellow :weight bold)))
           `(mode-line-inactive ((t :background ,base02)))
           `(region ((t :foreground unspecified :background ,blue-2bg :extend t)))
           `(separator-line ((t :height 0.1 :inherit transient-separator)))
           `(shortdoc-heading ((t :inherit info-title-1)))
           `(shortdoc-section ((t :inherit info-title-1 :weight normal)))
           `(symbol-overlay-default-face ((t :inherit unspecified :foreground ,magenta)))
           `(isearch-group-1 ((t :foreground ,base03 :background ,magenta-1fg :weight bold)))
           `(isearch-group-2 ((t :foreground ,base03 :background ,magenta-2fg :weight bold))))
          (custom-theme-set-variables
           theme-name
           `(ibuffer-filter-group-name-face 'link)
           `(ibuffer-title-face 'header-line))))

  (solarized-with-color-variables
    'dark 'solarized-dark solarized-dark-color-palette-alist solarized-custom-faces))
