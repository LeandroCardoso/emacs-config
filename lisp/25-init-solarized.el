(when (and (require 'solarized-theme nil t) (display-graphic-p))
  (require 'solarized-palettes)
  (setq solarized-distinct-doc-face t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (setq solarized-use-variable-pitch nil)

  (progn
    (setq solarized-custom-faces
          '("My personal solarized theme customizations"
            (custom-theme-set-faces
             theme-name
             `(cursor ((,class (:background ,yellow))))
             `(fringe ((,class (:foreground ,s-line))))
             `(header-line ((,class (:underline ,base01 :weight bold))))
             `(hl-line ((,class (:background ,(solarized-color-blend base03 "#000000" 0.8)))))
             `(minibuffer-prompt ((,class (:foreground ,yellow))))
             `(mode-line ((,class (:background ,blue-2bg))))
             `(mode-line-buffer-id ((,class (:weight bold))))
             `(mode-line-inactive ((,class (:background ,base02))))
             `(dired-header ((,class (:inherit (header-line dired-directory)))))
             ;; symbol-overlay - TODO add other faces and upstream
             ;; `(symbol-overlay-default-face ((,class :inherit unspecified :foreground ,magenta)))
             )))

    (load-theme 'solarized-dark t)

    (solarized-with-color-variables
      'dark 'solarized-dark solarized-dark-color-palette-alist solarized-custom-faces))
  )
