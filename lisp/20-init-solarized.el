(when (and (require 'solarized-theme nil t) (display-graphic-p))
  (setq solarized-distinct-doc-face t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-more-italic t)
  (setq solarized-use-variable-pitch nil)

  (load-theme 'solarized-dark t)

  ;; colors directly copied from solarized.el
  (let ((s-base03    "#002b36")
        (s-base02    "#073642")
        ;; emphasized content
        (s-base01    "#586e75")
        ;; primary content
        (s-base00    "#657b83")
        (s-base0     "#839496")
        ;; comments
        (s-base1     "#93a1a1")
        ;; background highlight light
        (s-base2     "#eee8d5")
        ;; background light
        (s-base3     "#fdf6e3")

        ;; Solarized accented colors
        (yellow    "#b58900")
        (orange    "#cb4b16")
        (red       "#dc322f")
        (magenta   "#d33682")
        (violet    "#6c71c4")
        (blue      "#268bd2")
        (cyan      "#2aa198")
        (green     "#859900")

        ;; Darker and lighter accented colors
        ;; Only use these in exceptional circumstances!
        (yellow-d  "#7B6000")
        (yellow-l  "#DEB542")
        (orange-d  "#8B2C02")
        (orange-l  "#F2804F")
        (red-d     "#990A1B")
        (red-l     "#FF6E64")
        (magenta-d "#93115C")
        (magenta-l "#F771AC")
        (violet-d  "#3F4D91")
        (violet-l  "#9EA0E5")
        (blue-d    "#00629D")
        (blue-l    "#69B7F0")
        (cyan-d    "#00736F")
        (cyan-l    "#69CABF")
        (green-d   "#546E00")
        (green-l   "#B4C342"))

    (set-face-background 'cursor blue)
    (set-face-foreground 'mode-line-buffer-id yellow)
    (set-face-foreground 'minibuffer-prompt (face-foreground 'mode-line-buffer-id))
    (set-face-attribute 'mode-line nil
                        :background s-base02
                        :overline (face-foreground 'mode-line-buffer-id)
                        :underline nil
                        :box `(:line-width 2
                               :color ,(face-background 'mode-line)
                               :style unspecified))

    (set-face-attribute 'mode-line-inactive nil
                        :background (face-background 'mode-line)
                        :overline nil
                        :underline nil
                        :box (face-attribute 'mode-line :box))


    ;; Do not use the defined colors with eval-after-load, it does not work

    (with-eval-after-load "powerline"
      (set-face-foreground 'mode-line-buffer-id-inactive "#7B6000") ; yellow-d
      ;; overline does not mix well with powerline
      (set-face-attribute 'mode-line nil :overline nil)
      ;; use a box color instead
      (set-face-attribute 'mode-line nil :box (face-foreground 'mode-line-buffer-id-inactive)))
    )

  ;; TODO packages
  ;; TODO some grep improvements
  )
