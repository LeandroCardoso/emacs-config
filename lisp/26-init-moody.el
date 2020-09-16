(when (require 'moody nil t)
  ;; Make sure that the face `mode-line' does not set `:box' and that `:underline' and `:overline'
  ;; are the same color or are both `undefined'. If defined, then the line color should be different
  ;; from the `:background' colors of both `mode-line'and `default'. The same rules apply to
  ;; `mode-line-inactive'. The line colors of `mode-line' and `mode-line-inactive' do not
  ;; necessarily have to be identical.
  (set-face-attribute 'mode-line          nil :overline   'unspecified)
  (set-face-attribute 'mode-line          nil :underline  'unspecified)
  (set-face-attribute 'mode-line          nil :box        'unspecified)
  (set-face-attribute 'mode-line-inactive nil :overline   'unspecified)
  (set-face-attribute 'mode-line-inactive nil :underline  'unspecified)
  (set-face-attribute 'mode-line-inactive nil :box        'unspecified)

  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 30)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))
