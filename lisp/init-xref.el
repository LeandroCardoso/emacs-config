(with-eval-after-load "xref"
  (define-key xref--xref-buffer-mode-map (kbd "<tab>") 'xref-next-line)
  (define-key xref--xref-buffer-mode-map (kbd "<backtab>") 'xref-prev-line))
