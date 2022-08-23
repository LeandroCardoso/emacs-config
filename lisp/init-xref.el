(with-eval-after-load "xref"
  (define-key xref--xref-buffer-mode-map (kbd "<tab>") 'xref-next-line) ; original is xref-quit-and-goto-xref
  (define-key xref--xref-buffer-mode-map (kbd "<backtab>") 'xref-prev-line)
  (define-key xref--xref-buffer-mode-map (kbd "M-<return>") 'xref-quit-and-goto-xref))
