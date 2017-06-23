(global-set-key (kbd "C-M-,") 'xref-find-references)

(with-eval-after-load "xref"
  (define-key xref--xref-buffer-mode-map (kbd "<tab>") 'xref-next-line)
  (define-key xref--xref-buffer-mode-map (kbd "<backtab>") 'xref-prev-line))
