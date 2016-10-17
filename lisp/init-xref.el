(global-set-key (kbd "C-M-,") 'xref-find-references)

(define-key xref--xref-buffer-mode-map (kbd "<tab>") 'xref-next-line)
(define-key xref--xref-buffer-mode-map (kbd "<backtab>") 'xref-prev-line)
