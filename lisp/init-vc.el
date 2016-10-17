(setq vc-command-messages t)
(setq vc-follow-symlinks t)

(define-key vc-prefix-map (kbd "e" ) 'vc-ediff)
(define-key vc-prefix-map (kbd "H" ) 'vc-region-history)
(define-key vc-prefix-map (kbd "R" ) 'vc-rename-file)
