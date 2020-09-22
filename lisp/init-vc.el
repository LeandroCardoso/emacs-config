(setq diff-font-lock-prettify t)
(setq vc-command-messages t)
(setq vc-find-revision-no-save t)
(setq vc-follow-symlinks t)

;; git
(setq vc-git-print-log-follow t)

(define-key vc-prefix-map (kbd "e" ) 'vc-ediff)
(define-key vc-prefix-map (kbd "H" ) 'vc-region-history)
(define-key vc-prefix-map (kbd "R" ) 'vc-rename-file)
