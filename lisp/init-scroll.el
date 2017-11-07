(setq scroll-conservatively 5) ;; recenter if scroll more than value
(setq scroll-margin 1)
(setq scroll-preserve-screen-position t)

(global-set-key (kbd "<scroll>") 'scroll-lock-mode)
(global-set-key (kbd "<M-scroll>") 'scroll-all-mode)
(global-set-key (kbd "<C-pause>") 'follow-mode)
