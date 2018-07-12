(setq hscroll-step 0.25)
(setq scroll-conservatively 5) ;; recenter if scroll more than value
;; (setq scroll-margin 1) scroll-margin does not work nicely with reposition-window
(setq scroll-preserve-screen-position t)

(global-set-key (kbd "<scroll>") 'scroll-lock-mode)
(global-set-key (kbd "<M-scroll>") 'scroll-all-mode)
(global-set-key (kbd "<C-pause>") 'follow-mode)

(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)
(global-set-key (kbd "<M-Scroll_Lock>") 'scroll-all-mode)
(global-set-key (kbd "<C-Scroll_Lock>") 'follow-mode)

(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "M-n") 'scroll-up-line)
