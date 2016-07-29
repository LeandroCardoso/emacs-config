(setq column-number-mode t)
(setq completion-show-help nil)
(setq kill-do-not-save-duplicates t)
(setq kill-whole-line t)
(setq next-error-highlight 'fringe-arrow)
(setq normal-erase-is-backspace nil)
(setq shift-select-mode nil)

(global-set-key (kbd "RET") 'newline-and-indent) ;; default is newline
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-M-|") 'delete-indentation)
