;; Display shell buffers in the same window
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*" .
               ((display-buffer-reuse-window display-buffer-same-window) (reusable-frames . t))))

(global-set-key (kbd "C-x $") 'shell) ; original is set-selective-display
