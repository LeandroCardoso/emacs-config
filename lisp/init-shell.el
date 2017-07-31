;; Display shell buffers in the same window
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*" . ((display-buffer-reuse-window display-buffer-same-window)
                                 (reusable-frames . t))))

(defun shell-other-window (&optional buffer)
  "Like `shell', but put buffer in another window."
  (interactive)
  (let (buf (call-interactively 'shell))
    (switch-to-buffer-other-window buf)))

(defun shell-other-frame (&optional buffer)
  "Like `shell', but put buffer in another frame."
  (interactive)
  (let (buf (call-interactively 'shell))
    (switch-to-buffer-other-frame buf)))

(global-set-key (kbd "C-x $") 'shell) ; original is set-selective-display
(global-set-key (kbd "C-x 4 $") 'shell-other-window)
(global-set-key (kbd "C-x 5 $") 'shell-other-frame)
