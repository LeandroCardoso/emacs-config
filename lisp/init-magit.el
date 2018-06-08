(when (require 'magit nil t)
  ;; settings
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-blame-echo-style 'margin)

  ;; Windows specific settings
  (when (eq system-type 'windows-nt)
    (setq w32-pipe-read-delay 0)
    (setq magit-process-connection-type nil)
    (setq magit-refresh-status-buffer nil)
    (remove-hook 'server-switch-hook 'magit-commit-diff)) ; remove diff output from commit

  ;; key bindings
  (define-key magit-mode-map [remap previous-line] 'magit-previous-line)
  (define-key magit-mode-map [remap next-line] 'magit-next-line)

  (define-key magit-file-section-map (kbd "SPC") 'magit-diff-visit-file-other-window)
  (define-key magit-hunk-section-map (kbd "SPC") 'magit-diff-visit-file-other-window))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
