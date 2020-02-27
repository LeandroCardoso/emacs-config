(when (require 'magit nil t)
  ;; settings
  (setq magit-blame-echo-style 'margin)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-ediff-dwim-show-on-hunks t)

  ;; transient
  (setq transient-default-level 7)

  ;; Windows specific settings
  (when (eq system-type 'windows-nt)
    (setq w32-pipe-read-delay 0)
    (setq magit-process-connection-type nil)
    (setq magit-refresh-status-buffer nil)

    ;; experimental performance settings
    (setq magit-diff-highlight-indentation nil)
    (setq magit-diff-highlight-trailing nil)
    (setq magit-diff-paint-whitespace nil)
    (setq magit-diff-highlight-hunk-body nil)
    (setq magit-diff-refine-hunk nil)
    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

    (remove-hook 'server-switch-hook 'magit-commit-diff)) ; remove diff output from commit

  (add-hook 'magit-status-mode-hook #'disable-global-hl-line-mode)

  ;; key bindings
  (define-key magit-mode-map (kbd "M-u") 'magit-section-up)
  (define-key magit-mode-map [remap previous-line] 'magit-previous-line)
  (define-key magit-mode-map [remap next-line] 'magit-next-line)

  (define-key magit-file-section-map (kbd "SPC") 'magit-diff-visit-file-other-window)
  (define-key magit-hunk-section-map (kbd "SPC") 'magit-diff-visit-file-other-window)

  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch))
