(setq ediff-custom-diff-options "-c -w")
(setq ediff-diff-options "--binary -w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; everything in one frame

(with-eval-after-load "ediff-mult"
  (define-key ediff-meta-buffer-map (kbd "<tab>") 'ediff-next-meta-item)
  (define-key ediff-meta-buffer-map (kbd "<backtab>") 'ediff-previous-meta-item))
