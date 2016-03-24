(setq ediff-custom-diff-options "-c -w")
(setq ediff-diff-options "--binary -w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; everything in one frame

;; I don't know why I need such a complex code to set the keymap with ediff-mult, but this works.
(with-eval-after-load "ediff-mult"
  (add-hook 'ediff-meta-buffer-keymap-setup-hook
            (lambda ()
              (define-key ediff-meta-buffer-map (kbd "<tab>") 'ediff-next-meta-item)
              (define-key ediff-meta-buffer-map (kbd "<backtab>") 'ediff-previous-meta-item))))
