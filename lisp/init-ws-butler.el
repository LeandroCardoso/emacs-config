(when (require 'ws-butler nil t)
  (setq ws-butler-keep-whitespace-before-point nil)

  (defun ws-butler-enable-ediff-buffer-C ()
    (when (and ediff-buffer-C
               ws-butler-global-mode
               (not (apply (function derived-mode-p) ws-butler-global-exempt-modes)))
      (with-current-buffer ediff-buffer-C
        (ws-butler-mode))))

  (defun ws-butler-disable-ediff-buffer-C ()
    (when ediff-buffer-C
      (with-current-buffer ediff-buffer-C
        (ws-butler-mode -1))))

  (add-hook 'ediff-startup-hook 'ws-butler-disable-ediff-buffer-C)
  (add-hook 'ediff-cleanup-hook 'ws-butler-enable-ediff-buffer-C)
  (add-hook 'ediff-suspend-hook 'ws-butler-enable-ediff-buffer-C)

  (ws-butler-global-mode))
