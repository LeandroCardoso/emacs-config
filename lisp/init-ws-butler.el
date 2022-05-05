(when (require 'ws-butler nil t)
  (setq ws-butler-keep-whitespace-before-point nil)

  (defun ws-butler-mode-enable ()
    (when (and ws-butler-global-mode
               (not (apply (function derived-mode-p) ws-butler-global-exempt-modes)))
      (ws-butler-mode)))

  (defun ws-butler-mode-disable ()
    (ws-butler-mode -1))

  (defun ws-butler-global-mode-disable ()
    (ws-butler-global-mode -1))

  (add-hook 'ediff-startup-hook 'ws-butler-mode-disable)
  (add-hook 'ediff-cleanup-hook 'ws-butler-mode-enable)

  (ws-butler-global-mode))
