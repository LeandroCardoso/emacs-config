(global-hl-line-mode)

;; global-hl-line-mode causes slowness when scrolling down repeatedly, this is a good workaround for
;; it.
(setq auto-window-vscroll nil)

;; Hack to disable global-hl-line-mode in ediff registry buffers
(add-hook 'ediff-mode-hook '(lambda () (setq-local global-hl-line-mode nil)))
