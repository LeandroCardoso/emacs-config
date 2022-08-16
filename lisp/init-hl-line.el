(dolist (mode '(archive-mode-hook
                dired-mode-hook
                grep-mode-hook
                ibuffer-mode-hook
                occur-mode-hook
                proced-mode-hook
                tabulated-list-mode-hook
                tar-mode-hook))
  (add-hook mode #'hl-line-mode))

;; hl-line-mode causes slowness when scrolling down repeatedly, this is a good workaround for it
(setq auto-window-vscroll nil)
