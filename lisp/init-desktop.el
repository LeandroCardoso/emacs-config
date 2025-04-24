(use-package desktop
  :config
  (defun desktop-save-mode-on ()
    "Enable `desktop-save-mode'.  Provided for use in hooks."
    (desktop-save-mode 1))

  (setopt desktop-save 'ask-if-exists)
  (add-to-list 'desktop-locals-to-save 'buffer-display-time)

  :hook
  (desktop-after-read . clean-buffer-list)
  ;; I am enabling the desktop-save-mode *after* a desktop session is loaded by the `desktop-read'
  ;; command to avoid loading a desktop session on Emacs initialization
  (desktop-after-read . desktop-save-mode-on))
