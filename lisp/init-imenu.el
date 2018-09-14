(when (require 'imenu nil t)
  (setq imenu-auto-rescan t)
  (setq imenu-auto-rescan-maxout 100000)
  (setq imenu-max-item-length nil)

  (defun imenu-rescan ()
    (interactive)
    (imenu--menubar-select imenu--rescan-item))

  (add-hook 'after-save-hook 'imenu-rescan)
  (add-hook 'find-file-hook 'imenu-rescan)

  (global-set-key (kbd "C-z") 'imenu) ; default is suspend-frame
  )

(when (require 'imenu-anywhere nil t)
  (defun ido-imenu-anywhere-dwim (arg)
    "Call `ido-imenu-anywhere' for all buffers.
If a prefix ARG is specified, call it for just the current buffer."
    (interactive "P")
    (let ((imenu-anywhere-buffer-list-function
           (if arg
               '(lambda () (list (current-buffer)))
             imenu-anywhere-buffer-list-function)))
      (ido-imenu-anywhere)))

  (add-to-list 'imenu-anywhere-friendly-modes '(c-mode c++-mode))

  (global-set-key (kbd "C-z") 'ido-imenu-anywhere-dwim))
