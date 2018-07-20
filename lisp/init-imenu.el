(when (require 'imenu nil t)
  (setq imenu-auto-rescan t)
  (setq imenu-auto-rescan-maxout 100000)
  (setq imenu-max-item-length nil)

  (add-hook 'imenu-after-jump-hook 'reposition-window)

  (defun imenu-rescan ()
    (interactive)
    (imenu--menubar-select imenu--rescan-item))

  (add-hook 'after-save-hook 'imenu-rescan)
  (add-hook 'find-file-hook 'imenu-rescan)

  (global-set-key (kbd "C-z") 'imenu) ; default is suspend-frame
  )

(when (require 'imenu-anywhere nil t)
  ;; (setq imenu-anywhere-buffer-filter-functions nil)
  (global-set-key (kbd "C-z") 'ido-imenu-anywhere))
