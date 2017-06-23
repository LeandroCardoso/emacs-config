(unless (file-directory-p "~/.emacs.d/auto-save")
  (mkdir "~/.emacs.d/auto-save"))

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/\\1" t)))
(setq backup-by-copying t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq confirm-kill-emacs 'y-or-n-p)
(setq delete-old-versions t)

(global-set-key (kbd "C-c r") 'revert-buffer)
