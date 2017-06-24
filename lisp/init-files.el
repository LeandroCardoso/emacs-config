(unless (file-directory-p (concat user-emacs-directory "auto-save"))
  (mkdir (concat user-emacs-directory "auto-save")))

(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/\\1") t)))
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,(concat user-emacs-directory "backup"))))
(setq confirm-kill-emacs 'y-or-n-p)
(setq delete-old-versions t)

(global-set-key (kbd "C-c r") 'revert-buffer)
