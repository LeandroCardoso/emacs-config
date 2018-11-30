(unless (file-directory-p (concat user-emacs-directory "auto-save"))
  (mkdir (concat user-emacs-directory "auto-save")))

(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/\\1") t)))
(setq make-backup-files nil)
(setq backup-by-copying t)
(setq version-control t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq delete-old-versions t)

(defun backup-buffer-interactive ()
  "Make a backup of the disk file visited by the current buffer.
See `backup-buffer'."
  (interactive)
  (let ((make-backup-files t)
        (backup-inhibited nil)
        (buffer-backed-up nil))
    (backup-buffer)
    (when buffer-backed-up
      (message "created backup for buffer %s" (file-name-nondirectory buffer-file-name)))))

(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x ~") 'backup-buffer-interactive)
