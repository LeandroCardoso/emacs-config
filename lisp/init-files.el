(defun directory-list (path)
  "Find all directories in PATH."
  (when (file-directory-p path)
    (process-lines find-program path "-type" "d")))

(defun copy-filename-as-kill (&optional full)
  "If the current buffer is a file visited buffer, save the
current file name in the kill ring.

Otherwise save the current directory in the kill ring.

With prefix argument FULL when current buffer is a file visied
buffer, save only the file name without the directory in the kill
ring."
  (interactive "P")
  (kill-new (if buffer-file-name
                (if full
                    buffer-file-name
                  (file-name-nondirectory buffer-file-name))
              default-directory))
  (if buffer-file-name
      (message "File name %s" buffer-file-name)
    (message "Directory %s" default-directory)))

;; Adapted from https://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" (buffer-name))
      (let ((new-name (read-file-name "New name: " nil nil nil (file-name-nondirectory filename))))
        (if (vc-backend filename)
            (vc-rename-file filename new-name)
          (rename-file filename new-name 1)
          (set-visited-file-name new-name t t))))))

(defun make-backup-buffer ()
  "Make a backup of the disk file visited by the current buffer.
See `backup-buffer'."
  (interactive)
  (let ((make-backup-files t)
        (backup-inhibited nil)
        (buffer-backed-up nil))
    (backup-buffer)
    (when buffer-backed-up
      (message "created backup for buffer %s" (file-name-nondirectory buffer-file-name)))))

;; auto-save
(unless (file-directory-p (concat user-emacs-directory "auto-save"))
  (mkdir (concat user-emacs-directory "auto-save")))
(setq auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/\\1") t)))
;; backup
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq make-backup-files nil)
(setq version-control t)
;; misc
(setq confirm-kill-emacs 'y-or-n-p)

;; key bindings
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x ~") 'make-backup-buffer)
(global-set-key (kbd "C-x C-d") 'copy-filename-as-kill) ; replace list-directory
