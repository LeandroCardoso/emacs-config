(defun directory-list (path)
  "Find all directories in PATH."
  (when (file-directory-p path)
    (process-lines find-program (expand-file-name path) "-type" "d")))

(defun copy-buffer-name-as-kill (&optional arg)
  "If the current buffer is a file visited buffer, show the full
 path of the file and without parameter ARG copy it to the kill
 ring, with paramater ARG copy just the file name to the kill
 ring.

If the current buffer is not a file visited buffer, show the
current default directory and wit parameter ARG copy it to the
kill ring, without parameter ARG copy the buffer name to the kill
ring."
  (interactive "P")
  (kill-new (if buffer-file-name
                (if arg
                    buffer-file-name
                  (file-name-nondirectory buffer-file-name))
              (if arg
                  default-directory
                (buffer-name))))
  (if buffer-file-name
      (message "File name %s" buffer-file-name)
    (message "Directory %s" default-directory)))

(defun rename-buffer-and-file ()
  "Rename the current buffer and the file name when it is
visiting a file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (let ((new-name (read-string "Rename buffer: " (buffer-name))))
          (rename-buffer new-name))
      (let ((new-name (read-file-name "Rename file: " nil nil nil (file-name-nondirectory filename))))
        (if (vc-backend filename)
            (vc-rename-file filename new-name)
          (rename-file filename new-name 1)
          (set-visited-file-name new-name t t))))))

(defun make-backup-buffer ()
  "Make a backup of the disk file visited by the current buffer.
See `backup-buffer'."
  (interactive)
  (if (not (buffer-file-name))
      (message "Buffer %s is not visiting a file" (buffer-name))
    (let ((make-backup-files t)
          (backup-inhibited nil)
          (buffer-backed-up nil))
      (backup-buffer)
      (when buffer-backed-up
        (message "Created backup for buffer %s" (file-name-nondirectory buffer-file-name))))))

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
(setq confirm-kill-processes nil)

;; key bindings
(global-set-key (kbd "C-c C-r") 'rename-buffer-and-file)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c ~") 'make-backup-buffer)
(global-set-key (kbd "C-x C-d") 'copy-buffer-name-as-kill) ; replace list-directory
