;;; files-extra.el --- Extra files functions for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'simple)

;;;###autoload
(defun cleanup-user-compiled-lisp ()
  "Clean up compiled lisp files in `user-lisp-directory'.

Delete old '.elc' files that don't have a corresponding '.el' associated
file."
  (interactive)
  (message "Cleaning up compiled user lisp files")
  (unless (file-directory-p user-lisp-directory)
    (error "No such directory: %S" user-lisp-directory))
  (let* ((ignored
          (concat "\\`" (regexp-opt user-lisp-ignored-directories) "\\'"))
         (pred
          (lambda (dir)
            (not (string-match-p ignored (file-name-nondirectory dir)))))
         (dir (expand-file-name user-lisp-directory)))
    (dolist (file (directory-files-recursively dir "" t pred t))
      (when (and (file-regular-p file)
                 (string-suffix-p ".elc" file)
                 (not (file-regular-p (file-name-with-extension file ".el"))))
        (message "Deleting file: %s" file)
        (delete-file file)))))

;;;###autoload
(defun copy-file-or-buffer-name-as-kill (&optional arg)
  "If the current buffer is a file visited buffer, copy the file
name of the current buffer to the kill ring. With parameter ARG,
copy the full file name path of the current buffer.

If the current buffer is not a file visited buffer, copy the
current buffer name to the kill ring."
  (interactive "P")
  (let ((name (if buffer-file-name
                  (if arg
                      (file-local-name buffer-file-name)
                    (file-name-nondirectory buffer-file-name))
                (buffer-name))))
  (kill-new name)
  (message "%s name %s" (if buffer-file-name "File" "Buffer") name)))

;;;###autoload
(defun copy-file-or-buffer-name-directory-as-kill (&optional arg)
  "If the current buffer is a file visited buffer, copy the
directory component of the current file to the kill ring.

If the current buffer is not a file visited buffer, copy the
current default directory of the current buffer to the kill ring.

With parameter ARG, convert the directory to absolute, and
canonicalize it."
  (interactive "P")
  (let* ((dir (if buffer-file-name
                  (file-name-directory (file-local-name buffer-file-name))
                default-directory))
         (dir-exp (if arg
                      (expand-file-name dir)
                    dir)))
    (kill-new dir-exp)
    (message "Directory %s" dir-exp)))

;;;###autoload
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

(defun directory-parent (directory &optional number)
  "Return the parent directory of DIRECTORY.
With NUMBER, return the NUMBER parent directory of DIRECTORY."
  (when directory
    (if (or (null number) (= number 1) (= number 0))
        (file-name-directory (directory-file-name directory))
      (directory-parent (file-name-directory (directory-file-name directory)) (1- number)))))

;;;###autoload
(defun sync-directories (source destination &optional ignore-timestamp)
  "Copy all files from SOURCE directory to DESTINATION directory
that exists in both directories and are newer in source.

With optional argument IGNORE-TIMESTAMP, ignore the timestamp and
copy all files that exist in both directories."
  (interactive
   (let ((dir (read-directory-name "Copy directory: " default-directory default-directory t)))
     (list dir (read-directory-name
                (format "Copy directory %s to: " dir) default-directory default-directory t))))
  (unless (file-directory-p source)
    (error "source: %s is not a directory" source))
  (unless (file-directory-p destination)
    (error "destination: %s is not a directory" destination))
  (if (equal (file-name-as-directory source) (file-name-as-directory destination))
      (error "source: %s and directory: %s are the same" source destination))
  (let ((files-copied 0))
    (message "Copying from %s to %s" source destination)
    (dolist (source-file (directory-files source t))
      (let ((destination-file (concat (file-name-as-directory destination)
                                      (file-name-nondirectory source-file))))
        (when (and (file-regular-p source-file)
                   (file-exists-p destination-file)
                   (or ignore-timestamp
                       (file-newer-than-file-p source-file destination-file)))
          (message "Copying %s" (file-name-nondirectory source-file))
          (copy-file source-file (file-name-as-directory destination) t)
          (setq files-copied (1+ files-copied)))))
    (message "%d files copied from %s to %s." files-copied source destination)))

(defun locate-dominating-file-match (file match)
  "Starting at FILE, look up directory hierarchy for file names that
match the regexp MATCH. FILE can be a file or a directory. If it's a
file, its directory will serve as the starting point for searching the
hierarchy of directories. Stop at the first parent directory containing a
file name that match the regexp MATCH, and return a list of file names.
Return nil if not found.

If FULL is non-nil, return absolute file names. Otherwise return names
 that are relative to the specified directory.

If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 Otherwise, the list returned is sorted with ‘string-lessp’. NOSORT is
 useful if you plan to sort the result yourself."
  ;; This function was inspired by the `locate-dominating-file'.
  ;;
  ;; Represent /home/luser/foo as ~/foo so that we don't try to look for
  ;; `match' in /home or in /.
  (let ((directory (file-name-directory (abbreviate-file-name (expand-file-name file))))
        (try nil))
    (while (not (or try
                    (null directory)
                    (not (file-directory-p directory))
                    (string-match-p locate-dominating-stop-dir-regexp directory)))
      (setq try (directory-files directory t match t))
      (unless try
        ;; if current directory is equal to root directory, then set it to nil and exit the loop
        (if (string= directory
                     (setq directory (file-name-directory (directory-file-name directory))))
            (setq directory nil))))
    try))

(provide 'files-extra)

;;; files-extra.el ends here
