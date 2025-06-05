;;; files-extra.el --- Extra files functions for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'simple)

(defun cleanup-compiled-elisp (directory &optional follow-symlinks)
  "Clean up compiled elisp files in DIRECTORY.

Delete old '.elc' files that don't have a corresponding '.el' associated
file.  Files in subdirectories of DIRECTORY are processed also.

This command will normally not follow symlinks when deleting files.  If
FOLLOW-SYMLINKS is non-nil, symlinked directories will also be followed."
  (let* ((directories (nconc (list directory) (list-directories directory t t follow-symlinks)))
         (delete-count 0)
         (skip-count 0)
         (dir-count 0)
         last-dir)
    (while directories
      (setq directory (expand-file-name (car directories)))
      (message "Checking %s..." directory)
      (dolist (f (directory-files directory t ".*\\.elc$"))
        (unless noninteractive
          (message "Checking %s..." directory))
        (let ((file (expand-file-name f directory)))
          (unless (file-exists-p (file-name-with-extension file ".el"))
            (message "Deleting file: %s" file)
            (delete-file file))
          (if (file-exists-p file)
              (setq skip-count (1+ skip-count))
            (setq delete-count (1+ delete-count)))
          (if (not (eq last-dir directory))
              (setq last-dir directory
                    dir-count (1+ dir-count)))))
      (setq directories (cdr directories)))
    (message "Done (Total of %d file%s deleted%s%s)"
	         delete-count (if (= delete-count 1) "" "s")
	         (if (> skip-count 0) (format ", %d skipped" skip-count) "")
	         (if (> dir-count 1)
                 (format " in %d directories" dir-count) ""))))

(defun byte-recompile-and-cleanup-directory (directory &optional force follow-symlinks)
  "Recompile and clean up eslip files in DIRECTORY.

Recompile every ‘.el’ file in DIRECTORY that needs recompilation.  This
happens when a '.elc' file doesn't exist, or it exists but is older than
the '.el' file.  Files in subdirectories of DIRECTORY are processed
also.

After recompilation, delete old '.elc' files that don't have a
corresponding '.el' associated file.

If the argument FORCE is non-nil, recompile every '.el'.

This command will normally not follow symlinks.  If FOLLOW-SYMLINKS is
non-nil, symlinked directories will also be followed."
  (interactive "DRecompile and clean up eslip files in directory: \nP")
  (require 'bytecomp)
  ;; Compile all elisp files
  (message "Compiling elisp files in %s..." directory)
  (byte-recompile-directory directory 0 force follow-symlinks)

  ;; Delete old elisp compiled files (.elc) that doesn't have a eslisp source file (.el) associated
  (message "Cleaning up elisp compiled files in %s..." directory)
  (cleanup-compiled-elisp directory follow-symlinks))

(defun list-directories-fast (directory &optional follow-symlinks)
  "Return a list of subdirectories in DIRECTORY.

This function works recursively and it uses the external `find-program'
to list subdirectories.

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to directories
are followed.  Note that this can lead to infinite recursion."
  (require 'grep)
  (when (file-directory-p directory)
    (let ((program-args (list (expand-file-name directory) "-type" "d")))
      (when follow-symlinks
        (push "-L" program-args))
      (apply 'process-lines find-program program-args))))

(defun list-directories (directory &optional full recursive follow-symlinks)
  "Return a list of subdirectories in DIRECTORY.

The list returned is sorted with ‘string-lessp’.

If FULL is non-nil, return absolute file names.  Otherwise return names
that are relative to the specified directory.

If RECURSIVE is non-nil, this function works recursively.  Directories
are returned in \"depth first\" order with absolute file names
regardless of the value of FULL.

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to directories
are followed.  Note that this can lead to infinite recursion."
  (let ((dir (expand-file-name directory))
        result)
    (dolist (f (directory-files dir (or full recursive)))
      (when (and (file-directory-p (expand-file-name f dir))
                 (not (member (file-name-nondirectory f) '("." ".."))))
        (setq result (nconc result (list f)))
        (when (and recursive
                   (or follow-symlinks
		               (not (file-symlink-p f))))
          (setq result (nconc result (list-directories f full recursive follow-symlinks))))))
    result))

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
