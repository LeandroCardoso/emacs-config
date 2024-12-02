(defun create-dir-local-file (directory)
  "Create a `dir-locals-file' in the DIRECTORY if it does not exist yet."
  (let ((full-dir-locals-file (concat directory dir-locals-file)))
    (when (not (file-exists-p full-dir-locals-file))
      ;;(with-temp-file full-dir-locals-file)))
      (write-region "" nil full-dir-locals-file))))


(defun directory-parent (directory &optional number)
  "Return the parent directory of DIRECTORY.
With NUMBER, return the NUMBER parent directory of DIRECTORY."
  (when directory
    (if (or (null number) (= number 1) (= number 0))
        (file-name-directory (directory-file-name directory))
      (directory-parent (file-name-directory (directory-file-name directory)) (1- number)))))

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

(defun copy-last-message ()
  "Copy the last non nil message in \"*Messages*\" buffer to the kill ring."
  (interactive)
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char (point-max))
      (while (and (string-empty-p (buffer-substring-no-properties (line-beginning-position)
                                                                  (line-end-position)))
                  (= 0 (forward-line -1)))) ; end case if buffer is empty
      (kill-new (buffer-substring (line-beginning-position) (line-end-position))))))


(defun standard-value (symbol)
  "Return SYMBOL's standard value.
This is the global default value."
  (eval (car (get symbol 'standard-value))))
