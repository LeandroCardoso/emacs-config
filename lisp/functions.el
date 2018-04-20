(defun ido-push-current-directory ()
  "Put the current directory in the ido working directory list"
  (interactive)
  (push default-directory ido-work-directory-list))


(defun create-dir-local-file (DIRECTORY)
  "Create the `dir-locals-file in the DIRECTORY if it does not exist yet."
  (let ((full-dir-locals-file (concat DIRECTORY dir-locals-file)))
    (when (not (file-exists-p full-dir-locals-file))
      ;;(with-temp-file full-dir-locals-file)))
      (write-region "" nil full-dir-locals-file))))


(defun kill-ring-insert ()
  "TODO"
  (interactive)
  (let ((to_insert (completing-read "Yank: "
                                    (delete-duplicates kill-ring :test #'equal))))
    (when (and to_insert (region-active-p))
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end)))
    (insert to_insert)))


(defun directory-parent (DIR &optional NUMBER)
  "Return the parent directory of `DIR'.
With `NUMBER', return the `NUMBER' parent directory of `DIR'."
  (when DIR
    (if (or (null NUMBER) (= NUMBER 1) (= NUMBER 0))
        (file-name-directory (directory-file-name DIR))
      (directory-parent (file-name-directory (directory-file-name DIR)) (1- NUMBER)))))

(defun copy-directory-if-newer (directory1 directory2)
  "Copy files from `DIRECTORY1' to `DIRECTORY2', but only if the
file already exists and it is older in the latter directory than
in the former."
  (interactive
   (let ((dir (read-directory-name
               "Copy directory: " default-directory default-directory t)))
     (list dir
           (read-directory-name
            (format "Copy directory %s to: " dir)
            default-directory default-directory t))))
  (when (file-directory-p directory2)
    (let ((files-copied 0))
      (dolist (file (directory-files directory1 t))
        (when (and (file-regular-p file)
                   (file-newer-than-file-p
                    file
                    (concat (file-name-as-directory directory2) (file-name-nondirectory file))))
          (message "Copying %s to %s" file directory2)
          (copy-file file directory2 t)
          (setq files-copied (1+ files-copied))))
      (message "%d files copied from %s to %s." files-copied directory1 directory2))))


(defun copy-last-message ()
  "Copy the last non nil message in \"*Messages*\" buffer to the kill ring."
  (interactive)
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char (point-max))
      (while (and (string-empty-p (buffer-substring-no-properties (line-beginning-position)
                                                                  (line-end-position)))
                  (= 0 (forward-line -1)))) ; end case if buffer is empty
      (copy-region-as-kill (line-beginning-position) (line-end-position)))))


(defun standard-value (symbol)
  "Return SYMBOL's standard value.
This is the global default value."
  (eval (car (get symbol 'standard-value))))
