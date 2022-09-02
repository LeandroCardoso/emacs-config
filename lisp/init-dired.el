(with-eval-after-load "dired"
  (require 'dired-x)

  (setq dired-auto-revert-buffer t)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-dwim-target t)
  (setq dired-isearch-filenames 'dwim)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-maybe-use-globstar t)

  (defun dired-do-backup (&optional arg)
    "Make a backup of the marked (or next ARG) files."
    (interactive)
    (mapc (lambda (file-name)
            (let ((backupname (car (find-backup-file-name file-name)))
                  (modes (file-modes file-name))
                  (extended-attributes (file-extended-attributes file-name)))
              (backup-buffer-copy file-name backupname modes extended-attributes)))
          (dired-get-marked-files nil arg))
    (revert-buffer))

  (defun dired-do-ediff (&optional arg)
    "Compare files with `ediff' or `ediff3'.

When there are two or three files marked, compare them without
prompt.

When there is not file marked, prompt for a file and compare it
with file at point. When there is no file at point, prompt for
two files and compare them.

If the file at point has a backup file, use that as the default
prompt file. If the file at point is a backup file, use its
original, if that exists and can be found. Note that
customizations of `backup-directory-alist' and
`make-backup-file-name-function' change where this function
searches for the backup file, and affect its ability to find the
original of a backup file."
    (interactive)
    (let* ((files (dired-get-marked-files nil arg))
           (num-files (length files)))
      (cond ((= num-files 0)
             (call-interactively 'ediff-files))
            ((= num-files 1)
             (let* ((file-A (dired-get-filename))
                    ;; Get the latest existing backup file or its original if that exists
                    (file-backup (if (backup-file-name-p file-A)
                                     (let ((f (file-name-sans-versions file-A)))
                                       (when (file-readable-p f)
                                         f))
                                   (diff-latest-backup-file file-A))))
               (ediff-files file-A
                            (read-file-name (format-prompt "Diff %s with"
                                                           (when file-backup
                                                             (file-name-nondirectory file-backup))
                                                           (file-name-nondirectory file-A))
                                            (file-name-directory file-A)
                                            (file-name-nondirectory (or file-backup file-A))
                                            t))))
            ((= num-files 2)
             (ediff-files (nth 0 files) (nth 1 files)))
            ((= num-files 3)
             (ediff-files3 (nth 0 files) (nth 1 files) (nth 2 files)))
            (t (error "Invalid number of files marked. Mark two or three files.")))))

  (defun dired-eww-open-file ()
    "In Dired, render the file on this line using EWW"
    (interactive)
    (eww-open-file (dired-get-file-for-visit)))

  (defun dired-position-at-filename ()
    "Move to the beginning of the filename on the current line.
Return the position of the beginning of the filename, or nil if none found."
    (interactive)
    (dired-move-to-filename))

  ;; local keys
  (define-key dired-mode-map (kbd "<tab>") 'dired-next-line)
  (define-key dired-mode-map (kbd "<backtab>") 'dired-previous-line)
  (define-key dired-mode-map (kbd "M-<return>") 'dired-up-directory)
  (define-key dired-mode-map (kbd "C-=") 'dired-compare-directories)
  (define-key dired-mode-map (kbd "M-m") 'dired-position-at-filename)
  (define-key dired-mode-map (kbd "K") 'dired-do-backup)
  (define-key dired-mode-map (kbd "E") 'dired-eww-open-file)
  (define-key dired-mode-map (kbd "C-+") 'dired-create-empty-file)
  (define-key dired-mode-map (kbd "M-=") 'dired-do-ediff)

  (with-eval-after-load "wdired"
    (define-key wdired-mode-map (kbd "M-m") 'dired-position-at-filename)))

;; global keys
(global-set-key (kbd "C-x M-d") 'find-name-dired)
