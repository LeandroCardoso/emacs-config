(with-eval-after-load "dired"
  (setq dired-auto-revert-buffer t)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-dwim-target t)
  (setq dired-isearch-filenames 'dwim)

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
    "Run `ediff', or `ediff3' with the marked files."
    (interactive)
    (let ((files (dired-get-marked-files nil arg)))
      (cond ((= (length files) 2)
             (ediff-files (nth 0 files) (nth 1 files)))
            ((= (length files) 3)
             (ediff-files3 (nth 0 files) (nth 1 files) (nth 2 files)))
            (t (error "Invalid number of files marked. Ediff only accept two or three files.")))))

  (defun dired-eww-open-file ()
    "In Dired, render the file on this line using EWW"
    (interactive)
    (eww-open-file (dired-get-file-for-visit)))

  (make-interactive dired-move-to-filename)

  ;; local keys
  (define-key dired-mode-map (kbd "<tab>") 'dired-next-line)
  (define-key dired-mode-map (kbd "<backtab>") 'dired-previous-line)
  (define-key dired-mode-map (kbd "M-<return>") 'dired-up-directory)
  (define-key dired-mode-map (kbd "C-=") 'dired-compare-directories)
  (define-key dired-mode-map (kbd "M-m") 'dired-move-to-filename)
  (define-key dired-mode-map (kbd "K") 'dired-do-backup)
  (define-key dired-mode-map (kbd "E") 'dired-eww-open-file)
  (define-key dired-mode-map (kbd "C-+") 'dired-create-empty-file)
  (define-key dired-mode-map (kbd "M-=") 'dired-do-ediff)

  (with-eval-after-load "wdired"
    (define-key wdired-mode-map (kbd "M-m") 'dired-move-to-filename)))

;; global keys
(global-set-key (kbd "C-x M-d") 'find-name-dired)

(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)

(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)

(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)
