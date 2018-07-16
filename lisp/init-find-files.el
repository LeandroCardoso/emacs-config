(setq ff-case-fold-search t)

(unless (eq system-type 'windows-nt)
  (defun find-file-sudo (&optional arg)
    "Reopen the current file as root, preserving point position.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:"
                           (ido-read-file-name "Find file (as root): ")))
      (let ((p (point)))
        (find-alternate-file (concat "/sudo::root@localhost" buffer-file-name))
        (goto-char p)))))

(global-set-key (kbd "C-M-o") 'ff-find-other-file) ;; default is split-line
