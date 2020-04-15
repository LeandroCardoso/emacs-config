(setq ff-case-fold-search t)

(unless (eq system-type 'windows-nt)
  (defun find-file-sudo (&optional arg)
    "Like `find-file', but as root.

With a prefix ARG re-open the current file as root."
    (interactive "P")
    (if (and arg buffer-file-name)
        (let ((p (point)))
          (find-file (concat "/sudo:root@localhost:" buffer-file-name))
          (goto-char p))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file (as root): "))))))

(global-set-key (kbd "C-M-o") 'ff-find-other-file) ;; default is split-line
