(setq ff-case-fold-search t)

(unless (eq system-type 'windows-nt)
  (defun find-file-sudo ()
    "Reopen the current file as root, preserving point position."
    (interactive)
    (let ((p (point)))
      (find-alternate-file (concat "/sudo::" buffer-file-name))
      (goto-char p))))

(global-set-key (kbd "C-M-o") 'ff-find-other-file) ;; default is split-line
