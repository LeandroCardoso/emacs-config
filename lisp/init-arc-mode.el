(with-eval-after-load 'arc-mode

  (defun archive-move-to-filename ()
  "Move to the beginning of the filename on the current line.
Return the position of the beginning of the filename, or nil if none found."
    (interactive)
    (forward-char archive-file-name-indent))

  (define-key archive-mode-map (kbd "M-m") 'archive-move-to-filename)
  (define-key archive-mode-map (kbd "<tab>") 'archive-next-line)
  (define-key archive-mode-map (kbd "<backtab>") 'archive-previous-line)
  )
