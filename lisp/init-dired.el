(require 'dired)

(setq dired-dwim-target t)

(defun dired-move-to-filename-i ()
  "Move to the beginning of the filename on the current line.
Return the position of the beginning of the filename, or nil if none found.
This is the interactive version of `dired-move-to-filename'"
  (interactive)
  (dired-move-to-filename))

(global-set-key (kbd "C-x M-d") 'find-name-dired)

(define-key dired-mode-map (kbd "b") 'browse-url-of-dired-file)
(define-key dired-mode-map (kbd "C-=") 'dired-compare-directories)
(define-key dired-mode-map (kbd "M-m") 'dired-move-to-filename-i)
(define-key dired-mode-map (kbd "<tab>") 'dired-next-line)
(define-key dired-mode-map (kbd "<backtab>") 'dired-previous-line)


(with-eval-after-load "wdired"
  (define-key wdired-mode-map (kbd "M-m") 'dired-move-to-filename-i))
