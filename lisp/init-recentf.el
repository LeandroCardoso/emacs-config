(require 'recentf)

(setq recentf-max-saved-items 200)
(recentf-mode)

;; Periodically saving the list of files, just in case of an emacs crash
(run-at-time nil 300 'recentf-save-list)

(defun recentf-find-file ()
  "Switch to a buffer visiting file from the recently visited file list from the `recentf-list'.
This function works better with a completion enhancement like `ido-ubiquitous' and `flx-ido-mode' enable."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t)))

(global-set-key (kbd "C-x C-r") 'recentf-find-file) ; default is ido-find-file-read-only
