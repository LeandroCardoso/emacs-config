(require 'recentf)

(setq recentf-max-saved-items 200)
(recentf-mode)

;; Periodically saving the list of files, just in case of an emacs crash
(run-with-idle-timer 90 t 'recentf-save-list)

(defun recentf-find-file ()
  "Edit file from the `recentf-list'.

This function works better with a `completing-read' enhancement like `ido'."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t)))

(defun recentf-find-file-other-window ()
  "Edit file from the `recentf-list', in another window.

This function works better with a `completing-read' enhancement like `ido'."
  (interactive)
  (find-file-other-window (completing-read "Find recent file: " recentf-list nil t)))

(defun recentf-find-file-other-frame ()
  "Edit file from the `recentf-list', in another frame.

This function works better with a `completing-read' enhancement like `ido'."
  (interactive)
  (find-file-other-frame (completing-read "Find recent file: " recentf-list nil t)))

(global-set-key (kbd "C-x C-r") 'recentf-find-file) ; default is find-file-read-only

;; other window
(global-set-key (kbd "C-x 4 C-r") 'recentf-find-file-other-window) ; default is find-file-read-only-other-window
(global-set-key (kbd "C-x 4 r") 'recentf-find-file-other-window) ; default is find-file-read-only-other-window

;; other frame
(global-set-key (kbd "C-x 5 C-r") 'recentf-find-file-other-frame) ; default is find-file-read-only-other-frame
(global-set-key (kbd "C-x 5 r") 'recentf-find-file-other-frame) ; default is find-file-read-only-other-frame
