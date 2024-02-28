(require 'recentf)

(setq recentf-max-saved-items 100)
(recentf-mode)

(defun recentf-find-file ()
  "Edit file from the `recentf-list'.

This function works better with a `completing-read' enhancement like `ido'."
  (interactive)
  (find-file (completing-read "Find recent file: "
                              (mapcar #'abbreviate-file-name recentf-list) nil t)))

(defun recentf-find-file-other-window ()
  "Edit file from the `recentf-list', in another window.

This function works better with a `completing-read' enhancement like `ido'."
  (interactive)
  (find-file-other-window (completing-read "Find recent file: "
                                           (mapcar #'abbreviate-file-name recentf-list) nil t)))

(defun recentf-find-file-other-frame ()
  "Edit file from the `recentf-list', in another frame.

This function works better with a `completing-read' enhancement like `ido'."
  (interactive)
  (find-file-other-frame (completing-read "Find recent file: "
                                          (mapcar #'abbreviate-file-name recentf-list) nil t)))

(define-key ctl-x-map (kbd "C-r") 'recentf-find-file)                ; original is find-file-read-only
(define-key ctl-x-4-map (kbd "C-r") 'recentf-find-file-other-window) ; original is find-file-read-only-other-window
(define-key ctl-x-5-map (kbd "C-r") 'recentf-find-file-other-frame)  ; original is find-file-read-only-other-frame
