;;; recentf-extra.el --- Extra recentf commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'recentf)

;;;###autoload
(defun recentf-find-file ()
  "Edit file from the `recentf-list'."
  (interactive)
  (find-file (completing-read "Find recent file: "
                              (mapcar #'abbreviate-file-name recentf-list) nil t)))
;;;###autoload
(defun recentf-find-file-other-window ()
  "Edit file from the `recentf-list', in another window."
  (interactive)
  (find-file-other-window (completing-read "Find recent file: "
                                           (mapcar #'abbreviate-file-name recentf-list) nil t)))
;;;###autoload
(defun recentf-find-file-other-frame ()
  "Edit file from the `recentf-list', in another frame."
  (interactive)
  (find-file-other-frame (completing-read "Find recent file: "
                                          (mapcar #'abbreviate-file-name recentf-list) nil t)))


(provide 'recentf-extra)

;;; init-recentf.el ends here
