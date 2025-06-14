;;; imenu-anywhere-extra.el --- Extra imenu-anywere commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'imenu-anywhere)

(defun imenu-anywhere-dwim (multi-buffers)
  "Jump to a place in a buffer chosen using a buffer menu.

If a prefix MULTI-BUFFERS is specified, all possible reachable buffers
are selectable, otherwise only the current buffer is selectable.

See `imenu-anywhere' for details."
  (interactive "P")
  (let ((imenu-anywhere-buffer-list-function (if multi-buffers
                                                 imenu-anywhere-buffer-list-function
                                               '(lambda () (list (current-buffer))))))
      (imenu-anywhere)))

(provide 'imenu-anywhere-extra)

;;; imenu-anywhere-extra.el ends here
