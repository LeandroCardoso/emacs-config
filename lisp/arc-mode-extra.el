;;; arc-mode-extra.el --- Extra arc-mode commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'arc-mode)

(defun archive-move-to-filename ()
  "Move to the beginning of the filename on the current line.

Return the position of the beginning of the filename, or nil if none
found."
  (interactive)
  (forward-char archive-file-name-indent))

(provide 'arc-mode-extra)

;;; arc-mode-extra.el ends here
