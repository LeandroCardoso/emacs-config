;;; eshell-extra.el --- Extra eshell commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'eshell)

(defun eshell-other-window (&optional arg)
  "Like `eshell', but creates a new window or reuses an existing one."
  (interactive "P")
  (require 'window)
  (if (one-window-p)
      (split-window)
    (other-window 1))
  (eshell arg))

(provide 'eshell-extra)

;;; eshell-extra.el ends here
