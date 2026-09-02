;;; markdown-mode-extra.el --- Extra markdown-mode commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'markdown-mode)
(require 'browse-url)

;;;###autoload
(defun markdown-guide (&optional arg)
  "Display the Markdown guide documentation.

With optional parameter ARG, display in an external browser, instead of eww."
  (interactive "P")
  (require 'eww)
  (require 'browse-url)
  (let ((url "https://www.markdownguide.org/basic-syntax/"))
    (if arg
        (browse-url-default-browser url)
      (eww url t))))

(provide 'markdown-mode-extra)

;;; markdown-mode-extra.el ends here
