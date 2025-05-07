;;; gtags-mode-extra.el --- Extra gtags-mode commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'gtags-mode)

(defun gtags-mode-project-create ()
  "Create a GLOBAL GTAGS file in the root directory of the current project asynchronously.

When no project is found, follow the same behavior as `project-current'.

See `gtags-mode-create' and `project-root'."
  (interactive)
  (require 'project)
  (gtags-mode-create (project-root (project-current t))))

(provide 'gtags-mode-extra)

;;; gtags-mode-extra.el ends here
