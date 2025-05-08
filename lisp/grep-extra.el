;;; grep-extra.el --- Extra grep functions for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'grep)

(defun grep-kill-ring-advice (regexp &rest r)
  "Save the latest grep REGEXP into the `kill-ring'.

This function is intended to be used as an advice.  Parameter R exists
for compatibility."
  (require 'simple)
  (kill-new regexp))

(defun grep-kill-ring-setup ()
  "Enable saving the latest grep regexp into the `kill-ring'."
  (require 'nadvice)
  (advice-add 'lgrep :after 'grep-kill-ring-advice)
  (advice-add 'rgrep :after 'grep-kill-ring-advice)
  (advice-add 'zrgrep :after 'grep-kill-ring-advice))

(provide 'grep-extra)

;;; grep-extra.el ends here
