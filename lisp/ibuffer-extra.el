;;; ibuffer-extra.el --- Extra ibuffer functions for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'ibuffer)

(defun ibuffer-remove-title-underline-advice (format)
  "Remove the underline from title in ibuffer buffers.

This function is intended to be used as an advice.  Parameter FORMAT
exists for compatibility."
  (ibuffer-assert-ibuffer-mode)
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (delete-region (pos-bol) (+ (pos-eol) 1))))

(defun ibuffer-remove-title-underline-setup ()
  "Enable Removing the underline from title in ibuffer buffers."
  (require 'nadvice)
  (advice-add 'ibuffer-update-title-and-summary :after 'ibuffer-remove-title-underline-advice))

(provide 'ibuffer-extra)

;;; ibuffer-extra.el ends here
