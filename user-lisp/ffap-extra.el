;;; ffap-extra.el --- ffap extra functions for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'ffap)
(require 'nadvice)
(require 'pcase)

(defun ffap-menu-ask-remove-rescan-advice (args)
  "Remove the \"*Rescan buffer*\" entry from the `ffap-menu' command."
  (pcase-let ((`(,title ,alist ,cont) args))
    (list title
          (if (string= (caar alist) "*Rescan Buffer*")
              (cdr alist)
            alist)
          cont)))

(advice-add 'ffap-menu-ask :filter-args 'ffap-menu-ask-remove-rescan-advice)

(provide 'ffap-extra)

;;; ffap-extra.el ends here
