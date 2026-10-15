;;; flymake-extra.el --- Extra flymake commands for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'flymake)

;;;###autoload
(defun flymake-toggle-show-diagnostics-at-end-of-line ()
  "Cycle `flymake-show-diagnostics-at-end-of-line' through its values."
  (interactive)
  (setopt flymake-show-diagnostics-at-end-of-line
          (pcase flymake-show-diagnostics-at-end-of-line
            ('nil t)
            ('t 'short)
            ('short 'fancy)
            (_ nil)))
  (message "Flymake show diagnostics at end of line: %s"
           flymake-show-diagnostics-at-end-of-line)
  (when flymake-mode
    (if flymake-show-diagnostics-at-end-of-line
        (flymake-start t)
      (mapc #'flymake--delete-overlay (flymake--really-all-overlays)))))

(provide 'flymake-extra)

;;; flymake-extra.el ends here
