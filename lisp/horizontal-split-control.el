;;; horizontal-split-control.el --- Control horizontal window splits -*- lexical-binding: t; -*-

;; Author: Leandro Cardoso
;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com
;; Version: 0.1
;; Package-Requires: (window)
;; Keywords: windows, convenience

;;; Commentary:

;; This library allows users to enable or disable horizontal window splits (i.e., top-bottom splits)
;; by dynamically modifying `split-height-threshold`.
;;
;; Usage:
;;   - M-x enable-horizontal-split
;;   - M-x disable-horizontal-split
;;   - M-x toggle-horizontal-split
;;
;; When horizontal splits are disabled, Emacs will only split windows vertically (side by side),
;; regardless of window height.

;;; Code:

(require 'window)

(defvar horizontal-split-control--saved-threshold split-height-threshold
  "Internal variable to save the previous `split-height-threshold' value.")

(defun horizontal-split-control--disabled-p ()
  "Check if horizontal split is currently disabled."
  (null split-height-threshold))

;;;###autoload
(defun enable-horizontal-split ()
  "Enable horizontal window splits."
  (interactive)
  (if (horizontal-split-control--disabled-p)
      (progn
        (setopt split-height-threshold horizontal-split-control--saved-threshold)
        (message "Horizontal splits enabled, split-height-threshold=%d" split-height-threshold))
    (message "Horizontal splits are already enabled")))

;;;###autoload
(defun disable-horizontal-split ()
  "Disable horizontal window splits."
  (interactive)
  (if (horizontal-split-control--disabled-p)
      (message "Horizontal splits are already disabled")
    (setq horizontal-split-control--saved-threshold split-height-threshold)
    (setopt split-height-threshold nil)
    (message "Horizontal splits disabled")))

;;;###autoload
(defun toggle-horizontal-split ()
  "Toggle the ability for windows to split horizontally."
  (interactive)
  (if (horizontal-split-control--disabled-p)
      (enable-horizontal-split)
    (disable-horizontal-split)))

(provide 'horizontal-split-control)

;;; horizontal-split-control.el ends here
