;;; woman-reformat.el --- Reformat WoMan buffers on demand -*- lexical-binding: t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;; This extension provides a simple mechanism to reformat WoMan buffers, useful after adjusting
;; variables such as `woman-fill-column' or when resizing the window while `woman-fill-frame' is
;; non-nil.

;; Usage:
;;   - Call `(woman-reformat-setup)' to install the hook.
;;   - Optionally bind `woman-reformat' to a convenient key.  For example, "R" is often a good
;;     replacement for `woman-reformat-last-file'.

;;; Code:

(require 'nadvice)
(require 'woman)

;;;###autoload
(defvar-mode-local woman-mode woman-buffer-file-name nil
  "Full pathname of the file being displayed by the current WoMan buffer.")

;;;###autoload
(defun woman-save-buffer-file-name (file-name &optional reformat)
  "Store `woman-last-file-name' into the buffer-local `woman-buffer-file-name'.

Only runs when the current buffer is in `woman-mode'."
  (when (eq major-mode 'woman-mode)
    (setq-local woman-buffer-file-name woman-last-file-name)))

;;;###autoload
(defun woman-reformat-setup ()
  "Set up automatic tracking of the underlying file for WoMan buffers.

Installs advice around `woman-find-file' so `woman-buffer-file-name' stays updated."
  (advice-add 'woman-find-file :after #'woman-save-buffer-file-name))

;;;###autoload
(defun woman-reformat ()
  "Reformat the current WoMan buffer.

Useful after changing formatting variables such as `woman-fill-column'
or when resizing the window while `woman-fill-frame' is non-nil."
  (interactive)
  (when (and (eq major-mode 'woman-mode)
             woman-buffer-file-name)
    (woman-find-file woman-buffer-file-name t)))

(provide 'woman-reformat)

;;; woman-reformat.el ends here
