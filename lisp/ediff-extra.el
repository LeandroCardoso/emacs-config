;;; ediff-extra.el --- Extra ediff functionality for Emacs -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;; The following functions must be called to enable extra functionality with ediff
;;
;; Setup a new action in ediff mode to copy current difference region from buffers A and B to buffer C:
;; (ediff-extra-setup-copy-AB-to-C)
;;
;; Setup a new action in ediff session to change the current font size temporarily:
;; (ediff-extra-setup-text-scale)
;;
;; Setup to auto restore the window configuration after exiting an ediff session:
;; (ediff-extra-setup-window-configuration)
;;
;; Setup a global keymap to ediff commands in \"C-x M-e\":
;; (ediff-extra-setup-global-keymap)

;;; Code:

(require 'ediff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy Buffers A and B to C ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adapted from https://stackoverflow.com/a/29757750
(defun ediff-copy-AB-to-C (reverse)
  "Copy current difference region from buffers A and B to buffer C.

With parameter REVERSE, copy in reverse order - buffer A past buffer B
to buffer C."
  (interactive "P")
  (ediff-barf-if-not-control-buffer)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents
                     ediff-current-difference
                     (if reverse 'B 'A)
                     ediff-control-buffer)
                    (ediff-get-region-contents
                     ediff-current-difference
                     (if reverse 'A 'B)
                     ediff-control-buffer)))
  (ediff-recenter))

(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map (kbd "d") 'ediff-copy-AB-to-C))

(defun ediff-extra-setup-copy-AB-to-C ()
  "Setup a new action in ediff mode to copy current difference region from
buffers A and B to buffer C."
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))


;;;;;;;;;;;;;;;;
;; Text scale ;;
;;;;;;;;;;;;;;;;

(defun ediff-text-scale-increase (inc)
  "Increase the font size of the current ediff session temporarily.

All ediff buffers font size will be increased by INC steps.  A negative
number of steps decreases the font size by the same amount.  As a
special case, an argument of 0 will remove any scaling currently active.

See `ediff-text-scale-decrease' and `ediff-text-scale-reset'."
  (interactive "p")
  (require 'face-remap)
  (ediff-barf-if-not-control-buffer)
  (dolist (buf `(,ediff-buffer-A ,ediff-buffer-B ,ediff-buffer-C ,ediff-ancestor-buffer))
    (when buf
      (with-current-buffer buf
        (text-scale-increase inc)))))

(defun ediff-text-scale-decrease (dec)
    "Decrease the font size of the current ediff session temporarily.

All ediff buffers font size will be decreased by DEC steps.  A negative
number of steps increases the font size by the same amount.  As a
special case, an argument of 0 will remove any scaling currently active.

See `ediff-text-scale-increase' and `ediff-text-scale-reset'."
  (interactive "p")
  (ediff-text-scale-increase (- dec)))

(defun ediff-text-scale-reset ()
  "Reset the font size of the current ediff session.

See `ediff-text-scale-increase' and`ediff-text-scale-decrease'."
  (interactive)
  (ediff-text-scale-increase 0))

(defun add-text-scale-to-ediff-mode-map ()
  (define-key ediff-mode-map (kbd "C-+") 'ediff-text-scale-increase)
  (define-key ediff-mode-map (kbd "C--") 'ediff-text-scale-decrease)
  (define-key ediff-mode-map (kbd "C-0") 'ediff-text-scale-reset))

(defun ediff-extra-setup-text-scale ()
  "Setup a new action in ediff session to change the current font size temporarily."
  (add-hook 'ediff-keymap-setup-hook 'add-text-scale-to-ediff-mode-map)
  (add-hook 'ediff-cleanup-hook 'ediff-text-scale-reset)
  (add-hook 'ediff-suspend-hook 'ediff-text-scale-reset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restore window configuration on quit ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ediff-window-configuration nil "Window configuration before ediff setup.")

(defun ediff-save-window-configuration ()
  (setq ediff-window-configuration (current-window-configuration)))

(defun ediff-restore-window-configuration ()
  (set-window-configuration ediff-window-configuration))

(defun ediff-extra-setup-window-configuration ()
  "Setup to auto restore the window configuration after exiting an ediff session."
  (add-hook 'ediff-before-setup-hook #'ediff-save-window-configuration)
  (add-hook 'ediff-after-quit-hook-internal #'ediff-restore-window-configuration 50)
  (add-hook 'ediff-suspend-hook #'ediff-restore-window-configuration 50))


;;;;;;;;;;;;;;;;;;;
;; Global keymap ;;
;;;;;;;;;;;;;;;;;;;

(defvar ediff-keymap nil "Keymap for ediff commands.")
(defvar ediff-merge-keymap nil "Keymap for ediff merge commands.")
(defvar ediff-patch-keymap nil "Keymap for ediff patch commands.")

(setq ediff-merge-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map "b" 'ediff-merge-buffers)
        (define-key map "B" 'ediff-merge-buffers-with-ancestor)
        (define-key map "d" 'ediff-merge-directories)
        (define-key map "D" 'ediff-merge-directories-with-ancestor)
        (define-key map "t" 'ediff-merge-directory-revisions)
        (define-key map "T" 'ediff-merge-directory-revisions-with-ancestor)
        (define-key map "f" 'ediff-merge-files)
        (define-key map "F" 'ediff-merge-files-with-ancestor)
        (define-key map "v" 'ediff-merge-revisions)
        (define-key map "V" 'ediff-merge-revisions-with-ancestor)
        map))
(defalias 'ediff-merge-keymap ediff-merge-keymap)

(setq ediff-patch-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map "b" 'ediff-patch-buffer)
        (define-key map "f" 'ediff-patch-file)
        map))
(defalias 'ediff-patch-keymap ediff-patch-keymap)

(setq ediff-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map "b" 'ediff-buffers)
        (define-key map "B" 'ediff-buffers3)
        (define-key map "c" 'ediff-current-file)
        (define-key map "d" 'ediff-directories)
        (define-key map "D" 'ediff-directories3)
        (define-key map "f" 'ediff-files)
        (define-key map "F" 'ediff-files3)
        (define-key map "h" 'ediff-documentation)
        (define-key map "k" 'ediff-backup)
        (define-key map (kbd "<return>") 'ediff-show-registry)
        (define-key map "l" 'ediff-regions-linewise)
        (define-key map "w" 'ediff-regions-wordwise)
        (define-key map "v" 'ediff-revision)
        (define-key map "t" 'ediff-directory-revisions)
        (define-key map "m" 'ediff-merge-keymap)
        (define-key map "p" 'ediff-patch-keymap)
        map))
(defalias 'ediff-keymap ediff-keymap)

(defun ediff-extra-setup-global-keymap ()
  "Setup a global keymap to ediff commands in \\[ediff-keymap]."
  (define-key ctl-x-map (kbd "M-e") 'ediff-keymap))


(provide 'ediff-extra)

;;; ediff-extra.el ends here
