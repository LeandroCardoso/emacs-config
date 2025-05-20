;;; dynamic-font-size.el --- Automatically adjust font size per monitor -*- lexical-binding: t -*-

;; Copyright (C) Leandro Cardoso
;; Maintainer: Leandro Cardoso <leandrocardoso@gmail.com>

;;; Commentary:

;; This package automatically adjusts the font size of frames based on the monitor they are
;; displayed on.  It saves user preferences per monitor and applies the corresponding font size when
;; frames are created or moved.

;;; Code:

(require 'faces)
(require 'files)
(require 'frame)
(require 'map)

;;; Customization

(defgroup dynamic-font-size nil
  "Automatically adjust font size based on monitor."
  :group 'faces)

(defcustom dynamic-font-size-data-file
  (expand-file-name "dynamic-font-size-data.el" user-emacs-directory)
  "File where font sizes per monitor are saved."
  :type 'file
  :group 'dynamic-font-size)

;;; Internal Variables

(defvar dynamic-font-size-list 'unset
  "Alist mapping monitor identifiers to default font sizes.

Monitor identifiers are computed by `dynamic-font-size-get-monitor-id'.")

;;; Utility Functions

(defun dynamic-font-size-get-monitor-id (&optional frame)
  "Return an identifier for the monitor displaying FRAME.

This ID is used as a key in `dynamic-font-size-list'.

If FRAME is nil, use the selected frame."
  (let* ((frame (or frame (selected-frame)))
         (size (frame-monitor-attribute 'mm-size frame))
         (geom (frame-monitor-attribute 'geometry frame))
         (res  (list (nth 2 geom) (nth 3 geom))))
    (list size res)))

(defun dynamic-font-size-list-initialize ()
  "Initialize `dynamic-font-size-list' from file if needed."
  (when (eq dynamic-font-size-list 'unset)
    (dynamic-font-size-read-file)))

(defun dynamic-font-size-read-file ()
  "Read `dynamic-font-size-list' from `dynamic-font-size-data-file'."
  (setq dynamic-font-size-list
        (when (file-exists-p dynamic-font-size-data-file)
          (with-temp-buffer
            (insert-file-contents dynamic-font-size-data-file)
            (read (current-buffer))))))

(defun dynamic-font-size-write-file ()
  "Save `dynamic-font-size-list' to `dynamic-font-size-data-file'."
  (with-temp-buffer
    (insert ";;; -*- lisp-data -*-\n")
    (let ((print-length nil)
          (print-level nil))
      (pp dynamic-font-size-list (current-buffer)))
    (write-region nil nil dynamic-font-size-data-file nil 'silent)))

(defun dynamic-font-size-get-font-size (frame)
  "Return current default font size in FRAME."
  (round (/ (face-attribute 'default :height frame) 10.0)))

(defun dynamic-font-size-set-font-size (frame size)
  "Set default font size in FRAME to SIZE."
  (set-face-attribute 'default frame :height (* 10 size))
  (set-frame-parameter frame 'dynamic-font-size-monitor-id
                       (dynamic-font-size-get-monitor-id frame)))

;;; Interactive Commands

;;;###autoload
(defun dynamic-font-size-adjust (&optional arg)
  "Adjust the font size of the current frame.

- With no ARG: increase font size.
- With a negative ARG: decrease font size.
- With ARG = 0: reset to saved size for this monitor.
- With \\[universal-argument]: save current font size as default for this monitor."
  (interactive "P")
  (cond
   ((and arg (listp arg))
    (dynamic-font-size-save))
   ((eq arg 0)
    (dynamic-font-size-reset))
   (t
    (dynamic-font-size-increase (prefix-numeric-value arg)))))

;;;###autoload
(defun dynamic-font-size-save (&optional frame)
  "Save current font size of FRAME as default for its monitor.

If FRAME is nil, use the selected frame."
  (interactive)
  (dynamic-font-size-list-initialize)
  (let* ((frame (or frame (selected-frame)))
         (frame-name (if (eq frame (selected-frame))
                         "current frame"
                       (format "frame %s" (frame-parameter frame 'name))))
         (monitor-id (dynamic-font-size-get-monitor-id frame))
         (current-size (dynamic-font-size-get-font-size frame)))
    (if dynamic-font-size-list
        (map-put! dynamic-font-size-list monitor-id current-size)
      (setq dynamic-font-size-list (list (cons monitor-id current-size))))
    (dynamic-font-size-write-file)
    (message "Saved font size %d for monitor in %s" current-size frame-name)))

;;;###autoload
(defun dynamic-font-size-increase (inc &optional frame)
  "Increase font size of FRAME by INC steps.

If FRAME is nil, use the selected frame."
  (interactive "p")
  (let* ((frame (or frame (selected-frame)))
         (current-size (dynamic-font-size-get-font-size frame))
         (new-size (+ (or inc 1) current-size)))
    (unless (eq current-size new-size)
      (dynamic-font-size-set-font-size frame new-size)
      (message (substitute-command-keys
                "Set font size to %d temporarily. Use `\\[dynamic-font-size-adjust]' with 0 to reset. Use `\\[dynamic-font-size-adjust]' with `\\[universal-argument]' to save.")
               new-size))))

;;;###autoload
(defun dynamic-font-size-reset (&optional frame)
  "Reset font size of FRAME to the saved value for its monitor.

If FRAME is nil, use the selected frame."
  (interactive)
  (dynamic-font-size-list-initialize)
  (let* ((frame (or frame (selected-frame)))
         (frame-name (if (eq frame (selected-frame))
                         "current frame"
                       (format "frame %s" (frame-parameter frame 'name))))
         (monitor-id (dynamic-font-size-get-monitor-id frame))
         (saved-size (map-elt dynamic-font-size-list monitor-id))
         (current-size (dynamic-font-size-get-font-size frame)))
    (if (not saved-size)
        (message "No saved font size for monitor in %s" frame-name)
      (if (eq current-size saved-size)
          (message "Font size already %d in %s" current-size frame-name)
        (dynamic-font-size-set-font-size frame saved-size)
        (message "Reset font size to %d in %s" saved-size frame-name)))))

(defun dynamic-font-size-reset-when-monitor-change (&optional frame)
  "Reset font size of FRAME when moved to a different monitor.

If FRAME is nil, use the selected frame."
  (let ((frame (or frame (selected-frame))))
    (unless (equal (dynamic-font-size-get-monitor-id frame)
                   (frame-parameter frame 'dynamic-font-size-monitor-id))
      (dynamic-font-size-reset frame))))

;;;###autoload
(define-minor-mode dynamic-font-size-mode
  "Toggle Dynamic Font Size mode.

When enabled, font size is adjusted automatically based on the monitor
a frame is displayed on."
  :global t
  :group 'dynamic-font-size
  (if dynamic-font-size-mode
      (progn
        ;; Set font size of all the existing frames
        (dolist (frame (frame-list))
          (dynamic-font-size-reset frame))
        ;; Set font size of new frames
        (add-hook 'after-make-frame-functions 'dynamic-font-size-reset)
        ;; Set font size when a frame moves to a different monitor
        (add-hook 'move-frame-functions 'dynamic-font-size-reset-when-monitor-change))
    ;; Unset font size of new frames
    (remove-hook 'after-make-frame-functions 'dynamic-font-size-reset)
    ;; Unset font size when a frame moves to a different monitor
    (remove-hook 'move-frame-functions 'dynamic-font-size-reset-when-monitor-change)))

(provide 'dynamic-font-size)

;;; dynamic-font-size.el ends here
