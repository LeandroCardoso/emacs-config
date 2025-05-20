;;; dynamic-font-size.el --- Set the font size automatically -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'faces)
(require 'files)
(require 'frame)
(require 'map)

;;; user options

(defgroup dynamic-font-size nil
  "Dynamic font size."
  :group 'faces)

(defcustom dynamic-font-size-list-file
  (expand-file-name "dynamic-font-size" user-emacs-directory)
  "Save file for default font size.

File in which to save the list of default fonts size for different
monitors."
  :type 'file
  :group 'dynamic-font-size)

;;; variables

(defvar dynamic-font-size-list 'unset
  "An association list of default fonts size for different monitors.

This list maps a monitor idenfification returned by
`dynamic-font-size-get-monitor-id' to a default font size for this
monitor.")

;;; functions

(defun dynamic-font-size-get-monitor-id (&optional frame)
  "Get a monitor identification.

Return a monitor identification for the monitor that frame FRAME is on
to be used as a key in the `dynamic-font-size-list'.

If FRAME is omitted or nil, use currently selected frame."
  (let* ((frame (if (null frame) (selected-frame) frame))
         (monitor-size (frame-monitor-attribute 'mm-size frame))
         (monitor-geometry (frame-monitor-attribute 'geometry frame))
         (monitor-resolution (list (nth 2 monitor-geometry) (nth 3 monitor-geometry))))
    (list monitor-size monitor-resolution)))

(defun dynamic-font-size-list-initialize ()
  "Initialize `dynamic-font-size-list'.

Initialize `dynamic-font-size-list' if it isn't already initialized."
  (when (eq dynamic-font-size-list 'unset)
    (dynamic-font-size-read-file)))

(defun dynamic-font-size-read-file ()
  "Initialize `dynamic-font-size-list' from file.

Initialize `dynamic-font-size-list' using contents of
`dynamic-font-size-list-file'."
  (setq dynamic-font-size-list
        (when (file-exists-p dynamic-font-size-list-file)
          (with-temp-buffer
            (insert-file-contents dynamic-font-size-list-file)
            (read (current-buffer))))))

(defun dynamic-font-size-write-file ()
  "Save `dynamic-font-size-list' to file.

Save `dynamic-font-size-list' contents into
`dynamic-font-size-list-file'."
  (with-temp-buffer
    (insert ";;; -*- lisp-data -*-\n")
    (let ((print-length nil)
          (print-level nil))
      (pp dynamic-font-size-list (current-buffer)))
    (write-region nil nil dynamic-font-size-list-file nil 'silent)))

(defun dynamic-font-size-get-font-size (frame)
  "Return the current size of the font in frame FRAME."
  (round (/ (face-attribute 'default :height frame) 10.0)))

(defun dynamic-font-size-set-font-size (frame font-size)
  "Set the size of the default font.

Set the size of the default font of the frame FRAME to FONT-SIZE."
  (set-face-attribute 'default frame :height (* 10 font-size))
  (set-frame-parameter frame
                       'dynamic-font-size-monitor-id
                       (dynamic-font-size-get-monitor-id frame)))

;;;###autoload
(defun dynamic-font-size-save (&optional frame)
  "Save the font size.

Save the size of the default font of the frame FRAME for the monitor
that the currently selected frame is on.

If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (dynamic-font-size-list-initialize)
  (let* ((frame (if (null frame) (selected-frame) frame))
         (frame-name (if (eq frame (selected-frame))
                         "current frame"
                       (format "frame %s" (frame-parameter frame 'name))))
         (monitor-id (dynamic-font-size-get-monitor-id frame))
         (current-font-size (dynamic-font-size-get-font-size frame)))
    (if dynamic-font-size-list
        (map-put! dynamic-font-size-list monitor-id current-font-size)
      (setq dynamic-font-size-list (list (cons monitor-id current-font-size))))
    (dynamic-font-size-write-file)
    (message "Saving font size to %d for monitor with %s" current-font-size frame-name)))

;;;###autoload
(defun dynamic-font-size-adjust (&optional arg)
  "Adjust the font size.

Change or save the size of the font of the currently selected frame.

With no argument ARG, temporarily increase the size of the font of the
currently selected frame.

With a negative argument ARG, temporarily decrease the size of the font
 of the currently selected frame.

With a zero as argument ARG, reset the size of the font to the last
saved value for the monitor that the currently selected frame is on.

With \\[universal-argument] as argument ARG, save the font size for the
monitor that the currently selected frame is on as the default font size
for frames on this monitor."
  (interactive "P")
  (cond ((and arg (listp arg))
         (dynamic-font-size-save))
        ((eq 0 arg)
         (dynamic-font-size-reset))
        (t (dynamic-font-size-increase (prefix-numeric-value arg)))))

;;;###autoload
(defun dynamic-font-size-increase (inc &optional frame)
  "Increase the font size.

Increase the size of the font of the frame FRAME by INC steps.

If INC is omitted or nil, increase the size of the font by 1.

If FRAME is omitted or nil, use currently selected frame."
  (interactive "p")
  (let* ((frame (if (null frame) (selected-frame) frame))
         (current-font-size (dynamic-font-size-get-font-size frame))
         (new-font-size (+ (or inc 1) current-font-size)))
    (unless (eq current-font-size new-font-size)
      (dynamic-font-size-set-font-size frame new-font-size)
      (message
       (substitute-command-keys "Setting font size temporarily to %d
Use `\\[dynamic-font-size-adjust]' with zero as prefix to reset the font size.
Use `\\[dynamic-font-size-adjust]' with `\\[universal-argument]' as prefix to save the font size.")
       new-font-size))))

;;;###autoload
(defun dynamic-font-size-reset (&optional frame)
  "Reset the font size.

Change the size of the font of the frame FRAME to the last saved value
for the monitor that the currently selected frame is on.

If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (dynamic-font-size-list-initialize)
  (let* ((frame (if (null frame) (selected-frame) frame))
         (frame-name (if (eq frame (selected-frame))
                         "current frame"
                       (format "frame %s" (frame-parameter frame 'name))))
         (current-font-size (dynamic-font-size-get-font-size frame))
         (new-font-size (map-elt dynamic-font-size-list (dynamic-font-size-get-monitor-id frame))))
    (if (not new-font-size)
        (message "No default font size saved for monitor with %s" frame-name)
      (if (eq current-font-size new-font-size)
          (message "Font size is %d in %s" current-font-size frame-name)
        (dynamic-font-size-set-font-size frame new-font-size)
        (message "Resetting font size to %d in %s" new-font-size frame-name)))))

(defun dynamic-font-size-reset-when-monitor-change (&optional frame)
  "Reset the font size when a frame move to a different monitor.

When the frame FRAME moves to a different monitor, change the size of
the font to the last saved value for the monitor that the currently
selected frame is on.

If FRAME is omitted or nil, use currently selected frame."
  (unless (equal (dynamic-font-size-get-monitor-id frame)
                 (frame-parameter frame 'dynamic-font-size-monitor-id))
    (dynamic-font-size-reset frame)))

;;;###autoload
(define-minor-mode dynamic-font-size-mode
  "Toggle Dynamic Font Size Mode.

When Dynamic Font Size Mode is enabled, the font size for existing and
new frames is set to the value saved for the monitor where the frame is
on, or it was created.  It also change the font size for a frame when it
moves to a different monitor."
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
