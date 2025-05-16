;;; default-font-height.el --- Set the font height of frames by monitor -*- lexical-binding:t -*-

;;; Copyright: Leandro Cardoso

;;; Maintainer: Leandro Cardoso - leandrocardoso@gmail.com

;;; Commentary:

;;; Code:

(require 'faces)
(require 'files)
(require 'frame)
(require 'seq)

;; TODO rename to auto-font-size or dynamic-font-size.
;;
;; TODO create a mode to enable/disable
;;
;; TODO change default-font-height-list to alist

;;; user options

(defcustom default-font-height-list-file (expand-file-name "default-font-height" user-emacs-directory)
  "Save file for default font heights.

File in which to save the list of default fonts height for different
monitors."
  :type 'file
  :group 'default-font-height)

;;; variables

(defvar default-font-height-list 'unset
  "A property list of default fonts height for different monitors.

The property list maps a monitor idenfification returned by
`default-font-height-get-monitor-id' to a default font height for this
monitor.")

;;; functions

(defun default-font-height-get-monitor-id (&optional frame)
  "Get a monitor identification.

Return a monitor identification for the monitor that frame FRAME is on
to be used as a key in the `default-font-height-list'.

If FRAME is omitted or nil, use currently selected frame."
  (let* ((frame (if (null frame) (selected-frame) frame))
         (monitor-size (frame-monitor-attribute 'mm-size frame))
         (monitor-geometry (frame-monitor-attribute 'geometry frame))
         (monitor-resolution (list (nth 2 monitor-geometry) (nth 3 monitor-geometry))))
    (list monitor-size monitor-resolution)))

(defun default-font-height-list-initialize ()
  "Initialize `default-font-height-list'.

Initialize `default-font-height-list' if it isn't already initialized."
  (when (eq default-font-height-list 'unset)
    (default-font-height-read-file)))

(defun default-font-height-read-file ()
  "Initialize `default-font-height-list' from file.

Initialize `default-font-height-list' using contents of
`default-font-height-list-file'."
  (setq default-font-height-list
        (when (file-exists-p default-font-height-list-file)
          (with-temp-buffer
            (insert-file-contents default-font-height-list-file)
            (read (current-buffer))))))

(defun default-font-height-write-file ()
  "Save `default-font-height-list' to file.

Save `default-font-height-list' contents into
`default-font-height-list-file'."
  (with-temp-buffer
    (insert ";;; -*- lisp-data -*-\n")
    (let ((print-length nil)
          (print-level nil))
      (pp default-font-height-list (current-buffer)))
    (write-region nil nil default-font-height-list-file nil 'silent)))

(defun default-font-height-set-font-height (frame font-height)
  "Set the height of the default font.

Set the height of the default font of the frame FRAME to FONT-HEIGHT."
  (set-face-attribute 'default frame :height (* 10 font-height))
  (set-frame-parameter frame
                       'default-font-height-monitor-id
                       (default-font-height-get-monitor-id frame)))

;;;###autoload
(defun default-font-height-adjust (&optional arg)
  "Adjust the font height.

Change or save the height of the default font of the currently selected
frame.

With no argument ARG, temporarily increase the height of the default
font of the currently selected frame.

With a negative argument ARG, temporarily decrease the height of the
default font of the currently selected frame.

With a zero as argument ARG, reset the height of the default font to the
last saved value for the monitor that the currently selected frame is
on.

With \\[universal-argument] as argument ARG, save the default font
height for the monitor that the currently selected frame is on.

The height of the default font of the frame is automatically reset when
the frame is moved."
  (interactive "P")
  (cond ((and arg (listp arg))
         (default-font-height-save))
        ((eq 0 arg)
         (default-font-height-reset))
        (t (default-font-height-increase (prefix-numeric-value arg)))))

;;;###autoload
(defun default-font-height-increase (inc &optional frame)
  "Increase the font height.

Increase the height of the default font of the frame FRAME by INC steps.

If INC is omitted or nil, increase the height of the default font by 1.

If FRAME is omitted or nil, use currently selected frame."
  (interactive "p")
  (let* ((frame (if (null frame) (selected-frame) frame))
         (current-font-height (round (/ (face-attribute 'default :height frame) 10.0)))
         (new-font-height (+ (or inc 1) current-font-height)))
    (unless (eq current-font-height new-font-height)
      (default-font-height-set-font-height frame new-font-height)
      (message
       (substitute-command-keys "Setting font height temporarily to %d
Use `\\[default-font-height-adjust]' with zero as prefix to reset the font height. Use `\\[universal-argument]' as prefix to save the font height.")
       new-font-height))))

;;;###autoload
(defun default-font-height-reset (&optional frame)
  "Reset the font height.

Change the height of the default font of the frame FRAME to the last
saved value for the monitor that the currently selected frame is on.

If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (default-font-height-list-initialize)
  (let* ((frame (if (null frame) (selected-frame) frame))
         (current-font-height (round (/ (face-attribute 'default :height frame) 10.0)))
         (new-font-height (plist-get default-font-height-list
                                     (default-font-height-get-monitor-id frame)
                                     'equal)))
    (if (not new-font-height)
        (message "No default font height saved for current monitor"))
      (if (eq current-font-height new-font-height)
          (message "Current font height is %d" current-font-height))
        (default-font-height-set-font-height frame new-font-height)
        (message "Resetting font height to %d" new-font-height)))

;;;###autoload
(defun default-font-height-save (&optional frame)
  "Save the font height.

Save the height of the default font of the frame FRAME for the monitor
that the currently selected frame is on.

If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (default-font-height-list-initialize)
  (let* ((frame (if (null frame) (selected-frame) frame))
         (current-font-height (round (/ (face-attribute 'default :height frame) 10.0))))
    (setq default-font-height-list
          (plist-put default-font-height-list
                     (default-font-height-get-monitor-id frame)
                     current-font-height
                     'equal))
    (default-font-height-write-file)
    (message "Saving font height to %d for current monitor with frame \"%s\""
             current-font-height (frame-parameter frame 'name))))

(defun default-font-height-reset-when-monitor-change (&optional frame)
  "Reset the font height when frame change monitor.

When the frame FRAME change monitor, change the height of the default
font to the last saved value for the monitor that the currently selected
frame is on.

If FRAME is omitted or nil, use currently selected frame."
  (unless (equal (default-font-height-get-monitor-id frame)
                 (frame-parameter frame 'default-font-height-monitor-id))
    (default-font-height-reset frame)))

;;;###autoload
(defun default-font-height-setup ()
  "Setup to automatically set the default font height."
  ;; Set font height for the initial frame
  (add-hook 'emacs-startup-hook 'default-font-height-reset)
  ;; Set font height for new frames
  (add-hook 'after-make-frame-functions 'default-font-height-reset)
  ;; Set font height when a frame moves to a different monitor
  (add-hook 'move-frame-functions 'default-font-height-reset-when-monitor-change))

(provide 'default-font-height)

;;; default-font-height.el ends here
