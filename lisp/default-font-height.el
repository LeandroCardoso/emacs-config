(require 'faces)
(require 'files)
(require 'frame)
(require 'seq)

;; user options

(defcustom default-font-height-list-file (locate-user-emacs-file "default-font-height")
  "File in which to save the list of default fonts height for
different monitors."
  :type 'file
  :group 'default-font-height)

;; variables

(defvar default-font-height-list 'unset
  "A property list of default fonts height for different
monitors.

The property list maps a monitor idenfification returned by
`default-font-height-get-monitor-id' to a default font height for
this monitor.")

;; functions

(defun default-font-height-get-monitor-id (&optional frame)
  "Return a monitor identification for the monitor that frame FRAME
is on to be used as a key in the `default-font-height-list'.

If FRAME is omitted or nil, use currently selected frame."
  (let* ((frame (if (null frame) (selected-frame) frame))
         (monitor-size (frame-monitor-attribute 'mm-size frame))
         (monitor-geometry (frame-monitor-attribute 'geometry frame))
         (monitor-resolution (list (nth 2 monitor-geometry) (nth 3 monitor-geometry))))
    (list monitor-size monitor-resolution)))

(defun default-font-height-list-initialize ()
  "Initialize `default-font-height-list' if it isn't already
initialized."
  (when (eq default-font-height-list 'unset)
    (default-font-height-read-file)))

(defun default-font-height-read-file ()
  "Initialize `default-font-height-list' using contents of
`default-font-height-list-file'."
  (setq default-font-height-list
        (when (file-exists-p default-font-height-list-file)
          (with-temp-buffer
            (insert-file-contents default-font-height-list-file)
            (read (current-buffer))))))

(defun default-font-height-write-file ()
  "Save `default-font-height-list' contents into
`default-font-height-list-file'."
  (with-temp-buffer
    (insert ";;; -*- lisp-data -*-\n")
    (let ((print-length nil)
          (print-level nil))
      (pp default-font-height-list (current-buffer)))
    (write-region nil nil default-font-height-list-file nil 'silent)))

(defun default-font-height-adjust (&optional arg)
  "Change or save the height of the default font of the frame
 FRAME.

If FRAME is omitted or nil, use currently selected frame.

With no prefix argument, temporarily increase the height of the
default font of the currently selected frame.

With a negative prefix argument, temporarily decrease the height
of the default font of the currently selected frame.

With a zero as prefix argument, reset the height of the default
font to the last saved value for the monitor that the currently
selected frame is on.

With \\[universal-argument] as prefix argument, save the default
font height for the monitor that the currently selected frame is
on.

The height of the default font of the frame is automatically
reset when the frame is moved."
  (interactive "P")
  (cond ((and arg (listp arg))
         (default-font-height-save))
        ((eq 0 arg)
         (default-font-height-reset))
        (t (default-font-height-increase (prefix-numeric-value arg)))))

(defun default-font-height-increase (inc &optional frame)
  "Increase the height of the default font of the frame FRAME by
INC steps.

If INC is omitted or nil, increase the height of the default font
by 1.

If FRAME is omitted or nil, use currently selected frame."
  (interactive "p")
  (let* ((frame (if (null frame) (selected-frame) frame))
         (current-font-height (round (/ (face-attribute 'default :height frame) 10.0)))
         (new-font-height (+ (or inc 1) current-font-height)))
    (unless (eq current-font-height new-font-height)
      (set-face-attribute 'default frame :height (* 10 new-font-height))
      (message
       (substitute-command-keys "Setting font height temporarily to %d
Use `\\[default-font-height-adjust]' with zero as prefix to reset the font height. Use `\\[universal-argument]' as prefix to save the font height.")
       new-font-height))))

(defun default-font-height-reset (&optional frame)
  "Change the height of the default font of the frame FRAME to the
last saved value for the monitor that the currently selected
frame is on.

If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (default-font-height-list-initialize)
  (let* ((frame (if (null frame) (selected-frame) frame))
         (current-font-height (round (/ (face-attribute 'default :height frame) 10.0)))
         (new-font-height (lax-plist-get default-font-height-list
                                         (default-font-height-get-monitor-id frame))))
    (unless (eq current-font-height new-font-height)
      (set-face-attribute 'default frame :height (* 10 new-font-height))
      (message "Resetting font height to %d" new-font-height))))

(defun default-font-height-save (&optional frame)
  "Save the height of the default font of the frame FRAME for the
monitor that the currently selected frame is on.

If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (default-font-height-list-initialize)
  (let* ((frame (if (null frame) (selected-frame) frame))
         (current-font-height (round (/ (face-attribute 'default :height frame) 10.0))))
    (setq default-font-height-list
          (lax-plist-put default-font-height-list
                         (default-font-height-get-monitor-id frame)
                         current-font-height))
    (default-font-height-write-file)
    (message "Saving font height to %d for current monitor with frame \"%s\""
             current-font-height (frame-parameter frame 'name))))

;; hooks

(add-hook 'window-setup-hook 'default-font-height-reset)
(add-hook 'move-frame-functions 'default-font-height-reset)

;; key bidings

(global-set-key (kbd "C-M-=") 'default-font-height-adjust)

(provide 'default-font-height)
