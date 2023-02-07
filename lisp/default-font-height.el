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
  (let* ((frame (if (null frame) (window-frame) frame))
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

(defun default-font-height-adjust (&optional arg frame)
  "Change or save the default font height of the frame FRAME.

If FRAME is omitted or nil, use currently selected frame.

With no prefix argument, increase the font height of the
currently selected frame.

With a negative prefix argument, decrease the font height of the
currently selected frame.

With a zero as prefix argument, reset the default font height to
the last saved value for the monitor that the currently selected
frame is on.

With \\[universal-argument] as prefix argument, save the default
font height for the monitor that the currently selected frame is
on."
  (interactive "P")
  (default-font-height-list-initialize)
  (when-let* ((frame (if (null frame) (window-frame) frame))
              (current-font-height (round (/ (face-attribute 'default :height frame) 10.0)))
              (font-height (cond ((null arg)
                                  (1+ current-font-height))
                                 ((eq '- arg)
                                  (1- current-font-height))
                                 ((eq 0 (prefix-numeric-value arg))
                                  (lax-plist-get default-font-height-list
                                                 (default-font-height-get-monitor-id frame)))
                                 ((eq 4 (prefix-numeric-value arg))
                                  (setq default-font-height-list
                                        (lax-plist-put default-font-height-list
                                                       (default-font-height-get-monitor-id frame)
                                                       current-font-height))
                                  (default-font-height-write-file)
                                  current-font-height)
                                 (t (error "error: invalid argument")))))
    (if (eq 4 (prefix-numeric-value arg))
        (message "Saving font height %d for current monitor with frame \"%s\""
                 current-font-height (frame-parameter frame 'name))
      (set-frame-font (number-to-string font-height) t (list frame))
      (message "Setting font height to %d in frame \"%s\""
               font-height (frame-parameter frame 'name)))))

(defun default-font-height-reset-frame (&optional frame)
  "Change the default font height of the frame FRAME to the last
saved value for the monitor that the currently selected frame is
on.

If FRAME is omitted or nil, use currently selected frame."
  (default-font-height-adjust 0 frame))

(add-hook 'window-setup-hook 'default-font-height-reset-frame)
(add-hook 'move-frame-functions 'default-font-height-reset-frame)

;; key bidings

(global-set-key (kbd "C-M-=") 'default-font-height-adjust)

(provide 'default-font-height)
